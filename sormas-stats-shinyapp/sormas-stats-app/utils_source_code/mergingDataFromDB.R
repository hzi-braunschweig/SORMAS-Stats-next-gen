# This function export the data of disease transmission chain from sormas
mergingDataFromDB = function(sormas_db, fromDate, toDate, uniquePersonPersonContact = TRUE)
{ 
  ## computing default time based on 90 days in the past if  not provided by user
  if(missing(fromDate) | missing(toDate)){
    fromDate = as.character(Sys.Date() - delay) 
    toDate = as.character(Sys.Date()) 
  }
  # connecting to sormas db and reading data needed by sormas-stats
  ## reading contact data ###
  queryContact <- paste0("SELECT id, uuid, caze_id, district_id, region_id, person_id,reportdatetime, lastcontactdate, disease,
                    contactclassification,contactproximity,resultingcase_id, followupstatus, followupuntil, contactstatus,
                    contactcategory, relationtocase
                          FROM public.contact
                          WHERE deleted = FALSE and caze_id IS NOT NULL and reportdatetime between '", fromDate, "' and '", toDate, "' ")
  contact = dbGetQuery(sormas_db,queryContact)
  
  # reading cases
  queryCase <- paste0("SELECT  id AS caze_id, uuid AS uuid_case, disease AS disease_case, reportdate AS reportdate_case, person_id AS person_idcase,
  responsibleregion_id AS region_idcase, responsibledistrict_id AS district_idcase, caseclassification AS caseclassification_case, outcome AS outcome_case, epidnumber,
  symptoms_id AS symptoms_idcase
                          FROM public.cases
                          WHERE deleted = FALSE and reportdate between '", fromDate, "' and '", toDate, "' ")
  case = dbGetQuery(sormas_db,queryCase)
  
  # reading region
  region = dbGetQuery(sormas_db,"SELECT  id AS region_id, name AS region_name
                               from public.region
                               where archived = FALSE
                               ") 
  #loading district
  district = dbGetQuery(sormas_db,"SELECT id AS district_id, name AS district_name
                               from public.district
                               where archived = FALSE
                               ")
  #loading symptom data
  querySymptom <- sprintf("SELECT id, onsetdate
                        FROM public.symptoms
                        WHERE id  in (%s)", paste("'", case$symptoms_idcase, "'",collapse=",") ) # only symtoms linkded to selected cases
  symptoms = dbGetQuery(sormas_db, querySymptom)
  
  # reading event
  queryEvent <- paste0("SELECT id AS event_id, uuid AS uuevent_id, reportdatetime AS reportdatetime_event, eventstatus, disease AS disease_events,
  typeofplace, eventlocation_id, archived, risklevel AS risklevel_event
                          FROM public.events
                          WHERE deleted = FALSE and eventstatus != 'DROPPED' and reportdatetime between '", fromDate, "' and '", toDate, "' ")
  events = dbGetQuery(sormas_db,queryEvent)
  
  ## reading event participants
  queryEventPart <- sprintf("SELECT id AS id_eventparticipant, uuid AS uuid_eventparticipant, event_id, person_id AS person_id_eventparticipant,
  resultingcase_id AS resultingcase_id_eventparticipant
                        FROM public.eventParticipant
                        WHERE deleted = FALSE and event_id  in (%s)", paste("'", events$event_id, "'",collapse=",") ) # only ep linkded to selected events
  eventsParticipant = dbGetQuery(sormas_db, queryEventPart)
  
  ## reading location
  queryLocation <- sprintf("SELECT id, district_id, region_id
                        FROM public.location
                        WHERE id  in (%s)", paste("'", na.omit(events$eventlocation_id), "'",collapse=",") ) # only locations linked to events
  location = dbGetQuery(sormas_db,queryLocation)
  
  ### reading person data  ###
  # only persons linkded to cases, contacts or eps
  queryPerson <- sprintf("SELECT id AS id_person, uuid AS uuid_person, sex 
                        FROM public.person
                        WHERE id  in (%s)", paste("'", base::unique(c(case$person_idcase, contact$person_id, eventsParticipant$person_id_eventparticipant )), "'",collapse=",") ) 
  person = dbGetQuery(sormas_db, queryPerson)
  
  
  # merging cases with CONFIRMED_UNKNOWN_SYMPTOMS and  CONFIRMED_NO_SYMPTOMS as confirmed 
  case$caseclassification_case[case$caseclassification_case == "CONFIRMED_NO_SYMPTOMS" ] = "CONFIRMED"
  case$caseclassification_case[case$caseclassification_case == "CONFIRMED_UNKNOWN_SYMPTOMS" ] = "CONFIRMED"
  
  ## converting all date formats from POSIXct to date
  case$reportdate_case = dateTimeToDate(case$reportdate_case)
  #
  contact$reportdatetime = dateTimeToDate(contact$reportdatetime)
  contact$lastcontactdate = dateTimeToDate(contact$lastcontactdate)
  contact$followupuntil = dateTimeToDate(contact$followupuntil)
  #
  symptoms$onsetdate  = dateTimeToDate(symptoms$onsetdate)
  #
  events$reportdatetime_event = dateTimeToDate(events$reportdatetime_event)
  #
  #merging data  
  ## merging event and event participant
  eventsParticipantEvents = events %>%
    dplyr::mutate(archivedEvent = ifelse(archived == TRUE, "t", "f"), .keep = "unused") %>%  
    dplyr::left_join(., location,  by=c("eventlocation_id" = "id" )) %>% # merging Event and location
    dplyr::rename(region_idEvent = region_id , district_idEvent = district_id ) %>% # renaming: eventLocation
    dplyr::inner_join(., eventsParticipant,  by= "event_id") ## merging event and event participants : eventsParticipantEvents
  
  ## Merging case and contact
  contCase = dplyr::inner_join(contact, case,  by = "caze_id") 
  
  # cases without having contacts will not show up here.
  # giving contacts with missing region id  the id of the region of the case   
  temp1 = contCase[is.na(contCase$region_id) == T,] # cotacts with missiing region_id
  temp2 = contCase[is.na(contCase$region_id) == F,]
  temp1$region_id = temp1$region_idcase
  temp1$district_id = temp1$district_idcase
  contCase = rbind(temp1, temp2)
  
  # merging contCase with region to get the names of the regions
  contRegionDist = dplyr::left_join(contCase, region, by = "region_id" ) %>% #  Merging contact and region:  contRegion
    dplyr::left_join(. , district, by = "district_id" )  # contRegionDist
  
  # deleting dupliccate contacts for the same disease between the same persons in contRegionDist
  if (uniquePersonPersonContact == TRUE){
    contRegionDist = dplyr::distinct(contRegionDist, person_idcase, person_id, disease, .keep_all = T) 
  }
  #   
  #choosing final set of variables for contRegionDist to be exported and deleting edges linking a node to itselt
  # contRegionDist = contRegionDist[contRegionDist$person_idcase != contRegionDist$person_id,]
  contRegionDist = contRegionDist %>%
    dplyr::select(id, person_id, person_idcase, disease, contactproximity, lastcontactdate, reportdatetime, contactclassification,
                  followupstatus,followupuntil, resultingcase_id, caseclassification_case, contactstatus,
                  region_name,district_name, outcome_case,caze_id, relationtocase, uuid_case, uuid, reportdate_case) %>%
    dplyr::filter(person_idcase != person_id ) 
  
  ## defining contact categories based on proximity
  # Would later add a configuration for this
  contRegionDist$label = NA
  contRegionDist$label[contRegionDist$contactproximity %in% c("FACE_TO_FACE_LONG","TOUCHED_FLUID","MEDICAL_UNSAVE","CLOTHES_OR_OTHER","PHYSICAL_CONTACT" )] = 1 
  contRegionDist$label[!(contRegionDist$contactproximity %in% c("FACE_TO_FACE_LONG","TOUCHED_FLUID","MEDICAL_UNSAVE","CLOTHES_OR_OTHER","PHYSICAL_CONTACT" ))] = 2
  
  #defining attributes of elist from contRegionDist
  elist =  contRegionDist %>%
    dplyr::rename(from = person_idcase, to = person_id) %>%
    dplyr::mutate(uuid_label = substr(uuid,1,6), smooth = TRUE, dashes = ifelse(label == 2,TRUE, FALSE), arrows = "to", uuid_case= substr(uuid_case,1,6),
                  eventstatus = NA,  entityType = "Case", archivedEvent = NA, risklevel_event = NA, .keep = "all") %>%
    dplyr::left_join(., person, by=c("from" = "id_person" )) %>%
    dplyr::mutate(from_uuid_person = substr(uuid_person,1,6), sex_from_person = sex , .keep = "unused") %>%
    dplyr::left_join(., person, by=c("to" = "id_person" )) %>%
    dplyr::mutate(to_uuid_person = substr(uuid_person,1,6), sex_to_person = sex, .keep = "unused") 
  
  # defining node data
  #get person id from resulting case id
  contConvCase = case[case$caze_id %in% elist$resultingcase_id,] ## cases resulted from contacts
  
  idPersonCaseCont = base::unique(as.character(c(elist$to, elist$from, contConvCase$person_idcase))) # uniqur persons in either case or contact table
  personUnique = person[person$id_person %in% idPersonCaseCont,] # uniqur personts in network diagram of elist
  
  Classification = rep("HEALTHY",nrow(personUnique)) # classification if person
  personId = personUnique$id
  # selzcting cases that belongs to cntact table or cntacts converted to cases
  idCaseUnique = base::unique(na.omit(c(elist$caze_id, elist$resultingcase_id)))
  caseUnique = case[case$caze_id %in% idCaseUnique, ]
  
  casPersonId = caseUnique$person_idcase # using caseUnique table, person id that belong to the set of cases in network
  personClass = as.character(caseUnique$caseclassification_case)
  
  for( i in 1:length(Classification))
  {
    for (j in 1:nrow(caseUnique))
    {
      if(personId[i] == casPersonId[j])
      {
        Classification[i] = personClass[j]
      }
    }
  }
  nodeLineList = data.frame(personUnique, Classification)
  
  # defining node attributes
  nodeLineList = nodeLineList %>%
    dplyr::mutate(group = Classification,  uuid_person = substr(uuid_person,1,6), label = substr(uuid_person,1,6),
                  value=1, shape = c("icon"), code = c("f007"), Classification = Classification,.keep = "unused") 
  
  # Merging eventsParticipantEvents with case table to get region and district of the resulting casses
  #eventsParticipantEventsCases =  base::merge(eventsParticipantEvents, case, by.x = "resultingcase_idevent" , by.y = "caze_id" , all.x = T, all.y = F ) # 
  eventsParticipantEventsCases =  dplyr::left_join(eventsParticipantEvents, case, by = c("resultingcase_id_eventparticipant" = "caze_id"))
  
  # selecting unique contact between cases and event
  if (uniquePersonPersonContact == TRUE){
    eventsParticipantEventsCases = dplyr::distinct(eventsParticipantEventsCases, event_id, person_id_eventparticipant, disease_events, .keep_all = T) 
  }
  # merging eventsParticipantEventsCases with region and district
  elistEvent = dplyr::left_join(eventsParticipantEventsCases, region, by= c("region_idEvent" = "region_id")) %>% # merging with region:  eventsParticipantEventsCasesReg
    dplyr::left_join(. , district, by = c("district_idEvent" = "district_id")) %>%  # Merging with district :  eventsParticipantEventsCasesRegDist
    dplyr::mutate(from = event_id, to = person_id_eventparticipant, id = id_eventparticipant, resultingcase_id = resultingcase_id_eventparticipant,
                  reportdatetime = reportdatetime_event, disease = disease_events, caze_id = event_id, relationtocase = NA, .keep = "all")  %>% # Adding relationtocase and contactcategory:eventsParticipantEventsCasesRegDist
    dplyr::mutate(., uuid_label = substr(uuid_eventparticipant,1,6), from_uuid_person = substr(uuevent_id,1,6) , label = 1, smooth = TRUE, dashes = FALSE, arrows = "to",
                  entityType = "Event", .keep = "unused" ) %>%  # definint elist based on event and event participant
    dplyr::left_join(., person, by=c("to" = "id_person" )) %>%
    dplyr::mutate(to_uuid_person = substr(uuid_person,1,6), .keep = "unused" )
  
  #defining node properties for elistEvent nodes
  eventNode = data.frame(id = elistEvent$event_id,  Classification = c("EVENT"), group =c("EVENT"),label = elistEvent$from_uuid_person, code = c("f0c0") )
  
  caseNode =    elistEvent %>%
    dplyr::mutate(id = person_id_eventparticipant,  Classification = caseclassification_case, 
                  group = ifelse(is.na(caseclassification_case) ==T, "HEALTHY", caseclassification_case),
                  label = substr(to_uuid_person,1,6), code = c("f007"),  .keep = "none")
  
  nodeListCaseEvent = base::rbind(eventNode, caseNode)
  nodeListCaseEvent = nodeListCaseEvent %>%
    dplyr::mutate(value = 1,shape = c("icon"), sex = NA)
  
  ## stacking nodelist and elist from events and contacts
  # stacking nodelinst
  nodeLineList = nodeLineList %>%
    dplyr::mutate(id = id_person, .keep = "unused" ) %>%
    dplyr::select(-c(uuid_person))
  
  nodeListCaseEvent = nodeListCaseEvent[,colnames(nodeLineList)]  # order columns as in nodelist
  nodeListCaseEvent = base::rbind(nodeListCaseEvent, nodeLineList)
  
  # deleting duplicate nodes at random, this is not the optimum method to do,
  nodeListCaseEvent = dplyr::distinct(nodeListCaseEvent, label,  .keep_all = T)  %>% 
    dplyr::mutate(., title = label, uuid_node = label) # labelling nodes
  
  # stacking elist
  # ordering  and combining elist and elistEvent  
  elist =  elist %>%
    dplyr::select(from,	to,	smooth,	dashes,	arrows,	label,	id,	caze_id,	resultingcase_id,	region_name,	district_name,	reportdatetime,
                  disease,	caseclassification_case,	relationtocase,	eventstatus,	entityType, uuid_label, from_uuid_person,archivedEvent, risklevel_event, to_uuid_person) 
  
  elistEvent  = elistEvent %>%
    dplyr::select(event_id,	to,	smooth,	dashes,	arrows,	label,	id,	caze_id,	resultingcase_id,	region_name,	district_name,	reportdatetime,
                  disease,	caseclassification_case,	relationtocase,	eventstatus,	entityType, uuid_label, from_uuid_person, archivedEvent, risklevel_event, to_uuid_person) %>%
    dplyr::mutate(from =event_id,  .keep = "unused" )
  
  # selecting few columns from elist to match those in elistEvent
  elist = elist[ , colnames(elistEvent)]
  #Stacking elistEvent and elist to include event to the elist of contacts
  elistCaseEvent = base::rbind(elist,elistEvent)
  # dropping useless columns form elistCaseEvent that are not needed by the network
  elistCaseEvent = elistCaseEvent  %>%
    dplyr::select(-c(caseclassification_case)) %>%
    # adding an elist id by combinig the uuid of the two nodes involved in the contact
    # This is needed because the id for contacts and event partcipant may be the same since we stacked both tables above
    dplyr::mutate(id_elist = paste(from_uuid_person, to_uuid_person, sep = "-"), .keep = "all")
  
  ############### determining serial interval  ###########
  # selecting unique case id from contats table
  temp = contRegionDist[,colnames(contRegionDist) %in% c("resultingcase_id", "caze_id", "disease", "region_name", "district_name","reportdatetime" )] # these varibales are used to filter commands from ui latter
  selCases = temp[is.na(temp$resultingcase_id) == F,] # edge table with casee id for source cases and resulting cases. We only use data for cases whose contacts became cases
  uniqCaseId = base::unique(c(selCases$caze_id, selCases$resultingcase_id))
  
  #merging uniqCaseId with case table to know the syptom of the cases 
  temp = case[case$caze_id %in% uniqCaseId, c("caze_id", "symptoms_idcase") ]  # cases in involved in contact network
  #merging with syptoms
  caseSymp = base::merge(temp,symptoms, by.x = "symptoms_idcase", by.y = "id", all.x = T, all.y = F)
  caseSymp = caseSymp[, colnames(caseSymp) != "symptoms_idcase"]
  #merging caseSymp with selCases
  selCasesSympCase  = base::merge(selCases, caseSymp, by = "caze_id", all.x = T, all.y = F)
  selCasesSympResultCase = base::merge(selCasesSympCase, caseSymp, by.x = "resultingcase_id", by.y = "caze_id", all.x = T, all.y = F)
  selCasesSympResultCase$si = as.numeric(c(selCasesSympResultCase$onsetdate.y - selCasesSympResultCase$onsetdate.x))
  siDat = selCasesSympResultCase[, colnames(selCasesSympResultCase) %in% c("si","reportdatetime", "disease", "region_name","district_name" )]
  siDat = siDat[is.na(siDat$si) == F,]  # dropping missing values
  
  return(list(contRegionDist = contRegionDist, nodeLineList = nodeListCaseEvent, elist = elistCaseEvent, siDat = siDat))
}