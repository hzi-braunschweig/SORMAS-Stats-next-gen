# loading functions needed

importingData = function(mypath, sep){
  dataList = import.multiple.csv.files(mypath = mypath, mypattern = ".csv$", sep = ";")
  return(dataList)
}
save(importingData, file = "importingData.R")
## 

dateTimeToDate = function(x)
{
  temp1 = substr(x,1,10) # cut first 10 characters in the string
  temp1[temp1==""]=NA  # replace empty substring with NA
  return(as.Date(temp1)) # convert substring to R date 
}
save(dateTimeToDate, file = "dateTimeToDate.R")

###
import.multiple.csv.files<-function(mypath,mypattern,...)
{
  tmp.list.1<-list.files(mypath, pattern=mypattern)
  tmp.list.2<-list(length=length(tmp.list.1))
  for (i in 1:length(tmp.list.1)){tmp.list.2[[i]]<-read.csv(tmp.list.1[i],...)}
  names(tmp.list.2)<-tmp.list.1
  tmp.list.2
}
save(import.multiple.csv.files, file = "import.multiple.csv.files.R")
##
plotNet = function(nodeLineList, elist, IgraphLayout=TRUE)
{
  # IgraphLayout helps to reduce ploting time but the nodes are placed on fixed positions
  defaultFont="font-family:'Open Sans', sans-serif, 'Source Sans Pro'"
  mainStyle = paste(defaultFont, "color: #6591C4", ";font-weight: 600", "font-size: 1.6em", "text-align:center;", sep="; ")
  submainStyle = paste(defaultFont, "text-align:center;", sep="; ")
  footerStyle = defaultFont
  addNodesS <- data.frame(label = c("Healthy","Not_classified" ,"Suspected", "Probable", "Confirmed", "Not case", "1 = High risk", "2 = Low risk", "Event"), shape = "icon",
                          icon.code = c("f007", "f007", "f007", "f007", "f007","f007", "f178", "f178", "f013"),
                          icon.size = c(25, 25, 25, 25, 25,25,25,25,25), icon.color = c("#17bd27", "#706c67", "#ffff00", "#ffa500", "#f70707","#99bd17", "#0d0c0c", "#0d0c0c", "#0000ff"))
  if(IgraphLayout ==FALSE){
    g= visNetwork(nodeLineList, elist,  main = list(text = "Disease network diagram", style = mainStyle),
                  submain = list(text = "The arrows indicate the direction of transmission", style = submainStyle), 
                  # footer = list(text = "Zoom in to see the IDs and contact category", style = footerStyle), 
                  background = "white", annot = T, width = "100%", height = "100vh") %>%  
      visEdges(arrows = "to", color = "black", smooth = FALSE) %>% 
      visOptions(selectedBy = NULL,highlightNearest = TRUE, nodesIdSelection = FALSE) %>% 
      visGroups(groupname = "SUSPECT", size = 10, shape = "icon", icon = list( face ='FontAwesome', code = c( "f007"), color="#ffff00")) %>%
      visGroups(groupname = "PROBABLE", size = 10, shape = "icon", icon = list( face ='FontAwesome', code = c( "f007"), color="#ffa500")) %>%
      visGroups(groupname = "CONFIRMED", size = 10, shape = "icon", icon = list( face ='FontAwesome', code = c( "f007"), color="#f70707")) %>%
      visGroups(groupname = "NOT_CLASSIFIED", size = 10, shape = "icon", icon = list( face ='FontAwesome', code = c( "f007"), color="#706c67" )) %>%
      visGroups(groupname = "HEALTHY", size = 10, shape = "icon", icon = list( face ='FontAwesome', code = c( "f007"), color="#17bd27")) %>%
      visGroups(groupname = "NO_CASE", size = 10, shape = "icon", icon = list( face ='FontAwesome', code = c( "f007"), color="#99bd17")) %>%
      visGroups(groupname = "EVENT", size = 10, shape = "icon", icon = list( face ='FontAwesome', code = c( "f013"), color= "#0000ff")) %>% 
      addFontAwesome() %>%
      visLegend(addNodes = addNodesS, useGroups = F, position = "right", width = 0.1, ncol = 1, stepX = 100, stepY = 100, main = "Legend") %>%  
      visPhysics(stabilization = F) %>%
      visInteraction(dragNodes = T, dragView = T, zoomView = T, hideEdgesOnDrag = T, hideNodesOnDrag=F, hover = T, navigationButtons=T)
    
  } else{
    g= visNetwork(nodeLineList, elist,  main = list(text = "Disease network diagram", style = mainStyle),
                  submain = list(text = "The arrows indicate the direction of transmission", style = submainStyle), 
                  # footer = list(text = "Zoom in to see the IDs and contact category", style = footerStyle), 
                  background = "white", annot = T, width = "100%", height = "100vh") %>%
      visIgraphLayout() %>%  # to improve performance ie reduce plotting time
      visEdges(arrows = "to", color = "black", smooth = FALSE) %>% 
      visOptions(selectedBy = NULL,highlightNearest = TRUE, nodesIdSelection = FALSE) %>% 
      visGroups(groupname = "SUSPECT", size = 10, shape = "icon", icon = list( face ='FontAwesome', code = c( "f007"), color="#ffff00")) %>%
      visGroups(groupname = "PROBABLE", size = 10, shape = "icon", icon = list( face ='FontAwesome', code = c( "f007"), color="#ffa500")) %>%
      visGroups(groupname = "CONFIRMED", size = 10, shape = "icon", icon = list( face ='FontAwesome', code = c( "f007"), color="#f70707")) %>%
      visGroups(groupname = "NOT_CLASSIFIED", size = 10, shape = "icon", icon = list( face ='FontAwesome', code = c( "f007"), color="#706c67" )) %>%
      visGroups(groupname = "HEALTHY", size = 10, shape = "icon", icon = list( face ='FontAwesome', code = c( "f007"), color="#17bd27")) %>%
      visGroups(groupname = "NO_CASE", size = 10, shape = "icon", icon = list( face ='FontAwesome', code = c( "f007"), color="#99bd17")) %>%
      visGroups(groupname = "EVENT", size = 10, shape = "icon", icon = list( face ='FontAwesome', code = c( "f013"), color= "#0000ff")) %>% 
      addFontAwesome() %>%
      visLegend(addNodes = addNodesS, useGroups = F, position = "right", width = 0.1, ncol = 1, stepX = 100, stepY = 100, main = "Legend") %>%  
      visPhysics(stabilization = F) %>%
      visInteraction(dragNodes = T, dragView = T, zoomView = T, hideEdgesOnDrag = T, hideNodesOnDrag=F, hover = T, navigationButtons=T)
  }
  print(g)
}
save(plotNet, file = "plotNet.R")

## network indicators computation 
# computinig vector of person nodeuuid only
compute_person_node_uuid = function(elist)
{
  # takes elist and return a vector of unique person nodeuuid
  person_eventPart_uuid = 
    elist %>%
    dplyr::select(to_uuid_person, entityType ) %>%
    dplyr::filter(entityType == "Event") %>%
    dplyr::distinct_at(. , vars(to_uuid_person)) 
  
  person_uuid = 
    elist %>%
    dplyr::select(from_uuid_person, to_uuid_person, entityType ) %>%
    dplyr::filter(entityType == "Case") %>%
    dplyr::distinct_at(. , vars(from_uuid_person, to_uuid_person))  
  
  paerson_node_uuid = unique( c(person_eventPart_uuid$to_uuid_person, person_uuid$from_uuid_person, person_uuid$to_uuid_person))
  return(paerson_node_uuid)
}
save(compute_person_node_uuid, file = "compute_person_node_uuid.R")
#id_vec = compute_person_node_uuid (elist = elist)

# proportion of contacts converted to case
prop_cont_ep_person_convertedToCase = function(elist)
{
  # number of contact and ep person nodes recorded, ie denominator
  n_contact_ep_nodes = elist %>%
    dplyr::select(to_uuid_person) %>%
    dplyr::distinct_at(. , vars(to_uuid_person)) %>%
    dplyr::summarise(n = n())
  
  #number of contact or ep persons converted to cases
  n_resultingCase_nodes = elist %>%
    dplyr::filter(resultingcase_id != "NA") %>%
    dplyr::select(to_uuid_person) %>%
    dplyr::distinct_at(. , vars(to_uuid_person)) %>%
    dplyr::summarise(n = n())
  
  # proportion of contacts or ep person nodes converted to cases
  ret = round(n_resultingCase_nodes/n_contact_ep_nodes *100, 2)
  
  return(list(prop_converted = ret, n_contact_ep_nodes= n_contact_ep_nodes, n_resultingCase_nodes=n_resultingCase_nodes))
}
save(prop_cont_ep_person_convertedToCase, file = "prop_cont_ep_person_convertedToCase.R")
# prop_cont_ep_person_convertedToCase(elist = elist)

## proportion of missing source nodes among nodes 
# This statistic only consider cases in the network ie not all cases in the system
prop_missing_source_case_nodes = function(elist, nodeLineList){
  nodeLineList = nodeLineList %>%
    dplyr::select(id, group) %>% 
    dplyr::filter( (id %in% unique(c(elist$from, elist$to))) & (group %in% c("EVENT","PROBABLE", "SUSPECT", "CONFIRMED", "NOT_CLASSIFIED")) )  # retaining only case or event nodes in elist 
  
  # extracting contacts with parents nodes
  elist_case = elist %>%
    dplyr::filter( !(from %in% to) ) %>%  # keeping only parent case nodes that were not previousely known to be contact nodes,
    dplyr::distinct_at(., vars(from), .keep_all = TRUE)
  
  source_node_uuid = sort(elist_case$from_uuid_person) # contains nodes for events and case person in case elist also had event nodes
  
  sum_missing_source_case_nodes = length(source_node_uuid) 
  sum_caseEvent_nodes = nrow(nodeLineList)  # computing total case nodes
  prop_missing_source_case_nodes = round((sum_missing_source_case_nodes/sum_caseEvent_nodes)*100, 2)
  
  return(list(source_node_uuid = source_node_uuid, sum_caseEvent_nodes=sum_caseEvent_nodes, 
              prop_missing_source_case_nodes = prop_missing_source_case_nodes, 
              sum_missing_source_case_nodes = sum_missing_source_case_nodes ))
}
save(prop_missing_source_case_nodes, file = "prop_missing_source_case_nodes.R")
#prop_missing_source_case_nodes(elist=elist, nodeLineList = nodeLineList)

##
mergingData = function(dataList)
{ 
  case = dataList$cases.csv
  contact = dataList$contacts.csv
  region = dataList$regions.csv
  person = dataList$persons.csv
  district = dataList$districts.csv
  symptoms = dataList$case_symptoms.csv
  events = dataList$events.csv
  eventsParticipant = dataList$event_persons_involved.csv
  
  
  #filtering useful variables
  events = events[events$deleted == "f" , colnames(events) %in% c("id", "reportdatetime","eventstatus",  "disease", "deleted", "typeofplace", "eventlocation_id")]
  # selecting only  event participants with resulting cases
  # eventsParticipant = eventsParticipant[is.na(eventsParticipant$resultingcase_id) ==F, colnames(eventsParticipant) %in% c("event_id", "person_id", "resultingcase_id"), ]
  eventsParticipant = eventsParticipant[, colnames(eventsParticipant) %in% c("id", "event_id", "person_id", "resultingcase_id"), ]
  colnames(eventsParticipant)[1] = "id_EventParticipant"
  
  ## merging event and event participants
  eventsParticipantEvents =  merge(eventsParticipant, events, by.x = "event_id" , by.y = "id" , all.x = T, all.y = F) # merging event participants with events and 
  # do not keep events that has no participant
  # Merging elist with eventsParticipantEvents to have event detials for all contacts that resulted to cases
  # renaming and merging
  colnames(eventsParticipantEvents)[colnames(eventsParticipantEvents) == "resultingcase_id"] = "resultingcase_idEvent" 
  colnames(eventsParticipantEvents)[colnames(eventsParticipantEvents) == "person_id"] = "person_idEvent" 
  colnames(eventsParticipantEvents)[colnames(eventsParticipantEvents) == "reportdatetime"] = "reportdatetimeEvent" 
  # deleting duplicate persons in one event
  
  
  symptoms = symptoms[,colnames(symptoms) %in% c("id","onsetdate")]
  symptoms$onsetdate = dateTimeToDate(symptoms$onsetdate)
  ## Merging contact and region table
  
  conVar = c( "id","caze_id","district_id", "region_id", "person_id","reportdatetime", "lastcontactdate", "disease",
              "contactclassification","contactproximity","resultingcase_id", "followupstatus", "followupuntil", "contactstatus")
  
  contact = contact[contact$deleted == "f" & is.na(contact$caze_id) == F ,colnames(contact) %in% conVar] # deleting contacts with missing caseid
  
  
  #defining dates as date data types 
  contact$lastcontactdate = dateTimeToDate(contact$lastcontactdate)
  contact$reportdatetime = dateTimeToDate(contact$reportdatetime)
  contact$followupuntil = dateTimeToDate(contact$followupuntil)
  
  
  # deleting duplicate contacts ie duplicate edges linking two nodes
  contact = distinct(contact, caze_id, person_id, .keep_all = T) # keep this for now so users can see and clean their data
  #deleting edges linking a node to itselt
  contact = contact[contact$caze_id != contact$person_id,]
  ## giving the region and district id of the case to contacts that have NA for region and district of contact 
  # merging contact and case table keeping only cases that have contacts becuse focus here is on contacts
  #caseVar=c("id","creationdate","disease","investigateddate","reportdate","healthfacility_id",
  #          "person_id","symptoms_id","region_id","district_id", "caseclassification",
  #          "investigationstatus","hospitalization_id", "epidata_id","epidnumber",
  #          "outcome","outcomedate","caseage")
  caseVar=c("id","disease","reportdate","person_id","region_id","district_id", "caseclassification", "outcome", "epidnumber", "symptoms_id" )
  case = case[,colnames(case) %in% caseVar]
  
  # converting date time to date 
  # case$creationdate = dateTimeToDate(case$creationdate)
  #case$investigateddate = dateTimeToDate(case$investigateddate)
  case$reportdate = dateTimeToDate(case$reportdate)
  
  # colnames(case) =  c("caze_id","creationdateCase","diseaseCase","investigateddateCase","reportdateCase","healthfacility_idCase",
  #                     "person_idCase","symptoms_idCase","region_idCase","district_idCase", "caseclassificationCase",
  #                     "investigationstatusCase","hospitalization_idCase", "epidata_idCase","epidnumberCase",
  #                   "outcomeCase","outcomedateCase","ageCase")
  case = case %>% 
    rename(caze_id = id, diseaseCase = disease, reportdateCase = reportdate, person_idCase = person_id, region_idCase = region_id,
           district_idCase = district_id, caseclassificationCase = caseclassification,  epidnumberCase = epidnumber, outcomeCase = outcome, symptoms_idCase = symptoms_id)
  
  
  contCase = merge(contact,case, by = "caze_id", all.x = T, all.y = F ) # to ge the casevaraibles of contacts. Contacts that
  
  # cases without having contacts will not show up here.
  # giving contacts with missing region id  the id of the region of the case   
  temp1 = contCase[is.na(contCase$region_id) == T,] # cotacts with missiing region_id
  temp2 = contCase[is.na(contCase$region_id) == F,]
  temp1$region_id = temp1$region_idCase
  temp1$district_id = temp1$district_idCase
  contCase = rbind(temp1, temp2)
  
  # for(i in 1:nrow(contCase))
  #  {
  #   if(is.na(contCase$region_id[i]) == T)
  #    {
  #    contCase$region_id[i] = contCase$region_idCase[i]
  #    contCase$district_id[i] = contCase$district_idCase[i]
  #  }
  
  #  }
  
  # selecting cntacts with mim set of varables needed to plot
  #minVar = c("id","caze_id","reportdatetime", "disease", "contactclassification", "region_id", "district_id", "resultingcase_id", "contactproximity" )
  #contactMinVar = contCase[, colnames(contCase) %in% minVar]
  contactMinVar = contCase
  
  # merging contactMinVar with region to get teh names of the regions
  regionVar = c("id", "name")
  region = region[region$archived == "f",colnames(region) %in% regionVar]
  colnames(region) = c("region_id", "region_name")
  
  contRegion = merge(contactMinVar,region, by = "region_id", all.x = T, all.y = F) # do not keep regions that do not have a contact
  contRegion$region_name = as.character(contRegion$region_name)
  contRegion$disease = as.character(contRegion$disease)
  contRegion$contactclassification = as.character(contRegion$contactclassification)
  contRegion$contactproximity = as.character(contRegion$contactproximity)
  
  ## Merging contRegion with district table to know the district names of contacts
  disVar = c("id", "name")
  district = district[district$archived == "f", colnames(district) %in% disVar ]
  colnames(district) = c("district_id", "district_name")
  district$district_name = as.character(district$district_name)
  
  contRegionDist = merge(contRegion,district, by = "district_id", all.x = T, all.y = F)
  
  # deleting dupliccate contacts in contRegionDist
  contRegionDist = distinct(contRegionDist, person_idCase, person_id, .keep_all = T)  #  keep this in plot so that users can see and clean their data
  # deleting edges linking a node to itselt
  contRegionDist = contRegionDist[contRegionDist$person_idCase != contRegionDist$person_id,]
  
  
  #choosing final set of variables for contRegionDist to be exported
  expVar = c("id", "person_id", "person_idCase", "disease","contactproximity", "lastcontactdate", "reportdatetime", "contactclassification",
             "followupstatus","followupuntil", "resultingcase_id","disease", "caseclassificationCase", "contactstatus",
             "region_name","district_name", "outcomeCase", "caze_id") 
  contRegionDist = contRegionDist[ ,colnames(contRegionDist) %in% expVar ]
  
  
  #merging contRegionDist and person table to keep only persons that belongs to contRegionDist table
  
  
  #  case_id = as.character(case$caze_id)
  #  case_idCont = as.character(contRegionDist$caze_id)
  
  #adding person id of cases in contact table
  #   pIdCase = rep(NA, nrow(contRegionDist))
  #   personIdCase = case$person_id
  
  #  for(i in 1:length(case_idCont))
  #   {
  #    temp = case_idCont[i]
  #    if(is.na(temp) == T)
  #   {
  #     pIdCase[i] = NA
  #    } else{
  #      for(j in 1:length(case_id))
  #      {
  #       if(temp == case_id[j] )
  #        {
  #         pIdCase[i] = personIdCase[j]
  #       }
  #       
  #      } 
  #    }
  
  #   }
  
  
  
  ## defining contact categories based on proximity
  contRegionDist$label = NA
  contRegionDist$label[contRegionDist$contactproximity %in% c("FACE_TO_FACE_LONG","TOUCHED_FLUID","MEDICAL_UNSAVE","CLOTHES_OR_OTHER","PHYSICAL_CONTACT" )] = 1 
  contRegionDist$label[!(contRegionDist$contactproximity %in% c("FACE_TO_FACE_LONG","TOUCHED_FLUID","MEDICAL_UNSAVE","CLOTHES_OR_OTHER","PHYSICAL_CONTACT" ))] = 2
  
  # elist = data.frame(pIdCase, contRegionDist$person_id, contRegionDist$contactproximity, contRegionDist$id )
  
  #elist = data.frame(contRegionDist$person_idCase, contRegionDist$person_id, contRegionDist$label, contRegionDist$id, contRegionDist$resultingcase_id, 
  #                   contRegionDist$caze_id, contRegionDist$region_name )
  
  elist = contRegionDist
  
  col_orderElist <- c( "person_idCase", "person_id" ,"label", "id", "caze_id", "resultingcase_id", "region_name", "district_name", "contactproximity","lastcontactdate","reportdatetime", "contactclassification","followupstatus", "followupuntil",
                       "contactstatus", "disease", "caseclassificationCase", "outcomeCase")
  elist <- elist[, col_orderElist]
  colnames(elist)[1:2] = c("from", "to")
  
  #deleting duplicate edges
  elist = distinct(elist, from, to, .keep_all = T)
  #deleting edges linking a node to itselt
  elist = elist[elist$from != elist$to,]
  
  # defining attribures of elist
  elist$smooth = TRUE
  elist$dashes = TRUE
  elist$dashes[elist$label == 1] = FALSE #  using broken lines for low risk contacts Need to update mathod latter for events
  elist$arrows = "to"
  
  
  #status of each node or person in elist
  #get person id from resulting case id
  personVar=c("id", "sex")
  person = person[, colnames(person) %in% personVar]
  
  contConvCase = case[case$caze_id %in% elist$resultingcase_id,] ## cases resulted from contacts
  
  idPersonCaseCont = unique(as.character(c(elist$to, elist$from, contConvCase$person_idCase))) # uniqur persons in either case or contact table
  personUnique = person[person$id %in% idPersonCaseCont,] # uniqur personts in network diagram of elist
  
  Classification = rep("HEALTHY",nrow(personUnique)) # classification if person
  personId = personUnique$id
  # selzcting cases that belongs to cntact table or cntacts converted to cases
  idCaseUnique = unique(na.omit(c(elist$caze_id, elist$resultingcase_id)))
  caseUnique = case[case$caze_id %in% idCaseUnique, ]
  
  casPersonId = caseUnique$person_idCase # using caseUnique table, person id that belong to the set of cases in network
  personClass = as.character(caseUnique$caseclassificationCase)
  
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
  nodeLineList$group = nodeLineList$Classification
  nodeLineList$label = nodeLineList$id
  nodeLineList$value = 1
  nodeLineList$shape = c("icon")
  nodeLineList$code = c("f007")
  #nodeLineList$shadow = F, 
  
  ## Building elistCaseEvent for cases that resulted from events and adding it to elist to get the list of all edges and noded in the combined network
  
  #extracting all eventsParticipantEvents that resulted to cases
  eventsParticipantEvents = eventsParticipantEvents[is.na(eventsParticipantEvents$resultingcase_idEvent) != T,]
  # Merging with case table to get region and district of the resulting casses
  eventsParticipantEventsCases =  merge(eventsParticipantEvents, case, by.x = "resultingcase_idEvent" , by.y = "caze_id" , all.x = T, all.y = F ) # 
  
  
  
  #temp =  merge(elist, eventsParticipantEvents, by.x = "caze_id" , by.y = "resultingcase_idEvent" , all.x = F, all.y = F ) # cases from eventsPaart that have contacts
  # temp will contain only resulting cases from event that do have a contact.
  # We may also need to show resulting cases that do not have contacts
  
  # selecting unique contact between cases and event
  eventsParticipantEventsCases = distinct(eventsParticipantEventsCases, event_id, person_idEvent,.keep_all = T) # person_idCase = person_idCase
  # merging eventsParticipantEventsCases with region and district
  eventsParticipantEventsCasesReg = merge(eventsParticipantEventsCases,region, by.x = "region_idCase", by.y = "region_id" , all.x = T, all.y = F) 
  eventsParticipantEventsCasesReg$region_name = as.character(eventsParticipantEventsCasesReg$region_name)
  
  eventsParticipantEventsCasesRegDist = merge(eventsParticipantEventsCasesReg,district, by.x = "district_idCase", by.y ="district_id"  , all.x = T, all.y = F)
  
  #dropping useless columns
  eventsParticipantEventsCasesRegDist = eventsParticipantEventsCasesRegDist[, colnames(eventsParticipantEventsCasesRegDist) %in% c("resultingcase_idEvent", "event_id", "id_EventParticipant", "diseaseCase",
                                                                                                                                   "reportdateCase","person_idCase","symptoms_idCase", "caseclassificationCase",
                                                                                                                                   "outcomeCase", "district_name", "region_name")] # keeping only impottant baraobles, may add the other s latter such as  "eventstatus", "typeofplace",
  
  
  # maping and renaming eventsParticipantEventsCasesRegDist to hve the same colnames as elist
  eventsParticipantEventsCasesRegDist = eventsParticipantEventsCasesRegDist %>% rename(from = event_id , to = person_idCase, id = id_EventParticipant, resultingcase_id = resultingcase_idEvent,
                                                                                       reportdatetime = reportdateCase, disease = diseaseCase )
  eventsParticipantEventsCasesRegDist$caze_id = eventsParticipantEventsCasesRegDist$from
  
  
  
  elistEvent = eventsParticipantEventsCasesRegDist[colnames(eventsParticipantEventsCasesRegDist) %in% c("from", "to", "id", "caze_id", "resultingcase_id","region_name",
                                                                                                        "district_name", "reportdatetime", "disease", "caseclassificationCase")]
  
  
  
  
  elistEvent$label = 2 # update latter
  elistEvent$smooth = TRUE
  elistEvent$dashes = TRUE
  elistEvent$dashes[elistEvent$label == 1] = FALSE #  using broken lines for low risk contacts Need to update mathod latter for events
  elistEvent$arrows = "to"
  
  # selecting few rows of elist
  elistReduced = elist[, colnames(elist) %in% colnames(elistEvent)]
  
  
  # ordering col names
  varOrder = c("from","to","smooth", "dashes","arrows", "label","id", "caze_id", "resultingcase_id", "region_name", "district_name", "reportdatetime", "disease","caseclassificationCase")
  elistEvent = elistEvent[, varOrder]
  elistReduced = elistReduced[, varOrder]
  
  #Stacking elistEvent and elistReduced to include event to the elist of contacts
  elistCaseEvent = rbind(elistReduced,elistEvent)
  
  #defining node properties for elistEvent nodes
  eventNode = data.frame(id = elistEvent$from,  Classification = c("EVENT"), group =c("EVENT"),label = elistEvent$from, code = c("f0c0") )
  #selecting unique event node since many cases comes from the same event
  
  
  eventNode = distinct(eventNode, id, .keep_all = T)
  caseNode = data.frame(id = elistEvent$to,  Classification = elistEvent$caseclassificationCase, group = elistEvent$caseclassificationCase,
                        label = elistEvent$to, code = c("f007") )
  nodeListCaseEvent = rbind(eventNode, caseNode)
  
  nodeListCaseEvent$value = 1
  nodeListCaseEvent$shape = c("icon")
  nodeListCaseEvent$sex = c("MALE") # jsut named all male for now since we do not analyse by sex
  #nodeListCaseEvent$shadow = F, 
  
  nodeListCaseEvent = nodeListCaseEvent[,colnames(nodeLineList)]  # order columns as in nodelist
  nodeListCaseEvent = rbind(nodeListCaseEvent, nodeLineList)
  # table(nodeListCaseEvent$id)[table(nodeListCaseEvent$id)>1] # to get dupllicates
  
  # deleting duplicate nodes at random, this is not the optimum method to do, can be improved
  nodeListCaseEvent = distinct(nodeListCaseEvent, id, .keep_all = T)
  
  
  ############### determining serial interval  ###########
  # selecting unique case id from contats table
  temp = contRegionDist[,colnames(contRegionDist) %in% c("resultingcase_id", "caze_id", "disease", "region_name", "district_name","reportdatetime" )] # these varibales are used to filter commands from ui latter
  selCases = temp[is.na(temp$resultingcase_id) == F,] # edge table with casee id for source cases and resulting cases. We only use data for cases whose contacts became cases
  uniqCaseId = unique(c(selCases$caze_id, selCases$resultingcase_id))
  
  #merging uniqCaseId with case table to know the syptom of the cases 
  temp = case[case$caze_id %in% uniqCaseId, c("caze_id", "symptoms_idCase") ]  # cases in involved in contact network
  #merging with syptoms
  caseSymp = merge(temp,symptoms, by.x = "symptoms_idCase", by.y = "id", all.x = T, all.y = F)
  caseSymp = caseSymp[, colnames(caseSymp) != "symptoms_idCase"]
  #merging caseSymp with selCases
  selCasesSympCase  = merge(selCases, caseSymp, by = "caze_id", all.x = T, all.y = F)
  selCasesSympResultCase = merge(selCasesSympCase, caseSymp, by.x = "resultingcase_id", by.y = "caze_id", all.x = T, all.y = F)
  selCasesSympResultCase$si = as.numeric(c(selCasesSympResultCase$onsetdate.y - selCasesSympResultCase$onsetdate.x))
  siDat = selCasesSympResultCase[, colnames(selCasesSympResultCase) %in% c("si","reportdatetime", "disease", "region_name","district_name" )]
  siDat = siDat[is.na(siDat$si) == F,]  # dropping missing values
  
  #return(list(contRegionDist = contRegionDist, nodeLineList = nodeLineList, elist = elist, siDat = siDat, 
  #            nodeListCaseEvent = nodeListCaseEvent, elistCaseEvent = elistCaseEvent))
  return(list(contRegionDist = contRegionDist, nodeLineList = nodeListCaseEvent, elist = elistCaseEvent, siDat = siDat))
}
save(mergingData, file = "mergingData.R")

##
# Merging data by reading tables from db
mergingDataFromDB = function(sormas_db, fromDate, toDate, uniquePersonPersonContact = TRUE)
{ 
  ## computing default time based on 90 days in the past if  not provided by user
  if(missing(fromDate) | missing(toDate)){
    fromDate = as.character(Sys.Date() - delay) 
    toDate = as.character(Sys.Date()) 
  }
  
  # connecting to db
  #sormas_db = dbConnect(PostgreSQL(), user="sormas_user", dbname="sormas", password = "password", host="127.0.0.1", port="5432")
  #con = dbConnect(PostgreSQL(), user=DB_USER,  dbname=DB_NAME, password = DB_PASS, host=DB_HOST, port=DB_PORT)
  
  # reading unique cases based on all varaibles. 
  # use  where deleted = FALSE  and caseclassification != 'NO_CASE'  in case you want to eliminate not a cases
  
  # case = dbGetQuery(sormas_db,"select distinct id, disease, reportdate,person_id,region_id,district_id, caseclassification, outcome, epidnumber, symptoms_id  
  #                        from public.cases
  #                        where deleted = FALSE
  #                       ")  
  queryCase <- paste0("SELECT  DISTINCT id, uuid, disease, reportdate,person_id,region_id,district_id, caseclassification, outcome, epidnumber, symptoms_id
                          FROM public.cases 
                          WHERE deleted = FALSE and reportdate between '", fromDate, "' and '", toDate, "' ")
  case = dbGetQuery(sormas_db,queryCase)
  
  ## reading contact data ###
  # excluded contacts without a source case. Including them will mess up stuffs when merging contdacts and case table
  # contact = dbGetQuery(sormas_db,"SELECT  DISTINCT id,caze_id,district_id, region_id, person_id,reportdatetime, lastcontactdate, disease,
  #             contactclassification,contactproximity,resultingcase_id, followupstatus, followupuntil, contactstatus,
  #             contactcategory, relationtocase
  #                        FROM public.contact
  #                        WHERE deleted = FALSE and caze_id IS NOT NULL
  #                       ")
  queryContact <- paste0("SELECT  DISTINCT id, uuid, caze_id,district_id, region_id, person_id,reportdatetime, lastcontactdate, disease,
                    contactclassification,contactproximity,resultingcase_id, followupstatus, followupuntil, contactstatus,
                    contactcategory, relationtocase
                          FROM public.contact
                          WHERE deleted = FALSE and caze_id IS NOT NULL and reportdatetime between '", fromDate, "' and '", toDate, "' ")
  contact = dbGetQuery(sormas_db,queryContact)
  
  ### reading person data  ###
  person = dbGetQuery(sormas_db,"SELECT  DISTINCT id, uuid, sex 
                               from public.person
                               ") 
  # reading region
  region = dbGetQuery(sormas_db,"SELECT  DISTINCT id, name
                               from public.region
                               where archived = FALSE
                               ") 
  
  #loading district
  district = dbGetQuery(sormas_db,"SELECT  DISTINCT id, name
                               from public.district
                               where archived = FALSE
                               ")
  #loading symptom data
  symptoms = dbGetQuery(sormas_db,"SELECT  DISTINCT id, onsetdate
                               from public.symptoms
                              ")
  # reading event
  # events = dbGetQuery(sormas_db,"select distinct id, reportdatetime,eventstatus,disease, typeofplace, eventlocation_id
  #                        from public.events
  #                        where deleted = FALSE and eventstatus != 'DROPPED'
  #                       ")
  queryEvent <- paste0("SELECT  DISTINCT id, uuid, reportdatetime,eventstatus,disease, typeofplace, eventlocation_id, archived, risklevel
                          FROM public.events
                          WHERE deleted = FALSE and eventstatus != 'DROPPED' and reportdatetime between '", fromDate, "' and '", toDate, "' ")
  events = dbGetQuery(sormas_db,queryEvent)
  
  
  ## reading event participants
  eventsParticipant = dbGetQuery(sormas_db,"SELECT  DISTINCT id, uuid, event_id, person_id, resultingcase_id
                               from public.eventParticipant
                               WHERE deleted = FALSE"
                                 )
  ## reading location; this can later be nested in event sql command
  queryLocation <- paste0("SELECT  id, district_id, region_id
                          FROM public.location")
  location = dbGetQuery(sormas_db,queryLocation)
  
  #disconnect from db
  # dbDisconnect(sormas_db)
  
  # merging CONFIRMED_UNKNOWN_SYMPTOMS and  CONFIRMED_NO_SYMPTOMS as confirmed 
  case$caseclassification[case$caseclassification == "CONFIRMED_NO_SYMPTOMS" ] = "CONFIRMED"
  case$caseclassification[case$caseclassification == "CONFIRMED_UNKNOWN_SYMPTOMS" ] = "CONFIRMED"
  
  
  ## converting all date formats from POSIXct to date
  case$reportdate = dateTimeToDate(case$reportdate)
  #
  contact$reportdatetime = dateTimeToDate(contact$reportdatetime)
  contact$lastcontactdate = dateTimeToDate(contact$lastcontactdate)
  contact$followupuntil = dateTimeToDate(contact$followupuntil)
  #
  symptoms$onsetdate  = dateTimeToDate(symptoms$onsetdate)
  #
  events$reportdatetime = dateTimeToDate(events$reportdatetime)
  #
  
  #cleaning data 
  
  
  # renaming varaibles, 
  # migrate later to sql code
  eventsParticipant = eventsParticipant %>%
    dplyr::rename(id_EventParticipant = id, uuid_EventParticipant = uuid, resultingcase_idEvent = resultingcase_id, 
                  person_idEvent = person_id)
  
  
  events = events %>%
    dplyr::rename(id_event = id, uuid_event = uuid, reportdatetimeEvent = reportdatetime, diseaseEvents = disease, risklevelEvent = risklevel ) %>%
    dplyr::mutate(archivedEvent = ifelse(archived == TRUE, "t", "f"), .keep = "unused")
  
  #merging Event and location
  eventLocation = events %>%
    dplyr::left_join(., location,  by=c("eventlocation_id" = "id" )) %>%
    dplyr::rename(region_idEvent = region_id , district_idEvent = district_id )
  
  ## merging event and event participants
  eventsParticipantEvents =  merge(eventsParticipant, eventLocation, by.x = "event_id" , by.y = "id_event" , all.x = F, all.y = F) # merging event participants with events and 
  # exclude events that has no participant and also ep that has no event since the list of event excluded dropped events.
  
  ## Mergion evet and location
  
  # renaming 
  # eventsParticipantEvents = eventsParticipantEvents %>% 
  #   rename(resultingcase_idEvent = resultingcase_id, person_idEvent = person_id, reportdatetimeEvent = reportdatetime)
  #
  
  case = case %>% 
    dplyr::rename(caze_id = id, uuid_case = uuid, diseaseCase = disease, reportdateCase = reportdate, person_idCase = person_id, region_idCase = region_id,
                  district_idCase = district_id, caseclassificationCase = caseclassification,  epidnumberCase = epidnumber, outcomeCase = outcome,
                  symptoms_idCase = symptoms_id)
  
  region = region %>%
    dplyr::rename(region_id = id, region_name = name)
  #
  district = district %>%
    dplyr::rename(district_id = id, district_name = name)
  #
  person = person %>%
    dplyr::rename(uuid_person = uuid, id_person = id)
  
  ## Merging data and cleaning
  contCase = merge(contact,case, by = "caze_id", all.x = F, all.y = F ) # to ge the casevaraibles of contacts.
  
  # cases without having contacts will not show up here.
  # giving contacts with missing region id  the id of the region of the case   
  temp1 = contCase[is.na(contCase$region_id) == T,] # cotacts with missiing region_id
  temp2 = contCase[is.na(contCase$region_id) == F,]
  temp1$region_id = temp1$region_idCase
  temp1$district_id = temp1$district_idCase
  contCase = rbind(temp1, temp2)
  
  # selecting cntacts with mim set of varables needed to plot
  #minVar = c("id","caze_id","reportdatetime", "disease", "contactclassification", "region_id", "district_id", "resultingcase_id", "contactproximity" )
  #contactMinVar = contCase[, colnames(contCase) %in% minVar]
  
  # merging contCase with region to get the names of the regions
  
  contRegion = merge(contCase,region, by = "region_id", all.x = T, all.y = F) # do not keep regions that do not have a contact
  
  contRegionDist = merge(contRegion,district, by = "district_id", all.x = T, all.y = F)
  
  # deleting dupliccate contacts for the same disease between the same persons in contRegionDist
  if (uniquePersonPersonContact == TRUE){
    contRegionDist = distinct(contRegionDist, person_idCase, person_id, disease, .keep_all = T) 
  }
  
  #   
  #choosing final set of variables for contRegionDist to be exported and deleting edges linking a node to itselt
  # contRegionDist = contRegionDist[contRegionDist$person_idCase != contRegionDist$person_id,]
  contRegionDist = contRegionDist %>%
    dplyr::select(id, person_id, person_idCase, disease,contactproximity, lastcontactdate, reportdatetime, contactclassification,
                  followupstatus,followupuntil, resultingcase_id, caseclassificationCase, contactstatus,
                  region_name,district_name, outcomeCase,caze_id, relationtocase, uuid_case, uuid, reportdateCase) %>%
    dplyr::filter(person_idCase != person_id ) 
  
  
  # expVar = c("id", "person_id", "person_idCase", "disease","contactproximity", "lastcontactdate", "reportdatetime", "contactclassification",
  #            "followupstatus","followupuntil", "resultingcase_id", "caseclassificationCase", "contactstatus",
  #            "region_name","district_name", "outcomeCase", "caze_id", "relationtocase") 
  # contRegionDist = contRegionDist[ ,colnames(contRegionDist) %in% expVar ]
  # 
  
  ## defining contact categories based on proximity
  # Would later add a configuration for this
  contRegionDist$label = NA
  contRegionDist$label[contRegionDist$contactproximity %in% c("FACE_TO_FACE_LONG","TOUCHED_FLUID","MEDICAL_UNSAVE","CLOTHES_OR_OTHER","PHYSICAL_CONTACT" )] = 1 
  contRegionDist$label[!(contRegionDist$contactproximity %in% c("FACE_TO_FACE_LONG","TOUCHED_FLUID","MEDICAL_UNSAVE","CLOTHES_OR_OTHER","PHYSICAL_CONTACT" ))] = 2
  
  
  
  #defining attributes of elist from contRegionDist
  elist =  contRegionDist %>%
    dplyr::rename(from = person_idCase, to = person_id) %>%
    dplyr::mutate(uuid_label = substr(uuid,1,6), smooth = TRUE, dashes = ifelse(label ==2,TRUE, FALSE), arrows = "to", uuid_case= substr(uuid_case,1,6),
                  eventstatus = NA,  entityType = "Case", archivedEvent = NA, risklevelEvent = NA, .keep = "all") %>%
    dplyr::left_join(., person, by=c("from" = "id_person" )) %>%
    dplyr::mutate(from_uuid_person = substr(uuid_person,1,6), sex_from_person = sex , .keep = "unused") %>%
    dplyr::left_join(., person, by=c("to" = "id_person" )) %>%
    dplyr::mutate(to_uuid_person = substr(uuid_person,1,6), sex_to_person = sex, .keep = "unused") 
  
  # col_orderElist <- c( "person_idCase", "person_id" ,"label", "id", "caze_id", "resultingcase_id", "region_name", "district_name", "contactproximity",
  #                      "lastcontactdate","reportdatetime", "contactclassification","followupstatus", "followupuntil", "contactstatus", "disease",
  #                      "caseclassificationCase", "outcomeCase", "relationtocase")
  # elist <- elist[, col_orderElist]
  # colnames(elist)[1:2] = c("from", "to")
  
  # defining attribures of elist
  
  # elist$smooth = TRUE
  # elist$dashes = TRUE
  # elist$dashes[elist$label == 1] = FALSE #  using broken lines for low risk contacts Need to update mathod latter for events
  # elist$arrows = "to"
  # 
  # # adding eventstatus to elist to facilitate stacking with events
  # elist =  elist %>%
  #   mutate(eventstatus = NA)
  
  
  
  # defining node data
  #get person id from resulting case id
  contConvCase = case[case$caze_id %in% elist$resultingcase_id,] ## cases resulted from contacts
  
  idPersonCaseCont = unique(as.character(c(elist$to, elist$from, contConvCase$person_idCase))) # uniqur persons in either case or contact table
  personUnique = person[person$id_person %in% idPersonCaseCont,] # uniqur personts in network diagram of elist
  
  Classification = rep("HEALTHY",nrow(personUnique)) # classification if person
  personId = personUnique$id
  # selzcting cases that belongs to cntact table or cntacts converted to cases
  idCaseUnique = unique(na.omit(c(elist$caze_id, elist$resultingcase_id)))
  caseUnique = case[case$caze_id %in% idCaseUnique, ]
  
  casPersonId = caseUnique$person_idCase # using caseUnique table, person id that belong to the set of cases in network
  personClass = as.character(caseUnique$caseclassificationCase)
  
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
  
  # %>%
  #   dplyr::mutate(to = substr(uuid_person, 1,6), .keep = "unused" )
  
  
  
  # nodeLineList$group = nodeLineList$Classification
  # nodeLineList$label = nodeLineList$id
  # nodeLineList$value = 1
  # nodeLineList$shape = c("icon")
  # nodeLineList$code = c("f007")
  #nodeLineList$shadow = F, 
  
  
  ## Building elistCaseEvent for cases that resulted from events and adding it to elist to get the list of all edges and noded in the combined network
  #extracting all eventsParticipantEvents that resulted to cases
  
  #      eventsParticipantEventsHealthy = eventsParticipantEvents[is.na(eventsParticipantEvents$resultingcase_idEvent) == T,] # ep that are healthy
  #      eventsParticipantEvents = eventsParticipantEvents[is.na(eventsParticipantEvents$resultingcase_idEvent) != T,]  # ep that converted to cases
  
  # Merging with case table to get region and district of the resulting casses
  eventsParticipantEventsCases =  merge(eventsParticipantEvents, case, by.x = "resultingcase_idEvent" , by.y = "caze_id" , all.x = T, all.y = F ) # 
  
  
  
  #temp =  merge(elist, eventsParticipantEvents, by.x = "caze_id" , by.y = "resultingcase_idEvent" , all.x = F, all.y = F ) # cases from eventsPaart that have contacts
  # temp will contain only resulting cases from event that do have a contact.
  # We may also need to show resulting cases that do not have contacts
  
  # selecting unique contact between cases and event
  if (uniquePersonPersonContact == TRUE){
    eventsParticipantEventsCases = distinct(eventsParticipantEventsCases, event_id, person_idEvent, diseaseEvents, .keep_all = T) 
  }
  #eventsParticipantEventsCases = distinct(eventsParticipantEventsCases, event_id, person_idEvent,.keep_all = T) # person_idCase = person_idCase
  
  
  # merging eventsParticipantEventsCases with region and district
  
  ## eventsParticipantEventsCasesReg = merge(eventsParticipantEventsCases,region, by.x = "region_idCase", by.y = "region_id" , all.x = T, all.y = F) 
  eventsParticipantEventsCasesReg = merge(eventsParticipantEventsCases,region, by.x = "region_idEvent", by.y = "region_id" , all.x = T, all.y = F) 
  #eventsParticipantEventsCasesReg$region_name = as.character(eventsParticipantEventsCasesReg$region_name)
  
  # eventsParticipantEventsCasesRegDist = merge(eventsParticipantEventsCasesReg,district, by.x = "district_idCase", by.y ="district_id"  , all.x = T, all.y = F)
  eventsParticipantEventsCasesRegDist = merge(eventsParticipantEventsCasesReg,district, by.x = "district_idEvent", by.y ="district_id"  , all.x = T, all.y = F)
  
  ## Adding relationtocase and contactcategory to eventsParticipantEventsCasesRegDist
  # eventsParticipantEventsCasesRegDist = eventsParticipantEventsCasesRegDist %>%
  #   dplyr::mutate(relationtocase = NA, contactcategory = "HIGH_RISK", .keep = "all") 
  
  eventsParticipantEventsCasesRegDist = eventsParticipantEventsCasesRegDist %>%
    #dplyr::mutate(relationtocase = NA, .keep = "all") %>%
    # select(resultingcase_idEvent, event_id, id_EventParticipant, diseaseCase,
    #          reportdateCase,person_idCase,symptoms_idCase, caseclassificationCase,
    #          outcomeCase, district_name, region_name, relationtocase, eventstatus, uuid_event, uuid_case, uuid_EventParticipant, typeofplace) %>%
    dplyr::mutate(from = event_id, to = person_idEvent, id = id_EventParticipant, resultingcase_id = resultingcase_idEvent,
                  reportdatetime = reportdatetimeEvent, disease = diseaseEvents, caze_id = event_id, relationtocase = NA, .keep = "all")
  
  
  #dropping useless columns
  # eventsParticipantEventsCasesRegDist = eventsParticipantEventsCasesRegDist[, colnames(eventsParticipantEventsCasesRegDist) %in% c("resultingcase_idEvent", "event_id", "id_EventParticipant", "diseaseCase",
  #                                                                                                                                  "reportdateCase","person_idCase","symptoms_idCase", "caseclassificationCase",
  #                                                                                                                                  "outcomeCase", "district_name", "region_name", "relationtocase", "eventstatus")] # keeping only impottant baraobles, may add the other s latter such as  "eventstatus", "typeofplace",
  # 
  
  # maping and renaming eventsParticipantEventsCasesRegDist to have the same colnames as elist so that we can stack them
  # eventsParticipantEventsCasesRegDist = eventsParticipantEventsCasesRegDist %>% 
  #   rename(from = event_id , to = person_idCase, id = id_EventParticipant, resultingcase_id = resultingcase_idEvent,
  #          reportdatetime = reportdateCase, disease = diseaseCase)
  # eventsParticipantEventsCasesRegDist$caze_id = eventsParticipantEventsCasesRegDist$from
  # 
  # definint elist of event similar to that of contacts
  elistEvent =  eventsParticipantEventsCasesRegDist %>%
    dplyr::mutate(uuid_label = substr(uuid_EventParticipant,1,6), from_uuid_person = substr(uuid_event,1,6) , label = 1, smooth = TRUE, dashes = FALSE, arrows = "to",
                  entityType = "Event", .keep = "unused" ) %>%
    dplyr::left_join(., person, by=c("to" = "id_person" )) %>%
    dplyr::mutate(to_uuid_person = substr(uuid_person,1,6), .keep = "unused" )
  
  # elistEvent = eventsParticipantEventsCasesRegDist[colnames(eventsParticipantEventsCasesRegDist) %in% c("from", "to", "id", "caze_id", "resultingcase_id","region_name",
  #                                                                                                       "district_name", "reportdatetime", "disease", "caseclassificationCase", "relationtocase", "eventstatus")]
  # # 
  # elistEvent$label = 2 # update later if need be
  # elistEvent$smooth = TRUE
  # elistEvent$dashes = TRUE
  # elistEvent$dashes[elistEvent$label == 1] = FALSE #  using broken lines for low risk contacts Need to update mathod later for events
  # elistEvent$arrows = "to"
  
  
  
  #defining node properties for elistEvent nodes
  eventNode = data.frame(id = elistEvent$event_id,  Classification = c("EVENT"), group =c("EVENT"),label = elistEvent$from_uuid_person, code = c("f0c0") )
  
  #selecting unique event node since many cases comes from the same event
  # eventNode = distinct(eventNode, id, .keep_all = T)
  
  # caseNode = data.frame(id = elistEvent$person_idEvent,  Classification = elistEvent$caseclassificationCase, group = elistEvent$caseclassificationCase,
  #                            label = substr(elistEvent$to_uuid_person,1,6), code = c("f007") )
  
  caseNode =    elistEvent %>%
    dplyr::mutate(id = person_idEvent,  Classification = caseclassificationCase, 
                  group = ifelse(is.na(caseclassificationCase) ==T, "HEALTHY", caseclassificationCase),
                  label = substr(to_uuid_person,1,6), code = c("f007"),  .keep = "none")
  
  
  
  nodeListCaseEvent = rbind(eventNode, caseNode)
  
  
  nodeListCaseEvent = nodeListCaseEvent %>%
    dplyr::mutate(value = 1,shape = c("icon"), sex = NA)
  
  
  # nodeListCaseEvent$value = 1
  # nodeListCaseEvent$shape = c("icon")
  # nodeListCaseEvent$sex = c("MALE") # jsut named all male for now since we do not analyse by sex
  # #nodeListCaseEvent$shadow = F, 
  # 
  
  ## stacking nodelist and elist from events and contacts
  
  # stacking nodelinst
  nodeLineList = nodeLineList %>%
    dplyr::mutate(id = id_person, .keep = "unused" ) %>%
    dplyr::select(-c(uuid_person))
  
  nodeListCaseEvent = nodeListCaseEvent[,colnames(nodeLineList)]  # order columns as in nodelist
  nodeListCaseEvent = rbind(nodeListCaseEvent, nodeLineList)
  # table(nodeListCaseEvent$id)[table(nodeListCaseEvent$id)>1] # to get dupllicates
  
  # deleting duplicate nodes at random, this is not the optimum method to do,
  # Improve laterlater by considering classification of duplicate nodes and keep the once once that are likes to confirmed cases.
  # nodeListCaseEvent = distinct(nodeListCaseEvent, id,  .keep_all = T)
  nodeListCaseEvent = distinct(nodeListCaseEvent, label,  .keep_all = T)
  
  nodeListCaseEvent = nodeListCaseEvent %>%
    dplyr::mutate(title = label)
  
  # stacking elist
  # ordering  and combining elist and elistEvent  
  #to_uuid_person
  
  elist =  elist %>%
    dplyr::select(from,	to,	smooth,	dashes,	arrows,	label,	id,	caze_id,	resultingcase_id,	region_name,	district_name,	reportdatetime,
                  disease,	caseclassificationCase,	relationtocase,	eventstatus,	entityType, uuid_label, from_uuid_person,archivedEvent, risklevelEvent, to_uuid_person) 
  # %>%
  #   dplyr::mutate(from = from_uuid_person, to = to_uuid_person, id = label, label = label,  .keep = "unused" )
  
  elistEvent  = elistEvent %>%
    dplyr::select(event_id,	to,	smooth,	dashes,	arrows,	label,	id,	caze_id,	resultingcase_id,	region_name,	district_name,	reportdatetime,
                  disease,	caseclassificationCase,	relationtocase,	eventstatus,	entityType, uuid_label, from_uuid_person, archivedEvent, risklevelEvent, to_uuid_person) %>%
    dplyr::mutate(from =event_id,  .keep = "unused" )
  
  
  # selecting few columns from elist to match those in elistEvent
  # elist = elist[, colnames(elist) %in% colnames(elistEvent)]
  
  # 
  # 
  # varOrder = c("from","to","smooth", "dashes","arrows", "label","id", "caze_id", "resultingcase_id", "region_name", "district_name",
  #              "reportdatetime", "disease","caseclassificationCase", "relationtocase", "eventstatus")
  # elistEvent = elistEvent[, varOrder]
  
  elist = elist[ , colnames(elistEvent)]
  
  # adding source type to elist to denote if the record is from an event or a case
  # elistEvent = elistEvent %>%
  #   dplyr::mutate(entityType = "event", .keep = "all")
  # elist = elist %>%
  #   dplyr::mutate(entityType = "case", .keep = "all")
  
  #Stacking elistEvent and elist to include event to the elist of contacts
  elistCaseEvent = rbind(elist,elistEvent)
  
  # dropping useless columns form elistCaseEvent that are not needed by the network
  elistCaseEvent = elistCaseEvent  %>%
    dplyr::select(-c(caseclassificationCase))
  
  ############### determining serial interval  ###########
  # selecting unique case id from contats table
  temp = contRegionDist[,colnames(contRegionDist) %in% c("resultingcase_id", "caze_id", "disease", "region_name", "district_name","reportdatetime" )] # these varibales are used to filter commands from ui latter
  selCases = temp[is.na(temp$resultingcase_id) == F,] # edge table with casee id for source cases and resulting cases. We only use data for cases whose contacts became cases
  uniqCaseId = unique(c(selCases$caze_id, selCases$resultingcase_id))
  
  #merging uniqCaseId with case table to know the syptom of the cases 
  temp = case[case$caze_id %in% uniqCaseId, c("caze_id", "symptoms_idCase") ]  # cases in involved in contact network
  #merging with syptoms
  caseSymp = merge(temp,symptoms, by.x = "symptoms_idCase", by.y = "id", all.x = T, all.y = F)
  caseSymp = caseSymp[, colnames(caseSymp) != "symptoms_idCase"]
  #merging caseSymp with selCases
  selCasesSympCase  = merge(selCases, caseSymp, by = "caze_id", all.x = T, all.y = F)
  selCasesSympResultCase = merge(selCasesSympCase, caseSymp, by.x = "resultingcase_id", by.y = "caze_id", all.x = T, all.y = F)
  selCasesSympResultCase$si = as.numeric(c(selCasesSympResultCase$onsetdate.y - selCasesSympResultCase$onsetdate.x))
  siDat = selCasesSympResultCase[, colnames(selCasesSympResultCase) %in% c("si","reportdatetime", "disease", "region_name","district_name" )]
  siDat = siDat[is.na(siDat$si) == F,]  # dropping missing values
  
  #return(list(contRegionDist = contRegionDist, nodeLineList = nodeLineList, elist = elist, siDat = siDat, 
  #            nodeListCaseEvent = nodeListCaseEvent, elistCaseEvent = elistCaseEvent))
  return(list(contRegionDist = contRegionDist, nodeLineList = nodeListCaseEvent, elist = elistCaseEvent, siDat = siDat))
}
save(mergingDataFromDB, file = "mergingDataFromDB.R")

### exporting event from sormas db ----
sormas_db = dbConnect(PostgreSQL(), user=DB_USER,  dbname=DB_NAME, password = DB_PASS, host=DB_HOST, port=DB_PORT)
eventExport = function(sormas_db, fromDate, toDate){
  # loading tables from sormas db
  # leading event
  queryEvent <- paste0("SELECT uuid AS uuid_event, id AS id_event, eventinvestigationstatus, reportdatetime AS reportdatetime_event, eventstatus, disease AS disease_event, typeofplace AS typeofplace_event,
      creationdate AS creationdate_event, enddate AS enddate_event, startdate AS startdate_event, archived AS archived_event, nosocomial AS nosocomial_event,
      srctype AS srctype_event, risklevel AS risklevel_event,  eventlocation_id, eventmanagementstatus, eventtitle
                        FROM public.events
                        WHERE deleted = FALSE and eventstatus != 'DROPPED' and reportdatetime between '", fromDate, "' and '", toDate, "' ")
  event = dbGetQuery(sormas_db,queryEvent)
  
  ## reading event participants data that are in events only; this is needed to compute the number of ep per events
  queryEventPart <- sprintf("SELECT  id AS id_eventPart, event_id AS event_id_eventpart, person_id AS person_id_eventPart, resultingcase_id AS resultingcase_id_eventPart
                        FROM public.eventParticipant
                        WHERE deleted = FALSE and event_id  in (%s)", paste("'", event$id_event, "'",collapse=",") )
  eventParticipant = dbGetQuery(sormas_db, queryEventPart)
  
  ## reading location; selecting only locations that correspond to an events. This improve performance
  queryLocation <- sprintf("SELECT  id AS id_location, district_id AS district_id_location, region_id AS region_id_location,
    facility_id AS facility_id_location, facilitytype AS facilitytype_location, latitude, longitude 
                        FROM public.location
                        WHERE id  in (%s)", paste("'", na.omit(event$eventlocation_id), "'",collapse=",") )
  location = dbGetQuery(sormas_db,queryLocation)
  
  # load region 
  region = dbGetQuery(
    sormas_db,
    "SELECT id AS region_id, name AS region_name
    FROM public.region
    WHERE archived = FALSE"
  ) 
  # load district
  district = dbGetQuery(
    sormas_db,
    "SELECT id AS district_id, name AS district_name
    FROM district
    WHERE archived = FALSE"
  )
  
  # merging event data with jurisdiction data
  event = event %>%
    dplyr::mutate(reportdatetime_event = as.Date(format(reportdatetime_event, "%Y-%m-%d")), 
                  startdate_event = as.Date(format(startdate_event, "%Y-%m-%d")),
                  enddate_event = as.Date(format(enddate_event, "%Y-%m-%d")),
                  creationdate_event = as.Date(format(creationdate_event, "%Y-%m-%d"))  ) %>%  # converting dates from POSIXct class to Date class
    dplyr::mutate(relevantdate_event = coalesce(startdate_event, reportdatetime_event )  ) %>% # computing most relevant date using start date and fall back to report date
    dplyr::left_join(. , location,  c( "eventlocation_id" = "id_location")) %>% # merging with locationn
    dplyr::left_join(. , region,  c( "region_id_location" = "region_id")) %>% # merging with region
    dplyr::left_join(. , district,  c( "district_id_location" = "district_id")) %>% 
    tidyr::replace_na(list(region_name = "Missing Region", district_name = "Missing District"))
  
  # merging event data with eventPart data and count number of resulting cases and ep per event
  eventPartEvent = eventParticipant %>%
      dplyr::left_join(. , event, c( "event_id_eventpart" = "id_event") ) %>%  # merging eventParticipant with event
      dplyr::group_by(. , event_id_eventpart) %>% 
      dplyr::summarise(eventPart_sum = n(),
                       resulting_case_sum = sum(!is.na(resultingcase_id_eventpart) )
                       ) %>% # counting number of ep and resulting cases per event
    dplyr::left_join(., event, c("event_id_eventpart" = "id_event") )  # merging with event
  
  return(event_data = eventPartEvent)  
}
save(eventExport, file = "eventExport.R")
eventData = eventExport(sormas_db, fromDate = fromDate, toDate = toDate)   
### end of event export

## 
# 2by2 function table. This method takes a dataframe and any user specified 2 columns and returned a 2x2 table -----
twoByTwoTablefunction = function(data,Var1, Var2, spread=FALSE, Proportion = FALSE)
{
  data = data[,colnames(data) %in% c(Var1,Var2 )]
  colnames(data) = c("Var1", "Var2")
  data = data %>%
    dplyr::mutate(across(everything(), as.character))
  temp = data %>% 
    dplyr::group_by(Var1, Var2 )  %>% 
    dplyr::summarise( n = n(), sort = TRUE) %>%
    dplyr::select(-sort) 
  if(spread == TRUE){
    if(Proportion == TRUE){
      ret = temp %>%
        dplyr::mutate(Prop = round(n/sum(n)*100 , 2) ) %>%
        dplyr::select(-n) %>%
        tidyr::spread(Var2, Prop)
      #ret[is.na(ret)] = 0  or  ret2 = ret %>% replace(is.na(.), 0) # It is not needed to replace NA with 0
    }else{ 
      ret =  temp %>%
        tidyr::spread(Var2, n)
      #ret[is.na(ret)] = 0
    }
  } else{
    if(Proportion == TRUE){
      ret = temp %>%
        dplyr::mutate(Prop = round(n/sum(n)*100 , 2) ) %>%
        dplyr::select(-n)
    } else {
      ret = temp 
    }
  }
  ret = as.data.frame(ret)
  return(ret)
}
save(twoByTwoTablefunction, file = "twoByTwoTablefunction.R")
#twoByTwoTablefunction(data = eventData, Var1 = "eventstatus", Var2 = "eventmanagementstatus", spread = FALSE, Proportion = FALSE)
## end of twoByTwoTablefunction

## Adding pie chart to events
# pieChartPlot takes a character vector and  plot a pie chart
pieChartPlot = function(variable){
  temp = data.frame(table(variable))
  labels = as.character(temp$variable)
  values = temp$Freq
  fig = plot_ly(type='pie', labels=labels, values=values, 
                textinfo='label+percent',
                insidetextorientation='radial') 
  return(fig)
}
save(pieChartPlot, file = "pieChartPlot.R")
pieChartPlot(variable = eventData$eventmanagementstatus)
## end of pie chart

## Event barplot by jurisdiction ----
barplotEventStatusByJurisdiction  = function(data, Var1, count = TRUE){
  # This function depends on twoByTwoTablefunction function and produce the barplot of events by status
  # Var1 and Var2 are any 2 categorical variables, 
  # for now Var2 must be event status, this function can be generalised to take any other varaible
  countData = twoByTwoTablefunction(data = data, Var1 = Var1, Var2 = "eventstatus", spread = FALSE, Proportion = FALSE)
  mypalett = c("CLUSTER" = "#f70707","EVENT" = "#ffa500", "SIGNAL" = "#ffff00", "SCREENING" = "#706c67")
  fig = ggplot(data=countData, aes(x=Var2, y=n, fill=Var1)) 
  if(count==FALSE){
    fig =  fig + geom_bar(stat="identity", position="fill") + #, position=position_dodge()) to place bars side by side
      xlab(NULL) + ylab("Proportion") 
  } else {
    fig =  fig + geom_bar(stat="identity") +
      xlab(NULL) + ylab("Count") 
  }
  fig = fig + 
    labs(fill = NULL) +
    scale_fill_manual(values=mypalett) +
    theme_classic()
  fig = ggplotly(fig)  
  return(fig)
}
save(barplotEventStatusByJurisdiction, file = "barplotEventStatusByJurisdiction.R")
# end of event bar plot 

# table count of event by jurisdictiin and other varibales -----
# region must not have NA for this method to work
factorLevelCountEvent = function(data,  rowName){ #takes a data and rerun the counts of the variables in the function
  Name = rowName
  Total = nrow(data)
  nlast24hrs = nrow(data[data$reportdatetime_event %in% c(Sys.Date()-1, Sys.Date()), ])
  eventinvestigationstatusTotal = data.frame(t(as.matrix(  table( addNA(factor(data$eventinvestigationstatus,levels = c("ONGOING", "PENDING", "DISCARDED", "DONE") ), ifany = FALSE) ) )))
  eventstatusTotal = data.frame(t(as.matrix(  table( addNA(factor(data$eventstatus,levels = c("CLUSTER", "EVENT", "SCREENING", "SIGNAL") ), ifany = FALSE) ) )))
  typeofplaceTotal = data.frame(t(as.matrix(  table( addNA(factor(data$typeofplace_event,
                                                                  levels = c("FACILITY", "FESTIVITIES", "HOME", "MEANS_OF_TRANSPORT", "PUBLIC_PLACES","SCATTERED") ), ifany = FALSE) ) )))
  archivedTotal = data.frame(t(as.matrix(  table( addNA(factor(data$archived_event,levels = c("TRUE", "FALSE") ), ifany = FALSE) ) )))
  nosocomialTotal = data.frame(t(as.matrix(  table( addNA(factor(data$nosocomial_event,levels = c("YES", "NO") ), ifany = FALSE) ) )))
  srctypeTotal = data.frame(t(as.matrix(  table( addNA(factor(data$srctype_event,levels = c("HOTLINE_PERSON", "INSTITUTIONAL_PARTNER", "MEDIA_NEWS", "NOT_APPLICABLE") ), ifany = FALSE) ) )))
  risklevelTotal = data.frame(t(as.matrix(  table( addNA(factor(data$risklevel_event,levels = c("LOW", "HIGH", "MODERATE") ), ifany = FALSE) ) )))
  managementstatusTotal = data.frame(t(as.matrix(  table( addNA(factor(data$eventmanagementstatus,levels = c("ONGOING", "PENDING", "DONE", "CLOSED") ), ifany = FALSE) ) )))
  globalCount = data.frame(Name,Total, nlast24hrs, eventinvestigationstatusTotal, eventstatusTotal,managementstatusTotal, typeofplaceTotal, nosocomialTotal, srctypeTotal,risklevelTotal, archivedTotal)
  return(globalCount)
}
save(factorLevelCountEvent, file = "factorLevelCountEvent.R")

# cuntbyRegionDistrictEvent takes  eventData and return the count by region or district and certain event varaibles defined in the  factorLevelCountEvent function
# this function depends on factorLevelCountEvent
cuntbyRegionDistrictEvent = function(data, byRegion = TRUE){  # depends on factorLevelCountEvent function
  rowTtotal = factorLevelCountEvent(data = data, rowName = "Global")  #first row on table containing total counts for all regions
  if(byRegion == FALSE){
    districtName = unique(data$district_name) 
    for(i in districtName){
      tempData = data[data$district_name == i,]
      rowTtotal = rbind(rowTtotal, factorLevelCountEvent(data = tempData, rowName = i))
    }
  }else{
    regionName = unique(data$region_name)
    for(i in regionName){
      tempData = data[data$region_name == i,]
      rowTtotal = rbind(rowTtotal, factorLevelCountEvent(data = tempData, rowName = i))
    }
  }
  countByJurisdictionVaribles = rowTtotal[order(rowTtotal$Total, decreasing = T), ]
  countByJurisdictionVaribles = countByJurisdictionVaribles %>% 
    dplyr::select(-NA..1, -NA..7) %>% #dropping missing event status and archived status since it is never missing
    dplyr::rename(Total_last24hrs  = nlast24hrs, ONGOING_IS = ONGOING, PENDING_IS = PENDING,          
                  DISCARDED_IS = DISCARDED, DONE_IS = DONE, MISSING_IS = NA., CLUSTER_S = CLUSTER, EVENT_S = EVENT,              
                  SCREENING_S=SCREENING, SIGNAL_S =SIGNAL, ONGOING_MS = ONGOING.1, PENDING_MS = PENDING.1,           
                  DONE_MS = DONE.1, CLOSED_MS = CLOSED, MISSING_MS = NA..2, FACILITY_TP = FACILITY, FESTIVITIES_TP = FESTIVITIES,         
                  HOME_TP = HOME, TRANSPORT_TP = MEANS_OF_TRANSPORT, PUBLIC_TP = PUBLIC_PLACES, SCATTERED_TP = SCATTERED, MISSING_TP = NA..3,               
                  YES_NOSOCOMIAL = YES, NO_NOSOCOMIAL= NO, MISSING_NOSOCOMIAL = NA..4,  HOTLINE_PERSON_ST = HOTLINE_PERSON,
                  INSTITUTIONAL_PARTNER_ST = INSTITUTIONAL_PARTNER, MEDIA_NEWS_ST = MEDIA_NEWS, NOT_APPLICABLE_ST = NOT_APPLICABLE,
                  MISSING_ST = NA..5, LOW_RISK = LOW, HIGH_RISK = HIGH, MODERATE_RISK = MODERATE, MISSING_RISK = NA..6,
                  ARCHIVED = TRUE.,  UNARCHIVED = FALSE.)  
  return(countByJurisdictionVaribles)
}
#eventByJurisdiction = cuntbyRegionDistrictEvent(data = eventData, byRegion = FALSE) 
save(cuntbyRegionDistrictEvent, file = "cuntbyRegionDistrictEvent.R")

## event_variable_category_maper: adding a mapper to map event varaibles to categories
event_variable_category_maper = function(cuntbyRegionTableEvent){
  # Creating a dataframe to map categories of events to their varaibles, This is needed for filtering later
  # This function should be updated each time new variables are added in cuntbyRegionDistrictEvent
  event_variable = c("Name","Total", "Total_last24hrs", "Investigation status", "Investigation status", "Investigation status", "Investigation status", "Investigation status",
                     "Event status", "Event status", "Event status", "Event status",
                     "Management status", "Management status", "Management status", "Management status", "Management status",
                     "Type of place", "Type of place","Type of place","Type of place","Type of place","Type of place","Type of place", 
                     "Nosocomial", "Nosocomial", "Nosocomial", 
                     "Source type", "Source type", "Source type", "Source type", "Source type",
                     "Risk level", "Risk level", "Risk level", "Risk level",
                     "Archive status" , "Archive status")
  event_variable_category = data.frame(event_variable, category = colnames(cuntbyRegionTableEvent))
  return(event_variable_category)
}
#event_variable_data = event_variable_category_maper(cuntbyRegionTableEvent = cuntbyRegionTableEvent)
save(event_variable_category_maper, file = "event_variable_category_maper.R")



# importing data from db in R with non sensitive varaibles to be sued for case analysis-----
####  OLD IMPORTFUNCTINS ##### ----
ImportingUnformatedDataFromDB_2020 = function(DB_USER, DB_NAME, DB_PASS, DB_HOST, DB_PORT)
{
  # connecting to db
  #con = dbConnect(PostgreSQL(), user="sormas_user", dbname="sormas", password = "password", host="127.0.0.1", port="5432")
  con = dbConnect(PostgreSQL(), user=DB_USER,  dbname=DB_NAME, password = DB_PASS, host=DB_HOST, port=DB_PORT)

  # reading unique cases based on all varaibles.
  # use  where deleted = FALSE  and caseclassification != 'NO_CASE'  in case you want to eliminate not a cases
  case = dbGetQuery(con,"select distinct id, disease, reportdate, creationdate, person_id,region_id,district_id, caseclassification, epidnumber, symptoms_id, healthfacility_id,
                         outcome,caseorigin,quarantine
                         from public.cases
                         where deleted = FALSE and caseclassification != 'NO_CASE'
                        ")

  ## reading contact data ###
  # excluded contacts without a source case. Including them will mess up stuffs when merging contdacts and case table
  contact = dbGetQuery(con,"select distinct id,caze_id,district_id, region_id, person_id,reportdatetime, lastcontactdate, disease,
              contactclassification,contactproximity,resultingcase_id, followupstatus, followupuntil, contactstatus
                         from public.contact
                         where deleted = FALSE and caze_id IS NOT NULL
                        ")


  ### reading person data  ###
  person = dbGetQuery(con,"select distinct id, sex, occupationtype, presentcondition, birthdate_dd, birthdate_mm, birthdate_yyyy
                         from public.person
                         ")

  # reading region
  region = dbGetQuery(con,"select distinct id, name
                         from public.region
                         where archived = FALSE
                         ")

  #loading district
  district = dbGetQuery(con,"select distinct id, name
                         from public.district
                         where archived = FALSE
                         ")

  #disconnect from db
  dbDisconnect(con)

  ## converting all date formats from POSIXct to date
  case$reportdate = dateTimeToDate(case$reportdate)
  case$creationdate = dateTimeToDate(case$creationdate)

  #
  contact$reportdatetime = dateTimeToDate(contact$reportdatetime)
  contact$lastcontactdate = dateTimeToDate(contact$lastcontactdate)
  contact$followupuntil = dateTimeToDate(contact$followupuntil)

  case = case %>% rename(case_id = id)
  #
  person = person %>% rename(person_id = id)
  #
  region = region %>%
    rename(region_id = id, region_name = name)
  #
  district = district %>%
    rename(district_id = id, district_name = name)

  return(list(case = case, contact = contact, person = person, region = region, district = district))
}

ImportingUnformatedDataFromDB_210503 = function(DB_USER, DB_NAME, DB_PASS, DB_HOST, DB_PORT)
{ 
  # connecting to db
  #con = dbConnect(PostgreSQL(), user="sormas_user", dbname="sormas", password = "password", host="127.0.0.1", port="5432")
  con = dbConnect(PostgreSQL(), user=DB_USER,  dbname=DB_NAME, password = DB_PASS, host=DB_HOST, port=DB_PORT)
  
  # reading unique cases based on all varaibles. 
  # use  where deleted = FALSE  and caseclassification != 'NO_CASE'  in case you want to eliminate not a cases
  case = dbGetQuery(con,"select distinct id, disease, reportdate, creationdate, person_id,region_id,district_id, caseclassification, epidnumber, symptoms_id, healthfacility_id,
                         outcome,caseorigin,quarantine
                         from public.cases
                         where deleted = FALSE and caseclassification != 'NO_CASE'
                        ")  
  
  ## reading contact data ###
  # excluded contacts without a source case. Including them will mess up stuffs when merging contdacts and case table
  contact = dbGetQuery(con,"select distinct id,caze_id,district_id, region_id, person_id,reportdatetime, lastcontactdate, disease,
              contactclassification,contactproximity,resultingcase_id, followupstatus, followupuntil, contactstatus
                         from public.contact
                         where deleted = FALSE and caze_id IS NOT NULL
                        ")
  
  
  ### reading person data  ###
  person = dbGetQuery(con,"select distinct id, sex, occupationtype, presentcondition, birthdate_dd, birthdate_mm, birthdate_yyyy
                         from public.person
                         ") 
  
  # reading region
  region = dbGetQuery(con,"select distinct id, name
                         from public.region
                         where archived = FALSE
                         ") 
  
  #loading district
  district = dbGetQuery(con,"select distinct id, name
                         from public.district
                         where archived = FALSE
                         ")
  # reading event
  event = dbGetQuery(con,"select distinct id, reportdatetime, startdate, enddate, eventstatus,disease, typeofplace, srctype, eventlocation_id
                         from public.events
                         where deleted = FALSE and eventstatus != 'DROPPED' 
                        ")
  
  ## reading event participants
  eventParticipant = dbGetQuery(con,"select distinct id, event_id, person_id, resultingcase_id
                         from public.eventParticipant
                         where deleted = FALSE
                         ")
  
  location = dbGetQuery(con,"select id, region_id, district_id
                         from public.location
                         ")
  
  #disconnect from db
  dbDisconnect(con)
  
  ## converting all date formats from POSIXct to date
  case$reportdate = dateTimeToDate(case$reportdate)
  case$creationdate = dateTimeToDate(case$creationdate)
  
  #contact
  contact$reportdatetime = dateTimeToDate(contact$reportdatetime)
  contact$lastcontactdate = dateTimeToDate(contact$lastcontactdate)
  contact$followupuntil = dateTimeToDate(contact$followupuntil)
  #event
  event$reportdatetime = dateTimeToDate(event$reportdatetime)
  event$startdate = dateTimeToDate(event$startdate)
  event$enddate = dateTimeToDate(event$enddate)
  
  #renaming ids
  case = case %>% dplyr::rename(case_id = id)
  person = person %>% dplyr::rename(person_id = id)
  event = event %>% dplyr::rename(event_id = id, startdateEvent = startdate, enddateEvent = enddate, diseaseEvent = disease )
  eventParticipant = eventParticipant  %>% dplyr::rename(eventParticipant_id = id)
  location = location  %>% dplyr::rename(location_id = id)
  
  region = region %>%
    dplyr::rename(region_id = id, region_name = name)
  district = district %>%
    dplyr::rename(district_id = id, district_name = name)
  
  return(list(case = case, contact = contact, person = person, region = region, district = district, event = event, eventParticipant=eventParticipant, location=location))
}

ImportingUnformatedDataFromDB = function(sormas_db, fromDate, toDate)
{ 
  # connecting to db
  #con = dbConnect(PostgreSQL(), user="sormas_user", dbname="sormas", password = "password", host="127.0.0.1", port="5432")
  # con = dbConnect(PostgreSQL(), user=DB_USER,  dbname=DB_NAME, password = DB_PASS, host=DB_HOST, port=DB_PORT)
  
  # reading unique cases based on all varaibles. 
  # use  where deleted = FALSE  and caseclassification != 'NO_CASE'  in case you want to eliminate not a cases
  queryCase <- paste0("SELECT  DISTINCT id, disease, reportdate, creationdate, person_id,region_id,district_id, caseclassification, epidnumber, symptoms_id, healthfacility_id,
                         outcome,caseorigin,quarantine
                          FROM public.cases 
                          WHERE deleted = FALSE and caseclassification != 'NO_CASE' and reportdate between '", fromDate, "' and '", toDate, "' ")
  case = dbGetQuery(sormas_db,queryCase)
  
  # 
  # case = dbGetQuery(con,"select distinct id, disease, reportdate, creationdate, person_id,region_id,district_id, caseclassification, epidnumber, symptoms_id, healthfacility_id,
  #                        outcome,caseorigin,quarantine
  #                        from public.cases
  #                        where deleted = FALSE and caseclassification != 'NO_CASE'
  #                       ")  
  
  ## reading contact data ###
  # excluded contacts without a source case. Including them will mess up stuffs when merging contdacts and case table
  
  
  queryContact <- paste0("SELECT  DISTINCT id,caze_id,district_id, region_id, person_id,reportdatetime, lastcontactdate, disease,
              contactclassification,contactproximity,resultingcase_id, followupstatus, followupuntil, contactstatus
                          FROM public.contact
                          WHERE deleted = FALSE and caze_id IS NOT NULL and reportdatetime between '", fromDate, "' and '", toDate, "' ")
  contact = dbGetQuery(sormas_db,queryContact)
  
  # 
  # contact = dbGetQuery(sormas_db,"select distinct id,caze_id,district_id, region_id, person_id,reportdatetime, lastcontactdate, disease,
  #             contactclassification,contactproximity,resultingcase_id, followupstatus, followupuntil, contactstatus
  #                        from public.contact
  #                        where deleted = FALSE and caze_id IS NOT NULL
  #                       ")
  
  
  ### reading person data  ###
  person = dbGetQuery(sormas_db,"select distinct id, sex, occupationtype, presentcondition, birthdate_dd, birthdate_mm, birthdate_yyyy
                         from public.person
                         ") 
  
  # reading region
  region = dbGetQuery(sormas_db,"select distinct id, name
                         from public.region
                         where archived = FALSE
                         ") 
  
  #loading district
  district = dbGetQuery(sormas_db,"select distinct id, name
                         from public.district
                         where archived = FALSE
                         ")
  # reading event
  queryEvent <- paste0("SELECT  DISTINCT id, reportdatetime, startdate, enddate, eventstatus,disease, typeofplace, srctype, eventlocation_id
                          FROM public.events
                          WHERE deleted = FALSE and eventstatus != 'DROPPED' and reportdatetime between '", fromDate, "' and '", toDate, "' ")
  event = dbGetQuery(sormas_db,queryEvent)
  
  
  # 
  # event = dbGetQuery(sormas_db,"select distinct id, reportdatetime, startdate, enddate, eventstatus,disease, typeofplace, srctype, eventlocation_id
  #                        from public.events
  #                        where deleted = FALSE and eventstatus != 'DROPPED' 
  #                       ")
  
  ## reading event participants
  eventParticipant = dbGetQuery(sormas_db,"select distinct id, event_id, person_id, resultingcase_id
                         from public.eventParticipant
                         where deleted = FALSE
                         ")
  
  location = dbGetQuery(sormas_db,"select id, region_id, district_id
                         from public.location
                         ")
  
  #disconnect from db
  # dbDisconnect(con)
  
  ## converting all date formats from POSIXct to date
  case$reportdate = dateTimeToDate(case$reportdate)
  case$creationdate = dateTimeToDate(case$creationdate)
  
  #contact
  contact$reportdatetime = dateTimeToDate(contact$reportdatetime)
  contact$lastcontactdate = dateTimeToDate(contact$lastcontactdate)
  contact$followupuntil = dateTimeToDate(contact$followupuntil)
  #event
  event$reportdatetime = dateTimeToDate(event$reportdatetime)
  event$startdate = dateTimeToDate(event$startdate)
  event$enddate = dateTimeToDate(event$enddate)
  
  #renaming ids
  case = case %>% dplyr::rename(case_id = id)
  person = person %>% dplyr::rename(person_id = id)
  event = event %>% dplyr::rename(event_id = id, startdateEvent = startdate, enddateEvent = enddate, diseaseEvent = disease )
  eventParticipant = eventParticipant  %>% dplyr::rename(eventParticipant_id = id)
  location = location  %>% dplyr::rename(location_id = id)
  
  region = region %>%
    dplyr::rename(region_id = id, region_name = name)
  district = district %>%
    dplyr::rename(district_id = id, district_name = name)
  
  return(list(case = case, contact = contact, person = person, region = region, district = district, event = event, eventParticipant=eventParticipant, location=location))
}
save(ImportingUnformatedDataFromDB, file = "ImportingUnformatedDataFromDB.R")

##

contIdsForSingleChainOld = function(elist, id)
{
  elistNew = elist[, colnames(elist) %in% c("from","to", 'id')]
  fromId = id
  datSel = elistNew[elistNew$from %in% fromId,  ]
  #datRetained = elistNew[!(elistNew$from %in% fromId),  ]
  fromId = datSel$to
  repeat{ 
    tempSel = elistNew[elistNew$from %in% fromId,  ] # one option to optimise this can be to select from remaining data set not from all elist each time
    fromId = tempSel$to
    if(dim(tempSel)[1] != 0)
    {
      datSel = rbind(datSel,tempSel)
    } else{
      break
    }
  }
  contId = datSel$id
  return(contId)
}

contIdsForSingleChain = function(elist, uuid_node)
{
  fromId = unique(elist[ elist$from_uuid_person %in% c(uuid_node), ]$from)
  elistNew = elist[, colnames(elist) %in% c("from","to", 'id')]
  datSel = elistNew[elistNew$from %in% fromId,  ]
  #datRetained = elistNew[!(elistNew$from %in% fromId),  ]
  fromId = datSel$to
  repeat{ 
    tempSel = elistNew[elistNew$from %in% fromId,  ] # one option to optimise this can be to select from remaining data set not from all elist each time
    fromId = tempSel$to
    if(dim(tempSel)[1] != 0)
    {
      datSel = rbind(datSel,tempSel)
    } else{
      break
    }
  }
  contId = datSel$id
  return(contId)
}
save(contIdsForSingleChain, file = "contIdsForSingleChain.R")
##
pyramidPlotFunction = function(data)
{
  data$sex = as.character(data$sex)
  # deleting casas with missing velues fro age and gender
  data$sex[data$sex == ""] = NA # assignning "" as missing
  pyramidData = data %>% drop_na(sex,age)   # dropping missing values of age and sex
  pyramidData = pyramidData[pyramidData$sex %in% c("MALE", "FEMALE"),] # retain mele and female only fro now
  ## cut the age variable into age groups with 5-year intervals
  pyramidData$ageCat = cut(pyramidData$age, breaks = seq(0, 100, 5), right = FALSE) 
  pyramidData$population = 1
  ## aggregate the data by gender and age group
  pyramidData <- aggregate(formula = population ~ sex + ageCat, data = pyramidData, FUN = sum)
  
  
  
  ## sort data by first by gender, then by age groups
  pyramidData <- with(pyramidData, pyramidData[order(sex,ageCat),])
  pyramidData$caseCount <- ifelse(pyramidData$sex == "MALE", -1*pyramidData$population, pyramidData$population)
  
  pyramitPlot = ggplot(data = pyramidData, 
                       mapping = aes(x = ageCat, y = caseCount, fill = sex)) + 
    geom_col() +
    coord_flip() +
    scale_y_continuous(labels = abs, limits = max(pyramidData$population) * c(-1,1)) +
    labs(y = "Case count", x = "Age group")
  
  #print(pyramitPlot)
  return(pyramitPlot)
}
save(pyramidPlotFunction, file = "pyramidPlotFunction.R")
##
## Time series plotting functions for cases count
timeSeriesPlotDay = function(data, cum){
  #defining legends
  f <- list(
    family = "Courier New, monospace",
    size = 18,
    color = "#7f7f7f"
  )
  y <- list(
    title = "Number of cases",
    titlefont = f
  )
  x <- list(
    title = "Date of report",
    titlefont = f
  )
  
  if (cum == T)
  {
    data$cumulative = cumsum(data$total)
    fig = data %>% plot_ly(x = ~reportdate, y = ~ cumulative, type="scatter",mode = "lines+markers") %>%
      layout(xaxis = x, yaxis = y)
  } else{
    fig =data %>% plot_ly(x = ~reportdate, y = ~ total, type="scatter",mode = "lines+markers") %>%
      layout(xaxis = x, yaxis = y)
  }
  return(fig)
}
save(timeSeriesPlotDay, file = "timeSeriesPlotDay.R")
##
timeSeriesPlotWeek = function(data){
  f <- list(
    family = "Courier New, monospace",
    size = 18,
    color = "#7f7f7f"
  )
  y <- list(
    title = "Number of cases",
    titlefont = f
  )
  x <- list(
    title = "Week of report",
    titlefont = f
  )
  fig = plot_ly(x =data$reportweek, y = data$total, type="scatter",mode = "markers") %>%
    add_lines(linetype = ~ data$reportyear)
  fig <- fig %>% layout(xaxis = x, yaxis = y)
  return(fig)
}
save(timeSeriesPlotWeek, file = "timeSeriesPlotWeek.R")
##
timeSeriesPlotMonth = function(data){
  f <- list(
    family = "Courier New, monospace",
    size = 18,
    color = "#7f7f7f"
  )
  y <- list(
    title = "Number of cases",
    titlefont = f
  )
  x <- list(
    title = "Month of report",
    titlefont = f
  )
  fig = plot_ly(x =data$reportmonth, y = data$total, type="scatter",mode = "markers") %>%
    add_lines(linetype = ~ data$reportyear)
  fig <- fig %>% layout(xaxis = x, yaxis = y)
  return(fig)
}
save(timeSeriesPlotMonth, file = "timeSeriesPlotMonth.R")
## time series by region 
timeSeriesPlotDayRegion = function(data, cum){
  f <- list(
    family = "Courier New, monospace",
    size = 18,
    color = "#7f7f7f"
  )
  x <- list(
    title = "Date of report",
    titlefont = f
  )
  y <- list(
    title = "Number of cases",
    titlefont = f
  )
  if (cum == T)
  {
    temp = data.frame()
    regionUnique = unique(data$region_name)
    for(i in regionUnique)
    {
      temp2 = data[data$region_name == i,]
      temp2 = temp2[order(temp2$reportdate),]
      temp2$cumulative = cumsum(temp2$total)
      temp = rbind(temp, temp2)
    }
    fig = temp %>% plot_ly(x = ~reportdate, y = ~ cumulative, type="scatter",mode = "none") %>% 
      add_lines(linetype = ~ region_name)  %>% layout(xaxis = x, yaxis = y)
    
  } else{
    fig = data %>% plot_ly(x = ~ reportdate, y = ~ total, type="scatter",mode = "none") %>%
      add_lines(linetype = ~ region_name)  %>% layout(xaxis = x, yaxis = y)
  }
  return(fig)
}
save(timeSeriesPlotDayRegion, file = "timeSeriesPlotDayRegion.R")
## epidemic curve by date
epicurveDate = function(data){
  #sorting data by case clessification nefore plotting to have the legends right
  data$caseclassification  <- reorder.factor(data$caseclassification, new.order=c("CONFIRMED", "PROBABLE",  "SUSPECT", "NOT_CLASSIFIED"))                  
  data = data[order(data$caseclassification), ]
  #defining legends
  f <- list(
    family = "Courier New, monospace",
    size = 18,
    color = "#7f7f7f"
  )
  y <- list(
    title = "Number of cases",
    titlefont = f
  )
  x <- list(
    title = "Date of report",
    titlefont = f
  )
  fig = data %>%
    plot_ly(x = ~reportdate, y = ~total, color = ~factor(caseclassification), colors = c("#f70707", "#ffa500", "#ffff00", "#706c67" )) %>%
    add_bars() %>%
    layout(barmode = "stack")
  fig <- fig %>% layout(xaxis = x, yaxis = y)
  return(fig)
}
save(epicurveDate, file = "epicurveDate.R")
##
epicurveMonth = function(data){
  # initial formatting
  data$onset <- as.Date(data$reportdate, format="%d/%m/%Y")    # convert to Date class
  data$onset <- strftime(data$onset, format="%Y/%m")      # convert to Year-month
  data$onset <- paste(data$onset, "/01", sep = "")        # add arbitrary day on to end to make compatible w/ ggplot2
  
  # aggregate by month
  onset_counts <- aggregate(data$onset, by = list(date = data$onset, classification = data$caseclassification), length) # aggregate by month
  onset_counts$date = as.Date(onset_counts$date, format = "%Y/%m/%d") # covert to Date class
  onset_counts$classification = factor(onset_counts$classification, levels = c("NOT_CLASSIFIED", "SUSPECT", "PROBABLE","CONFIRMED")) 
  ## plotting
  mypalett = c("CONFIRMED" = "#f70707","PROBABLE" = "#ffa500", "SUSPECT" = "#ffff00", "NOT_CLASSIFIED" = "#706c67")
  # p4 =  ggplot(onset_counts, aes(x=date, y=x)) + geom_bar(stat="identity", aes(fill = factor(classification, levels = c("NOT_CLASSIFIED", "SUSPECT", "PROBABLE","CONFIRMED")) )) +
  p4 =  ggplot(onset_counts, aes(x=date, y=x)) + geom_bar(stat="identity", aes(fill = classification )) +
    theme(axis.text.x = element_text(angle=90, hjust = 1, vjust = 1)) +
    ylab("Number of cases") + xlab("Date of report") + scale_x_date(breaks="month", labels=date_format("%Y-%m"))+
    theme(legend.position="bottom", legend.direction="horizontal",legend.title = element_blank())+
    scale_fill_manual(values=mypalett)
  return(ggplotly(p4))
  
}
save(epicurveMonth, file = "epicurveMonth.R")
##
### map of incidence ot case count by region
regionMapPlot = function(data , lnd)
{
  casePerRegion =  data.frame(table(as.factor(data$region_name))) # calculatinf number of case by region
  colnames(casePerRegion) = c("StateName", "Number of cases")
  ## joining sahp file data with surveillance data 
  lnd@data <- left_join(lnd@data, casePerRegion, by = "StateName")   #c('name' = 'Borough')
  p = qtm(shp = lnd, fill = "Number of cases", fill.palette = "Reds", text = "StateName", 
          text.size = 1, style = "white", format = "World") + tm_compass(type="8star", position=c("right", "bottom")) # fill.palette = "-Blues" to qhow in reverse order, type = "4star", "8star", "radar", "rose"
  return(p)
  
}
save(regionMapPlot, file = "regionMapPlot.R")

## function to plot district shapes based on case count or incidence
districtMapPlot = function(data , districtShapes){
  temp =  data.frame(table(as.factor(data$district_name))) # calculatinf number of case by region
  colnames(temp) = c("LGAName", "Number of cases")
  ## joining sahp file data with surveillance data 
  districtShapes@data <- left_join(districtShapes@data, temp, by = "LGAName")   #c('name' = 'Borough')
  p =  qtm(shp = districtShapes, fill = "Number of cases", fill.palette = "Reds", 
           text.size = 1, style = "white", format = "World") + tm_compass(type="8star", position=c("right", "bottom")) 
  # fill.palette = "-Blues" to qhow in reverse order, type = "4star", "8star", "radar", "rose"
  return(p)
  
}
save(districtMapPlot, file = "districtMapPlot.R")
###
## Function to plot Rt
RtPlot = function(mean_si, std_si, method="parametric_si", burnin = 1000, dateSumCase, si_data, rsi="all") # rsi = "all","R", "SI"
{
  if(method == "parametric_si")
  {
    res <- estimate_R(dateSumCase,method="parametric_si", config = make_config(list(mean_si = mean_si, std_si = std_si))) 
  }
  if(method == "si_from_data")
  {
    MCMC_seed <- 1
    overall_seed <- 2
    mcmc_control <- make_mcmc_control(seed = MCMC_seed, 
                                      burnin = burnin)
    dist <- "G" # fitting a Gamma dsitribution for the SI
    config <- make_config(list(si_parametric_distr = dist,
                               mcmc_control = mcmc_control,
                               seed = overall_seed, 
                               n1 = 50, 
                               n2 = 50))
    res <- estimate_R(dateSumCase,
                      method = "si_from_data",
                      si_data = si_data,
                      config = config)
  }
  return(plot(res, rsi, legend = T))
  # if(rsi !="all")
  # {
  #   return(plot(res, rsi = rsi, legend = T))
  # }else{
  #   return(plot(res, rsi = rso, legend = T))
  # }
  
  
}
save(RtPlot, file = "RtPlot.R")

##### Table of case count by regions and other case variables: classification, outcome, quarantine, etc
#functions here uses casePersonRegionDist as input data
### case count by region and one or more categorical variable
factorLevelCount = function(data,  rowName){ #takes a data and rerun the counts of the variables in the function
  Name = rowName
  Total = nrow(data)
  nlast24hrs = nrow(data[data$reportdate %in% c(Sys.Date()-1, Sys.Date()), ])
  caseByClassTotal = data.frame(t(as.matrix( table(factor(data$caseclassification, levels = c("CONFIRMED", "CONFIRMED_NO_SYMPTOMS", "CONFIRMED_UNKNOWN_SYMPTOMS",
                                                                                              "NOT_CLASSIFIED", "PROBABLE", "SUSPECT") ) ) )))
  caseByOutcomeTotal = data.frame(t(as.matrix( table(factor(data$outcome, levels = c("DECEASED","NO_OUTCOME","RECOVERED","UNKNOWN") )) )))
  caseByQuarantineTotal = data.frame(t(as.matrix( table(factor(data$quarantine, levels =  c("HOME","INSTITUTIONELL","NONE","OTHER","UNKNOWN") )) )))
  caseByOriginTotal = data.frame(t(as.matrix( table(factor(data$caseorigin, levels = c("IN_COUNTRY","POINT_OF_ENTRY"))) )))
  globalCount = data.frame(Name,Total, nlast24hrs, caseByClassTotal, caseByOutcomeTotal, caseByOriginTotal, caseByQuarantineTotal)
  return(globalCount)
}
save(factorLevelCount, file = "factorLevelCount.R")

# cuntbyRegionDistrictCase takes the data of cases and return the case count by region or district and certain case varaibles defined by factorLevelCount function
cuntbyRegionDistrictCase = function(data, byRegion = TRUE){  # depends on factorLevelCount function
  rowTtotal = factorLevelCount(data = data, rowName = "Global")  #forst row on table containing total counts for all regions
  
  if(byRegion == FALSE){
    districtName = unique(data$district_name) 
    for(i in districtName){
      tempData = data[data$district_name == i,]
      rowTtotal = rbind(rowTtotal, factorLevelCount(data = tempData, rowName = i))
    }
  }else{
    regionName = unique(data$region_name)
    for(i in regionName){
      tempData = data[data$region_name == i,]
      rowTtotal = rbind(rowTtotal, factorLevelCount(data = tempData, rowName = i) )
    }
    
  }
  
  countByRegionVaribles = rowTtotal[order(rowTtotal$Total, decreasing = T), ]
  countByRegionVaribles$CONFIRMED = countByRegionVaribles$CONFIRMED
  countByRegionVaribles = countByRegionVaribles %>%
    dplyr::mutate(CONFIRMED = CONFIRMED+ CONFIRMED_NO_SYMPTOMS+ CONFIRMED_UNKNOWN_SYMPTOMS ) %>%
    dplyr::select(-CONFIRMED_NO_SYMPTOMS, -CONFIRMED_UNKNOWN_SYMPTOMS  )
  countByRegionVaribles = countByRegionVaribles %>% 
    dplyr::rename(Total_last24hrs  = nlast24hrs, confirmed = CONFIRMED,  Unclassified = NOT_CLASSIFIED, Probable=PROBABLE, Suspected=SUSPECT,
                  Deseased =DECEASED, No_Outcome= NO_OUTCOME, Recovered = RECOVERED, UnK_Outcome =UNKNOWN, In_Country =IN_COUNTRY, Imported = POINT_OF_ENTRY,
                  Home_Q=HOME, Institutional_Q=INSTITUTIONELL, No_Q=NONE, Other_Q=OTHER, Unk_Q=UNKNOWN.1 )
  
  return(countByRegionVaribles)
}
save(cuntbyRegionDistrictCase, file = "cuntbyRegionDistrictCase.R")

# proportionByregion: this function takes the table of number of cases by retion returned by cuntbyRegion function and computes the proportion for each cell
proportionByregion = function(data){
  temp = data[, !(colnames(data) %in% c("Name"))] # excluding region/district name, and totals
  rowPercent = (temp[1,][-1])/(as.numeric(temp[1,][1])) * 100
  rowPercent = format(round(rowPercent, digits = 2), nsmall =2) # to always maintain 2 dp after rounding to 2dp
  n = nrow(temp)
  for(i in 2:n){
    res = (temp[i,][-1])/(as.numeric(temp[i,][1])) * 100
    res = format(round(res, digits = 2), nsmall =2) # to always maintain 2 dp after rounding to 2dp
    rowPercent = rbind(rowPercent, res)
  }
  temp1 = data[, (colnames(data) %in% c("Name", "Total"))] 
  percentage_by_region = cbind(temp1, rowPercent)
  return(percentage_by_region)
}
save(proportionByregion, file = "proportionByregion.R")




fixBirthDate = function(person){
  # cases with birth year set!!!
  birthYear = person[is.na(person$birthdate_yyyy) == F, ] 
  firstJan = rep(1, nrow(birthYear))
  # if only year is set, set birth date to 1st January
  birthYear$date_of_birth = 
    as.Date(
      with(
        birthYear,
        paste(birthdate_yyyy, firstJan, firstJan, sep = "-")
      )
    )
  
  # cases with no birth date set!!!
  noBirthYear = person[is.na(person$birthdate_yyyy) == T, ]
  # null birth year
  noBirthYear$date_of_birth = rep(NA,nrow(noBirthYear)) 
  
  
  person = rbind(birthYear, noBirthYear)
  return (person)
}
save(fixBirthDate, file = "fixBirthDate.R")

######## infectorInfecteeExport #############
# This function connects to the sormas db generate the data specified by issue https://github.com/hzi-braunschweig/SORMAS-Stats/issues/87

# DB_USER = "sormas_user"
# DB_PASS = "password"
# DB_HOST = "127.0.0.1"
# DB_PORT = "5432"
# DB_NAME= "sormas"
# 
# library(RPostgreSQL)
# library(DBI)
# library(lubridate)
# library(dplyr)
# sormas_db = dbConnect(PostgreSQL(), user=DB_USER,  dbname=DB_NAME, password = DB_PASS, host=DB_HOST, port=DB_PORT) # should be replaced when doin gpull request
#load("fixBirthDate.R") # to load this method

# infectorInfecteeExport require fixBirthDate.R, libraries mentioned above and connection to db (sormas_db), 
infectorInfecteeExport = function(sormas_db, fromDate, toDate){
  ## computing default time based on 90 days in the past if  not provided by user
  if(missing(fromDate) | missing(toDate)){
    fromDate = as.character(Sys.Date() - delay) 
    toDate = as.character(Sys.Date()) 
  }
  
  # loading tables from sormas db
  # load cases
  queryCase <- paste0("SELECT  DISTINCT uuid AS case_uuid, id AS case_id, disease, reportdate AS report_date_case, creationdate AS creation_date_case, person_id AS person_id_case,
    region_id AS region_id_case, district_id AS district_id_case, caseclassification AS case_classification, caseorigin AS case_origin, symptoms_id
                          FROM public.cases 
                          WHERE deleted = FALSE and caseclassification != 'NO_CASE' and reportdate between '", fromDate, "' and '", toDate, "' ")
  case = dbGetQuery(sormas_db,queryCase)
  # 
  # case = dbGetQuery(
  #   sormas_db,
  #   "SELECT uuid AS case_uuid, id AS case_id, disease, reportdate AS report_date_case, creationdate AS creation_date_case, person_id AS person_id_case,
  #   region_id AS region_id_case, district_id AS district_id_case, caseclassification AS case_classification, caseorigin AS case_origin, symptoms_id
  #   FROM cases
  #   WHERE deleted = FALSE and caseclassification != 'NO_CASE' "
  # )
  
  #load contacts
  queryContact <- paste0("SELECT uuid AS contact_uuid, id AS contact_id, caze_id AS case_id, district_id AS district_id_contact, region_id AS region_id_contact,
    person_id AS person_id_contact, reportdatetime AS report_date_contact, creationdate AS creation_date_contact, lastcontactdate AS lastcontact_date_contact,
    contactproximity AS contact_proximity, resultingcase_id, contactstatus, contactclassification
                          FROM public.contact
                          WHERE deleted = FALSE and caze_id IS NOT NULL and contactclassification = 'CONFIRMED' and reportdatetime between '", fromDate, "' and '", toDate, "' ")
  contact = dbGetQuery(sormas_db,queryContact)
  # 
  # contact = dbGetQuery(
  #   sormas_db,
  #   "SELECT uuid AS contact_uuid, id AS contact_id, caze_id AS case_id, district_id AS district_id_contact, region_id AS region_id_contact,
  #   person_id AS person_id_contact, reportdatetime AS report_date_contact, creationdate AS creation_date_contact, lastcontactdate AS lastcontact_date_contact,
  #   contactproximity AS contact_proximity, resultingcase_id, contactstatus, contactclassification
  #   FROM public.contact
  #   WHERE deleted = FALSE and caze_id IS NOT NULL and contactclassification = 'CONFIRMED'" 
  #   # no need to add "unconfirmed" and  "not a contact" because our focus is only on contacts that resulted to cases
  #   # only confirmed contacts are permitted to result to cases in app
  # )
  
  #loading symptom data
  symptoms = dbGetQuery(
    sormas_db,
    "SELECT id AS symptom_id, onsetdate AS onset_date
                         from public.symptoms"
  )
  
  # load person data
  person = dbGetQuery(
    sormas_db,
    "SELECT id AS person_id, sex, birthdate_dd, birthdate_mm, birthdate_yyyy
    FROM person"
  ) 
  
  # load region
  region = dbGetQuery(
    sormas_db,
    "SELECT id AS region_id, name AS region_name
    FROM public.region
    WHERE archived = FALSE"
  ) 
  
  # load district
  district = dbGetQuery(
    sormas_db,
    "SELECT id AS district_id, name AS district_name
    FROM district
    WHERE archived = FALSE"
  )
  
  ## clean-up tables 
  ## fixing date formats for case,  contacts, symptoms
  case = case %>%
    dplyr::mutate(report_date_case = as.Date(format(report_date_case, "%Y-%m-%d")),
                  creation_date_case = as.Date(format(creation_date_case, "%Y-%m-%d")))
  
  contact  = contact %>%
    dplyr::mutate(report_date_contact = as.Date(format(report_date_contact, "%Y-%m-%d")) ,
                  creation_date_contact = as.Date(format(creation_date_contact, "%Y-%m-%d")),
                  lastcontact_date_contact = as.Date(format(lastcontact_date_contact, "%Y-%m-%d")))
  
  symptoms = symptoms %>%
    dplyr::mutate(onset_date = as.Date(format(onset_date, "%Y-%m-%d")))
  
  # fixing birth date of person. 
  person = fixBirthDate(person) # This method assign the birhdate of the person as first January of the year of birth. Improvement will follow
  person = person %>%
    dplyr::select(person_id, sex, date_of_birth) #dropping unused varaibles
  
  ## Obtaining dataset to the exported
  # The primary dataset to begin with is the contact since the goel is to export the contact data
  
  # Merging tables
  ret = contact %>%
    dplyr::filter(resultingcase_id != "NA")  %>%  #dropping contacts that did not resulted to a case
    dplyr::left_join(., case, by="case_id") %>%  # Merging contact and case data by source case or infector case id
    dplyr::mutate(case_uuid_infector = case_uuid, case_id_infector= case_id,  disease_infector = disease,  report_date_infector = report_date_case,
                  creation_date_case_infector = creation_date_case,  person_id_case_infector = person_id_case, region_id_case_infector = region_id_case,
                  district_id_case_infector = district_id_case, case_classification_infector = case_classification, case_origin_infector = case_origin,
                  symptoms_id_infector = symptoms_id, .keep = "unused" ) %>% # renaming variables ny adding infector to them
    dplyr::left_join(., case, by = c( "resultingcase_id" = "case_id") ) %>%  # merging data with case table again by resulting case or infectee id
    dplyr::mutate(case_uuid_infectee = case_uuid,case_id_infectee = resultingcase_id,  disease_infectee = disease,  report_date_infectee = report_date_case,
                  creation_date_case_infectee = creation_date_case,  person_id_case_infectee = person_id_case, region_id_case_infectee = region_id_case,
                  district_id_case_infectee = district_id_case, case_classification_infectee = case_classification, case_origin_infectee = case_origin,
                  symptoms_id_infectee = symptoms_id, .keep = "unused" ) %>% # renaming variables ny adding infectee to them
    dplyr::select(., -c(person_id_contact, case_uuid_infector, contact_uuid, case_uuid_infectee, disease_infectee, case_origin_infectee, region_id_case_infectee, district_id_case_infectee)) %>%  # dropping duplicates or unimportant varibles
    dplyr::left_join(., person, by = c( "person_id_case_infector" = "person_id") ) %>% # merging with person table to get person details of  infector person
    dplyr::mutate(sex_infector = sex, date_of_birth_infector = date_of_birth, .keep = "unused") %>% # renaming
    dplyr::left_join(., person, by = c( "person_id_case_infectee" = "person_id") )  %>%  # merging with person table to get person details of  infectee person
    dplyr::mutate(sex_infectee = sex, date_of_birth_infectee = date_of_birth, .keep = "unused") %>% #renaming
    dplyr::left_join(., region, by = c("region_id_case_infector" = "region_id" )) %>% # merging with region table to get region details of  infector person
    dplyr::left_join(., district, by = c("district_id_case_infector" = "district_id" )) %>% # merging with district table to get district details of  infector person
    dplyr::mutate(region_infector = region_name, district_infector = district_name, .keep = "unused") %>% #renaming
    dplyr::left_join(., symptoms, by = c("symptoms_id_infector" = "symptom_id" )) %>% #  merging with symptom table to get symptoms details of  infector person
    dplyr::mutate(onset_date_infector = onset_date, .keep = "unused") %>%  #renaming
    dplyr::left_join(., symptoms, by = c("symptoms_id_infectee" = "symptom_id" )) %>%  # merging with symptom table to get symptoms details of  infectee person
    dplyr::mutate(onset_date_infectee = onset_date, .keep = "unused") %>% # renaming
    
    # computing derived variables 
    dplyr::mutate(age_at_report_infector = floor(as.numeric(report_date_infector - date_of_birth_infector)/365),
                  age_at_report_infectee = floor(as.numeric(report_date_infectee - date_of_birth_infectee)/365),
                  serial_interval = as.numeric(onset_date_infectee - onset_date_infector),
                  incubation_Period = as.numeric(onset_date_infectee - lastcontact_date_contact),
                  report_week_contact = lubridate::week(report_date_contact),
                  report_month_contact = lubridate::month(report_date_contact),
                  report_year_contact = lubridate::year(report_date_contact)
    ) %>%
    
    ## selecting variables to export
    dplyr::select(
      # contact varibles, rendering by time will be based on report date of contact
      contact_id, report_date_contact,  report_week_contact, report_month_contact, report_year_contact, contact_proximity,
      #infector variables, rendering by disease, region and district will be based on the infrestructure of the infector
      person_id_case_infector, case_id_infector, disease_infector, 
      report_date_infector, region_infector, district_infector, case_classification_infector, case_origin_infector, 
      sex_infector, onset_date_infector, age_at_report_infector,
      #infectiee variables
      person_id_case_infectee, case_id_infectee, report_date_infectee, case_classification_infectee, sex_infectee,
      onset_date_infectee, age_at_report_infectee,
      #other variables
      serial_interval, incubation_Period
    )
  return(ret)
}
save(infectorInfecteeExport, file = "infectorInfecteeExport.R")


# fixContactJurisdiction, method assign the jurisdiction contacts with mission region and district using that of their source cases
# if both jurisdictions are missing, then the contact is deleted
fixContactJurisdiction = function(contCase){
  temp1 = contCase[is.na(contCase$region_id_contact) == T,] # cotacts with missiing region_id for contact
  temp2 = contCase[is.na(contCase$region_id_contact) == F,]
  temp1 = temp1[is.na(temp1$region_id_case)==F,]  #dropping contacts with missiong region of source case. This should normally not happen but is does
  temp1$region_id_contact = temp1$region_id_case
  temp1$district_id_contact = temp1$district_id_case
  res = rbind(temp1, temp2)
  return(res)
}
save(fixContactJurisdiction, file = "fixContactJurisdiction.R")

###########  serialIntervalPlot ##############
serialIntervalPlot = function(infectorInfecteePair, distr = "Lognormal", minSi = NULL, maxSi = NULL){ 
  # distr = "eibull", "gamma", "lnorm", "norm"  
  # minSi and maxSi are the min and max user specified values of si to be used for the analysis
  # fiting normal, weibull, gamma, lnorm distributions to serial intervals 
  
  # filtering based on user specified min and max values of serial interval.
  if(any(is.null(c(minSi, maxSi)))) {
    minSi = min(infectorInfecteePair$serial_interval, na.rm = T)
    maxSi = max(infectorInfecteePair$serial_interval, na.rm = T)
  }
  siVector = c(minSi: maxSi)
  selData <- infectorInfecteePair %>%
    dplyr::filter(serial_interval != 'NA' & serial_interval %in% siVector)
  
  # Fitting user specified distributions to the data. Dafault distribution is lognormal 
  if(distr == "Normal"){
    fit  = selData %>%
      dplyr::pull(serial_interval) %>% # extracting si
      fitdistrplus::fitdist(data = ., distr = 'norm')  # fiting a normal distribution to the data
    
    # Estimating CI for mean and standard deviation by bootstrap method 
    #  1001 were used by default for bootstraping, Add a parameter for this at front end later if needed
    fit_boot <- summary(fitdistrplus::bootdist(fit))  
    
    # extracting estimates
    siEstmate = dplyr::bind_cols(data.frame(fit$estimate), data.frame(fit_boot$CI) ) # extracting estimates and CI as a data frame
    colnames(siEstmate) = c("Estimate", "Median", "2.5% CI", "97.5% CI")
    siEstmate = round(siEstmate, 2)
    
    #Plotting 
    siDistributionPlot = ggplot(data = selData) +
      geom_histogram(aes(x = serial_interval, y = ..density..), fill = '#dedede', colour = "black", binwidth = 1) +
      stat_function(fun = dnorm, args = list(mean = fit$estimate[[1]], sd = fit$estimate[[2]]), size = 0.8, linetype = 2) +
      #scale_x_continuous("Serial Interval (Days)", limits = c(minSi-2,maxSi+2), breaks = seq(minSi-2,maxSi+2, by =5), expand = c(0,0)) +
      scale_x_continuous("Serial interval (Days)", expand = c(0, 0), breaks = seq(min(selData$serial_interval), max(selData$serial_interval), by = 2))+
      scale_y_continuous("Proportion", expand = c(0,0)) +
      theme_classic() +
      theme(aspect.ratio = 1)
    ret = list(siEstmate = siEstmate, siDistributionPlot = siDistributionPlot)  # list object: table of estimates and image
    
  }
  
  # Fitting weibull distribution
  # presymptomatic pair (seriel interval <= 0) will be dropped
  if(distr == "Weibull"){
    fit  = selData %>%
      dplyr::filter(serial_interval > 0)  %>%  # weibull can not be used to describe a random varaible with negative or 0 values
      dplyr::pull(serial_interval) %>% # extracting si
      fitdistrplus::fitdist(data = ., distr = "weibull")  
    
    # Estimating CI for mean and standard deviation by bootstrap method 
    #  1001 were used by default for bootstraping, Add a parameter for this at front end later if needed
    fit_boot <- summary(fitdistrplus::bootdist(fit))  
    
    # extracting estimates
    siEstmate = dplyr::bind_cols(data.frame(fit$estimate), data.frame(fit_boot$CI) ) # extracting estimates and CI as a data frame
    colnames(siEstmate) = c("Estimate", "Bootstrap median", "2.5% percentile CI", "97.5% percentile CI")
    siEstmate = round(siEstmate, 2)
    
    #Plotting 
    siDistributionPlot = ggplot(data = selData) +
      geom_histogram(aes(x = serial_interval, y = ..density..), fill = '#dedede', colour = "black", binwidth = 1) +
      #geom_point(aes(x = serial_interval, y = dweibull(x= serial_interval, shape = fit$estimate[[1]], scale = fit$estimate[[2]])), size = 1) +
      stat_function(fun = dweibull, args = list(shape = fit$estimate[[1]], scale = fit$estimate[[2]]), size = 0.8, linetype = 2) +
      scale_x_continuous("Serial interval (Days)", expand = c(0, 0), breaks = seq(min(selData$serial_interval), max(selData$serial_interval), by = 2))+
      scale_y_continuous("Proportion", expand = c(0,0)) +
      theme_classic() +
      theme(aspect.ratio = 0.7)
    ret = list(siEstmate = siEstmate, siDistributionPlot = siDistributionPlot)  # list object: table of estimates and image
  }
  
  # Fitting gamma distribution
  # presymptomatic pair (seriel interval <= 0) will be dropped
  if(distr == "Gamma"){
    fit  = selData %>%
      dplyr::filter(serial_interval > 0)  %>%  # gamma can not be used to describe a random varaible with negative or 0 values
      dplyr::pull(serial_interval) %>% # extracting si
      fitdistrplus::fitdist(data = ., distr = "gamma")  
    
    # Estimating CI for mean and standard deviation by bootstrap method 
    #  1001 were used by default for bootstraping, Add a parameter for this at front end later if needed
    fit_boot <- summary(fitdistrplus::bootdist(fit))  
    
    # extracting estimates
    siEstmate = dplyr::bind_cols(data.frame(fit$estimate), data.frame(fit_boot$CI) ) # extracting estimates and CI as a data frame
    colnames(siEstmate) = c("Estimate", "Bootstrap median", "2.5% percentile CI", "97.5% percentile CI")
    siEstmate = round(siEstmate, 2)
    
    #Plotting 
    siDistributionPlot = ggplot(data = selData) +
      geom_histogram(aes(x = serial_interval, y = ..density..), fill = '#dedede', colour = "black", binwidth = 1) +
      #geom_point(aes(x = serial_interval, y = dweibull(x= serial_interval, shape = fit$estimate[[1]], scale = fit$estimate[[2]])), size = 1) +
      stat_function(fun = dgamma, args = list(shape = fit$estimate[[1]], rate = fit$estimate[[2]]), size = 0.8, linetype = 2) +
      scale_x_continuous("Serial interval (Days)", expand = c(0, 0), breaks = seq(min(selData$serial_interval), max(selData$serial_interval), by = 2))+
      scale_y_continuous("Proportion", expand = c(0,0)) +
      theme_classic() +
      theme(aspect.ratio = 0.7)
    ret = list(siEstmate = siEstmate, siDistributionPlot = siDistributionPlot)  # list object: table of estimates and image
  }
  
  # Fitting lognormal distribution
  # presymptomatic pair (seriel interval <= 0) will be dropped
  if(distr == "Lognormal"){
    fit  = selData %>%
      dplyr::filter(serial_interval > 0)  %>%  # lnorm can not be used to describe a random varaible with negative or 0 values
      dplyr::pull(serial_interval) %>% # extracting si
      fitdistrplus::fitdist(data = ., distr = "lnorm")  
    
    # Estimating CI for mean and standard deviation by bootstrap method 
    #  1001 were used by default for bootstraping, Add a parameter for this at front end later if needed
    fit_boot <- summary(fitdistrplus::bootdist(fit))  
    
    # extracting estimates
    siEstmate = dplyr::bind_cols(data.frame(fit$estimate), data.frame(fit_boot$CI) ) # extracting estimates and CI as a data frame
    colnames(siEstmate) = c("Estimate", "Bootstrap median", "2.5% percentile CI", "97.5% percentile CI")
    siEstmate = round(siEstmate, 2)
    
    #Plotting 
    siDistributionPlot = ggplot(data = selData) +
      geom_histogram(aes(x = serial_interval, y = ..density..), fill = '#dedede', colour = "black", binwidth = 1) +
      stat_function(fun = dlnorm, args = list(meanlog = fit$estimate[[1]], sdlog = fit$estimate[[2]]), size = 0.8, linetype = 2) +
      scale_x_continuous("Serial interval (Days)", expand = c(0, 0), breaks = seq(min(selData$serial_interval), max(selData$serial_interval), by = 2))+
      scale_y_continuous("Proportion", expand = c(0,0)) +
      theme_classic() +
      theme(aspect.ratio = 0.7)
    ret = list(siEstmate = siEstmate, siDistributionPlot = siDistributionPlot)  # list object: table of estimates and image
  }
  
  
  return(ret) #return a list object: table of estimates and image
}
save(serialIntervalPlot, file = "serialIntervalPlot.R")

## offspringDistPlot ------
offspringDistPlot = function(infectorInfecteePair){ 
  #counting the number of offsprings per infector
  offspring <- infectorInfecteePair %>%
    dplyr::select(case_id_infector) %>%
    dplyr::group_by(case_id_infector) %>%
    dplyr::count() %>%
    dplyr::arrange(desc(n))
  
  # extracting infectee nodes
  infectee <- infectorInfecteePair %>%
    dplyr::select(case_id_infectee) %>%
    tidyr::gather() #%>%
  #extracting infector nodes
  infector <-  infectorInfecteePair %>%
    dplyr::select(case_id_infector) %>%
    tidyr::gather()
  
  # selecting nodes that are linked to both infectors and infectees, thus duplicates
  duplicate <- infector %>%
    left_join(., infectee, by = 'value') %>%  # leftjoin infector and infectee
    dplyr::filter(key.y != 'NA') %>%  # filter all infectors who were also infectees in the past
    dplyr::select(value) %>% # keep only id
    dplyr::distinct()   # selecte distict node id, this should be person id in this case.
  
  # selecting terminal infectees nodes and sum them. 
  # This is a subset of termainal doses in the db because of infectorInfecteePair data
  nterminal_infectees <- infectee %>%
    dplyr::select(value) %>%
    dplyr::filter(!value %in% duplicate$value) %>% # infectees that are not infectors
    dplyr::transmute(case.no = as.character(value)) %>%
    nrow() 
  
  #create vector of complete offspring distribution with terminal cases having zero secondary cases
  complete_offspringd <- enframe(c(offspring$n, rep(0,nterminal_infectees))) # convert vector to dataframe
  
  #fit negative binomial distribution to the final offspring distribution
  fit <- complete_offspringd %>%
    dplyr::pull(value) %>%
    fitdistrplus::fitdist(., distr = 'nbinom')
  
  # Estimating CI by bootstrap method 
  fit_boot <- summary(fitdistrplus::bootdist(fit))  
  
  # extracting estimates
  rkEstmate = dplyr::bind_cols(data.frame(fit$estimate), data.frame(fit_boot$CI) ) # extracting estimates and CI as a data frame
  colnames(rkEstmate) = c("Estimate", "Bootstrap median", "2.5% percentile CI", "97.5% percentile CI")
  rkEstmate = round(rkEstmate, 2) # mu = nbfit$estimate[[2]] = mean = overall reporoduction number and  size = nbfit$estimate[[1]] = dispersion parameter k 
  rownames(rkEstmate) = c("Reproduction number (R)" , "Dispersion parameter (k)" )
  #plot offspring distribution with negative binomial parameters
  #Setting polynomial degree
  polyDegree = length(unique(complete_offspringd$value))
  if(polyDegree > 9){
    polyDegreePlot = 9 # Think of adding parameter for user defined if need be.
  } else {
    polyDegreePlot = polyDegree-1 # polyDegreePlot should be less than the number of unique values
  }
  offspringDistributionPlot = ggplot(data = complete_offspringd) +
    geom_histogram(aes(x=value, y = ..density..), fill = "#dedede", colour = "Black", binwidth = 1) +
    geom_point(aes(x = value, y = dnbinom(x = value, size = fit$estimate[[1]], mu = fit$estimate[[2]])), size = 1.5) +
    stat_smooth(aes(x = value, y = dnbinom(x = value, size = fit$estimate[[1]], mu = fit$estimate[[2]])), method = 'lm', formula = y ~ poly(x, polyDegreePlot), se = FALSE, size = 0.5, colour = 'black') +
    expand_limits(x = 0, y = 0) +
    scale_x_continuous("Secondary cases per infector", expand = c(0, 0), breaks = seq(min(complete_offspringd$value), max(complete_offspringd$value), by = 5))  +
    scale_y_continuous("Proportion of onward transmission", limits = c(0,0.7), expand = c(0, 0)) +
    theme_classic() +
    theme(aspect.ratio = 1)
  ret = list(rkEstmate = rkEstmate, offspringDistributionPlot = offspringDistributionPlot)  # list object: table of estimates and image
  
}
save(offspringDistPlot, file = "offspringDistPlot.R")
#retOffspring = offspringDistPlot(infectorInfecteePair = infectorInfecteeData)


########## contactDataExport ############
contactDataExport = function(sormas_db){
  # loading tables from sormas db
  # load cases
  case = dbGetQuery(
    sormas_db,
    "SELECT uuid AS case_uuid, id AS case_id, person_id AS person_id_case, region_id AS region_id_case, reportdate AS report_date_case,
    district_id AS district_id_case, caseclassification AS case_classification, symptoms_id, disease AS disease_case
    FROM cases
    WHERE deleted = FALSE and caseclassification != 'NO_CASE'"
  )
  
  #load contacts
  contact = dbGetQuery(
    sormas_db,
    "SELECT uuid AS contact_uuid, id AS contact_id, caze_id AS case_id, district_id AS district_id_contact, disease AS disease_contact, region_id AS region_id_contact,
    person_id AS person_id_contact, reportdatetime AS report_date_contact, lastcontactdate AS lastcontact_date_contact, 
    contactproximity AS contact_proximity, resultingcase_id, contactstatus, contactclassification, followupstatus, followupuntil,
    relationtocase, returningtraveler AS returningtraveler_contact
    FROM public.contact
    WHERE deleted = FALSE" 
    # To include only contacts with source case id, use: WHERE deleted = FALSE and caze_id IS NOT NULL
  )
  #loading symptom data
  symptoms = dbGetQuery(
    sormas_db,
    "SELECT uuid AS symptom_uuid, id AS symptom_id, onsetdate AS onset_date
                         from public.symptoms"
  )
  # load person data
  person = dbGetQuery(
    sormas_db,
    "SELECT uuid AS person_uuid, id AS person_id
    FROM person"
  ) 
  # reading event
  events = dbGetQuery(
    sormas_db,
    "SELECT uuid AS event_uuid, id AS event_id, reportdatetime AS report_date_event, eventstatus, disease AS disease_event, typeofplace, eventlocation_id
    FROM public.events
    WHERE deleted = FALSE AND eventstatus != 'DROPPED'"
  )
  
  ## reading event participants
  eventsParticipant = dbGetQuery(
    sormas_db,
    "SELECT uuid AS event_part_uuid, id as eventPart_id, event_id, person_id AS person_id_eventPart, resultingcase_id AS resultingcaseid_eventPart
    FROM public.eventParticipant
    WHERE deleted = FALSE "
  )
  # load region
  region = dbGetQuery(
    sormas_db,
    "SELECT id AS region_id, name AS region_name
    FROM public.region
    WHERE archived = FALSE"
  ) 
  # load district
  district = dbGetQuery(
    sormas_db,
    "SELECT id AS district_id, name AS district_name
    FROM district
    WHERE archived = FALSE"
  )
  
  ## clean-up tables 
  ## fixing date formats for case,  contacts, symptoms
  case = case %>%
    dplyr::mutate(report_date_case = as.Date(format(report_date_case, "%Y-%m-%d")))
  
  contact  = contact %>%
    dplyr::mutate(report_date_contact = as.Date(format(report_date_contact, "%Y-%m-%d")),
                  lastcontact_date_contact = as.Date(format(lastcontact_date_contact, "%Y-%m-%d")),
                  followupuntil = as.Date(format(followupuntil, "%Y-%m-%d"))
    )
  
  symptoms = symptoms %>%
    dplyr::mutate(onset_date = as.Date(format(onset_date, "%Y-%m-%d")))
  
  events = events %>%
    dplyr::mutate(report_date_event = as.Date(format(report_date_event, "%Y-%m-%d")))
  
  ## Converting id to character for eary stacking of tables
  person$person_id = as.character(person$person_id)
  contact$person_id_contact = as.character(contact$person_id_contact )
  case$person_id_case = as.character(case$person_id_case)
  eventsParticipant$person_id_eventpart = as.character(eventsParticipant$person_id_eventpart)
  
  ### merging data to generate the line-list of contacts
  contCaseRegionDist = contact %>%
    dplyr::left_join(., case, by="case_id") %>%  # Merging contact and case data by source case or infector case id
    fixContactJurisdiction(contCase = .) %>%  # giving contacts with missing region id the region and district id of their source cases
    dplyr::select(., -c(region_id_case, district_id_case )) %>%  # dropping jurisdiction of source case
    dplyr::left_join(., region, by = c("region_id_contact" = "region_id" )) %>% # merging with region table to get region details of  infector person
    dplyr::left_join(., district, by = c("district_id_contact" = "district_id" )) %>% # merging with district table to get district details of  infector person
    dplyr::mutate(region_name_contact = region_name, district_name_contact = district_name, case_classification_infector = case_classification, .keep = "unused") #renaming
  # contCaseRegionDist dataset has unique contacts but not unique person to person pair, thus we need to filter it to het unique adges between 2 nodes
  
  # Generation dataset of edges, elist, including events 
  # relationtocase
  elist = contCaseRegionDist %>%
    dplyr::distinct(. , person_id_case, person_id_contact, .keep_all = T) %>% # keeping only contacts with distinct persons pair
    dplyr::filter(person_id_case != person_id_contact) %>% # deleting contacts between one person and itselt and also dropping contacts wothout source cases
    dplyr::select(. , c(from = person_id_case, to = person_id_contact, case_id, contact_id, resultingcase_id, region_name_contact, district_name_contact,
                        contact_proximity, report_date_contact, disease_contact, case_classification_infector, relationtocase)  
    ) %>%
    dplyr::mutate(
      label = case_when(
        contact_proximity %in% c("AEROSOL", "FACE_TO_FACE_LONG","TOUCHED_FLUID","MEDICAL_UNSAVE","CLOTHES_OR_OTHER","PHYSICAL_CONTACT" ) ~ 1,
        !(contact_proximity %in% c("AEROSOL", "FACE_TO_FACE_LONG","TOUCHED_FLUID","MEDICAL_UNSAVE","CLOTHES_OR_OTHER","PHYSICAL_CONTACT" )) ~ 2 
      ), # assiginh 2 to missing and all other contact proximities not listed here
      dashes = case_when(label == 1 ~ FALSE,
                         label == 2 ~ TRUE
      ), #defining broken lines fro high risk contact based on con tact procimity or label
      smooth = TRUE,
      arrows = "to", .keep = "unused"
    ) 
  
  # Defining nodes data and their attributes 
  contConvertedCase = elist %>%
    dplyr::filter(resultingcase_id != 'NA') %>%
    dplyr::left_join(., case,  c( "resultingcase_id" = "case_id")) %>%
    dplyr::select(., c( from = person_id_case, case_classification_infector = case_classification) )  %>%  # 
    dplyr::mutate(from = as.character(from), .keep = "unused")
  # person case classification fro resulting cases
  #  c( to, case_classification_infectee = case_classification) this is normall naming
  # above naming is to facilitate stacking later
  
  #combining nodes of all person who are either infector or infectee case
  nodesCasePersons = elist %>% 
    dplyr::select(., c(from, case_classification_infector )) %>% 
    dplyr::bind_rows(., contConvertedCase ) %>%
    dplyr::distinct(from, .keep_all = TRUE ) # person id and case classificatiion of all nodes that are cases
  
  # obtaining combined nodes from contact table and defining their group attributes
  nodeLineList = elist %>%   # conmbined nodes fro all network
    dplyr::select(., c(to, case_classification_infector )) %>%
    dplyr::filter(!(to %in% contConvertedCase$from) ) %>%  # filter contact persons that did not become cases
    dplyr::distinct(to, .keep_all = TRUE )  %>% # keeping only distinct person 
    dplyr::mutate( from = to, case_classification_infector = "HEALTHY", .keep = "unused" ) %>% #
    dplyr::bind_rows(., nodesCasePersons )  %>%  # combinig healthy and case nodes
    # nodeLineList$group = nodeLineList$Classification
    dplyr::mutate(id = from,
                  value = 1,
                  shape = "icon",
                  code = "f007",
                  group = case_classification_infector,
                  .keep = "unused"
    ) %>% # merge with person table to get the person_uuid of each node
    dplyr::left_join(., person,  c( "id" = "person_id"))
  
  # At this point, we have buit the nodeLineList and elist onjects using only contact data as base.
  # We will now do the same thing for events and then stack them to have the complate transmission chain 
  # of cases, contacts and events
  # in theis transformation, we map source case person  node to event, contacts to event participants and resulting  case node to resulting case node
  # evet participant that do not result to cases are not included at this stage
  
  elistEvent = eventsParticipant %>%
    dplyr::distinct(., event_id, person_id_eventpart , .keep_all = TRUE )  %>% # randomly deleting the one pair of the same person created twice in an event
    dplyr::filter(resultingcaseid_eventpart != 'NA') %>% # keeping only event part that resulted to cases
    dplyr::left_join(. , case,  c( "resultingcaseid_eventpart" = "case_id")) %>%
    dplyr::filter(person_id_case != 'NA') %>%  # dropping resulting cases that were later deleted.  When resulting case from ep is deleted, the resulting case id is not deleted on the ep table
    dplyr::left_join(. , events,  c( "event_id" = "event_id")) %>% # merge with event
    dplyr::left_join(. , region,  c( "region_id_case" = "region_id")) %>% # use jurisdiction  of resulting case as referece
    dplyr::left_join(., district, by = c("district_id_case" = "district_id" )) %>% # merging with district table to get district details
    dplyr::mutate(from = event_uuid, to = as.character(person_id_case), case_id = resultingcaseid_eventpart, contact_id = eventpart_id,
                  resultingcase_id = resultingcaseid_eventpart, region_name_contact = region_name, district_name_contact = district_name,
                  report_date_contact = report_date_event, disease_contact = disease_case,
                  case_classification_infector  = "CONFIRMED", case_classification_infectee = case_classification,
                  relationtocase = "EVENT",
                  label = 1,
                  dashes = FALSE,
                  smooth = TRUE,
                  arrows = "to",   
                  .keep = "none") # mappping to produce a similar data like  elist
  
  # defining node properties for event nodes
  eventNode = elistEvent %>%
    dplyr::mutate(., case_classification_infector = "EVENT",
                  id = from,
                  value = 1,
                  shape = "icon",
                  code = c("f0c0"),
                  group = case_classification_infector,
                  .keep = "none" ) %>%
    dplyr::distinct(., id , .keep_all = TRUE )
  
  # defining node properties for event part nodes. These are just person nodes
  eventPartNode = elistEvent %>%
    dplyr::mutate(., case_classification_infector = case_classification_infectee,
                  id = to,
                  value = 1,
                  shape = "icon",
                  code = c("f007"),
                  group = case_classification_infector,
                  .keep = "none" ) %>%
    dplyr::distinct(., id , .keep_all = TRUE )
  
  nodeLineListEvent =  dplyr::bind_rows(eventNode, eventPartNode )  # combining event nodes and ep person nodes 
  
  # combining elist from event and elist from contact
  # delete case_classification_infectee from elistEvent before merging with elist
  elistCombined = elistEvent %>%
    dplyr::select(., -c(case_classification_infectee)) %>%
    dplyr::bind_rows(., elist )  %>%
    dplyr::mutate(from = substr(from, 1, 6), .keep = "unused" )   # keep first 6 characters  of person id
  
  nodeListCombined =  nodeLineList  %>%
    dplyr::bind_rows(. , nodeLineListEvent ) %>%
    dplyr::distinct(., id , .keep_all = TRUE ) %>%
    dplyr::mutate(id = substr(id, 1, 6), .keep = "unused" ) %>%   # keep first 6 characters  of person id
    dplyr::select( -c(person_uuid, case_classification_infector ) ) # dropping useless varaibles
  
  return(list(contRegionDist = contCaseRegionDist, nodeLineList = nodeListCombined, elist = elistCombined)) 
}
save(contactDataExport, file = "contactDataExport.R")



#################"
