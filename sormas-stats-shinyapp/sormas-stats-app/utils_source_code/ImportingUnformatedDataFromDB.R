ImportingUnformatedDataFromDB = function(sormas_db, fromDate, toDate)
{ 
  # connecting to db
  #con = dbConnect(PostgreSQL(), user="sormas_user", dbname="sormas", password = "password", host="127.0.0.1", port="5432")
  # con = dbConnect(PostgreSQL(), user=DB_USER,  dbname=DB_NAME, password = DB_PASS, host=DB_HOST, port=DB_PORT)
  
  # reading unique cases based on all varaibles. 
  # use  where deleted = FALSE  and caseclassification != 'NO_CASE'  in case you want to eliminate not a cases
  queryCase <- paste0("SELECT  DISTINCT id, disease, reportdate, creationdate, person_id, responsibleregion_id AS region_id, responsibledistrict_id AS district_id, 
  caseclassification, epidnumber, symptoms_id, healthfacility_id, outcome,caseorigin,quarantine
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