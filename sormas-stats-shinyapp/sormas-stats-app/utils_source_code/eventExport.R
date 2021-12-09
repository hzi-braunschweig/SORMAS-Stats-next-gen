# exporting event from sormas db ----
# sormas_db = dbConnect(PostgreSQL(), user=DB_USER,  dbname=DB_NAME, password = DB_PASS, host=DB_HOST, port=DB_PORT)
eventExport = function(sormas_db, fromDate, toDate){
  # loading tables from sormas db
  # leading event
  queryEvent <- paste0("SELECT uuid AS uuid_event, id AS id_event, eventinvestigationstatus, reportdatetime AS reportdatetime_event, eventstatus, disease AS disease_event, typeofplace AS typeofplace_event,
      creationdate AS creationdate_event, enddate AS enddate_event, startdate AS startdate_event, archived AS archived_event, nosocomial AS nosocomial_event,
      srctype AS srctype_event, risklevel AS risklevel_event,  eventlocation_id, eventmanagementstatus, eventidentificationsource AS  event_identification_source, eventtitle
                        FROM public.events
                        WHERE deleted = FALSE and eventstatus != 'DROPPED' and disease IS NOT NULL and reportdatetime between '", fromDate, "' and '", toDate, "' ")
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
    dplyr::right_join(., event, c("event_id_eventpart" = "id_event") ) %>% # merging with event, use right join to keep events with no ep
    tidyr::replace_na(list(eventPart_sum = 0, resulting_case_sum = 0, event_identification_source = "MISSING" )) # replacing missing values (NA) with o or "MISSING"
  
  return(event_data = eventPartEvent)  
}