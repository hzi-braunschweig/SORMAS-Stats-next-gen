# exporting sample from sormas db ----
# attributes exported are those needed to generate indicators on the sample data analysis tab of sormas-stats
sample_export = function(sormas_db, fromDate, toDate){
  # leading sample table
  querySample <- paste0("SELECT uuid AS uuid_sample, id AS id_sample, creationdate, sampledatetime AS data_sample_collected, reportdatetime AS date_of_report,
  pathogentestresult, associatedcase_id, associatedcontact_id, associatedeventparticipant_id, samplingreason, samplepurpose, shipped, received, referredto_id,
  specimencondition, samplesource, shipmentdate, receiveddate, samplematerial
                        FROM public.samples
                        WHERE deleted = FALSE and reportdatetime between '", fromDate, "' and '", toDate, "' ")
  # determining derived attributes (region, district, disease) by merging with source entity (case, contact, event participant) of sample
  #
  sample = dbGetQuery(sormas_db,querySample)
  
#   ## reading event participants data that are linked to the extracted samples
#   id_event_part = unique(na.omit(sample$associatedeventparticipant_id))
#   if(!vector_is_empty(id_event_part)){
#     queryEventPart <- sprintf("SELECT  id AS id_eventPart, event_id AS event_id_eventpart, person_id AS person_id_eventPart, resultingcase_id AS resultingcase_id_eventPart
#                         FROM public.eventParticipant
#                         WHERE deleted = FALSE and id in (%s)", paste("'", id_event_part, "'",collapse=",") )
#     eventParticipant = dbGetQuery(sormas_db, queryEventPart)
#   }
#   ## reading case data that are linked to the extracted samples
#   id_case = unique(na.omit(sample$associatedcase_id))
#   if(!vector_is_empty(id_case)){
#     queryCase <- base::sprintf("SELECT uuid AS case_uuid, id AS case_id, disease AS disease_case, person_id AS person_id_case, responsibleregion_id AS region_id_case, responsibledistrict_id AS district_id_case,
#     caseclassification AS case_classification
#                           FROM public.cases 
#                           WHERE deleted = FALSE and id in (%s)", paste("'", id_case, "'",collapse=",") )
#     case = dbGetQuery(sormas_db, queryCase)
#   }
#   # reading contact data that are linked to  samples
#   id_contact = unique(na.omit(sample$associatedcontact_id))
#   if(!vector_is_empty(id_contact)){
#     queryContact <- base::sprintf("SELECT uuid AS uuid_contact, id AS id_contact, disease AS disease_contact, district_id AS id_responsibledistrict_contact, region_id AS id_responsibleregion_contact,
#     person_id AS id_person_contact, caze_id AS id_sourcecase_contact 
#     FROM public.contact
#     WHERE deleted = FALSE and id in (%s)", paste("'", id_contact, "'",collapse=",") )
#     contact = dbGetQuery(sormas_db, queryContact)
#   }
#   
# ## for contacts that do not have a jurisdiction, map their jurisdictions with that of the source case, do the same for event particiipant using the jurisdiction of events
#   
#  
#   # test if source dataframes are empty
#   # dataframe_is_empty
#   
#   # load region 
#   region = dbGetQuery(
#     sormas_db,
#     "SELECT id AS region_id, name AS region_name
#     FROM public.region
#     WHERE archived = FALSE"
#   ) 
#   # load district
#   district = dbGetQuery(
#     sormas_db,
#     "SELECT id AS district_id, name AS district_name
#     FROM district
#     WHERE archived = FALSE"
#   )
#   
#   # merging event data with jurisdiction data
#   event = event %>%
#     dplyr::mutate(reportdatetime_event = as.Date(format(reportdatetime_event, "%Y-%m-%d")), 
#                   startdate_event = as.Date(format(startdate_event, "%Y-%m-%d")),
#                   enddate_event = as.Date(format(enddate_event, "%Y-%m-%d")),
#                   creationdate_event = as.Date(format(creationdate_event, "%Y-%m-%d"))  ) %>%  # converting dates from POSIXct class to Date class
#     dplyr::mutate(relevantdate_event = coalesce(startdate_event, reportdatetime_event )  ) %>% # computing most relevant date using start date and fall back to report date
#     dplyr::left_join(. , location,  c( "eventlocation_id" = "id_location")) %>% # merging with locationn
#     dplyr::left_join(. , region,  c( "region_id_location" = "region_id")) %>% # merging with region
#     dplyr::left_join(. , district,  c( "district_id_location" = "district_id")) %>% 
#     tidyr::replace_na(list(region_name = "Missing Region", district_name = "Missing District"))
#   
#   # merging event data with eventPart data and count number of resulting cases and ep per event
#   eventPartEvent = eventParticipant %>%
#     dplyr::left_join(. , event, c( "event_id_eventpart" = "id_event") ) %>%  # merging eventParticipant with event
#     dplyr::group_by(. , event_id_eventpart) %>% 
#     dplyr::summarise(eventPart_sum = n(),
#                      resulting_case_sum = sum(!is.na(resultingcase_id_eventpart) )
#     ) %>% # counting number of ep and resulting cases per event
#     dplyr::right_join(., event, c("event_id_eventpart" = "id_event") ) %>% # merging with event, use right join to keep events with no ep
#     tidyr::replace_na(list(eventPart_sum = 0, resulting_case_sum = 0, event_identification_source = "MISSING" )) # replacing missing values (NA) with o or "MISSING"
  ret = sample
  return(sample_data = ret)  
}
# # # test run
# sormas_db = dbConnect(PostgreSQL(), user=DB_USER,  dbname=DB_NAME, password = DB_PASS, host=DB_HOST, port=DB_PORT)
# source(file.path(".", "loading_packages.R"))
# source(file.path(".", "loading_functions_source.R"))
# sample_table = sample_export(sormas_db, fromDate, toDate)
