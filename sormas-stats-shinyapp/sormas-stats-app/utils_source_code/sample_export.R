# exporting sample from sormas db ----
# attributes exported are those needed to generate indicators on the sample data analysis tab of sormas-stats
sample_export = function(sormas_db, fromDate, toDate){
  # leading sample table linked to cases only. A sample can be referenced by contacts, cases, and event participants
  # the goal is to export sample for each entity type and stack the tables 
  # this implementation is for samples linked to cases only, others can be added later 
  querySample <- base::paste0("SELECT uuid AS uuid_sample, id AS id_sample, creationdate, sampledatetime AS data_sample_collected, reportdatetime AS date_of_report, pathogentestresult,
  associatedcase_id, samplingreason, samplepurpose, shipped, received, referredto_id,specimencondition, samplesource, shipmentdate, receiveddate, samplematerial
  FROM public.samples
  WHERE deleted = FALSE and associatedcase_id IS NOT NULL and reportdatetime between '", fromDate, "' and '", toDate, "' ")
  sample_cases = dbGetQuery(sormas_db,querySample)
#determining derived attributes (disease, region, district) by merging with source entity (case) of sample
#reading case data that are linked to the extracted samples
if(!dataframe_is_empty(sample_cases)){
  #loading cases
  queryCase <- base::paste0("SELECT uuid AS case_uuid, id AS case_id, disease AS disease_case, responsibleregion_id AS region_id_case, responsibledistrict_id AS district_id_case,
  caseclassification AS case_classification
  FROM public.cases
  WHERE deleted = FALSE and id IN (
  SELECT  distinct associatedcase_id
  FROM public.samples 
  WHERE deleted = FALSE and associatedcase_id IS NOT NULL and reportdatetime between '", fromDate, "' and '", toDate, "')" )
  
  case = dbGetQuery(sormas_db, queryCase)
  # load region
  region = dbGetQuery(sormas_db, 
  "SELECT id AS region_id, name AS region_name
  FROM public.region"
  )
  # load district
  district = dbGetQuery(sormas_db,
  "SELECT id AS district_id, name AS district_name
  FROM district"
  )
  #merging data
  ret = sample_cases %>% 
    dplyr::mutate(creationdate = as.Date(format(creationdate, "%Y-%m-%d")),
                  data_sample_collected = as.Date(format(data_sample_collected, "%Y-%m-%d")),
                  date_of_report = as.Date(format(date_of_report, "%Y-%m-%d")),
                  shipmentdate = as.Date(format(shipmentdate, "%Y-%m-%d"))) %>%  # converting dates from POSIXct class to Date class
    dplyr::left_join(.,case,c("associatedcase_id"="case_id")) %>% 
    dplyr::left_join(.,region,c("region_id_case"="region_id")) %>% # merging with region
    dplyr::left_join(.,district,c("district_id_case"="district_id")) %>% 
    dplyr::mutate(disease_sample = disease_case) %>% # signing disease of samples as that of case. include disease of other entities (contacts, event part) when adding them to the sample export
    dplyr::select(., -c(associatedcase_id,region_id_case,district_id_case )) # dropping unimportant variables
}else{
 ret=print("There is no data available to extract")
}
 return(sample_data = ret)  
} 
## test run----
# sormas_db = dbConnect(PostgreSQL(), user=DB_USER,  dbname=DB_NAME, password = DB_PASS, host=DB_HOST, port=DB_PORT)
# sample_table = sample_export(sormas_db, fromDate, toDate)
