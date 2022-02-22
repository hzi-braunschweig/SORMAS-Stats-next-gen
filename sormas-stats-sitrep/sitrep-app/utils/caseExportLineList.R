caseExportLineList <- function(sormas_db, fromDate, toDate){
  
  # This function creates a table containing a line listing of all cases
  # reported in the defined time period. This line listing of cases includes
  # all the variables as columns that are necessary for the creation of the
  # SitRep, apart from population and geo shapes data which can be exported 
  # using the populationExport() & geoshapesExport() functions.
  
  
  ## Computing default time based on 365 days in the past if not provided by user
  if(missing(fromDate) | missing(toDate)){
    fromDate = as.character(Sys.Date() - delay) 
    toDate = as.character(Sys.Date()) 
  }

  ## Loading tables from SORMAS DB
  
  # Case table SQL query
  queryCases <- base::sprintf("SELECT  DISTINCT id AS id_case,
    district_id AS id_district,
    region_id AS id_region,
    caseclassification AS caseclassification_case,
    EndOfIsolationReason AS end_of_iso_reason_case,
    reportdate AS report_date_case,
    disease AS disease_case,
    symptoms_id AS id_symptoms,
    person_id AS id_person,
    hospitalization_id AS id_hospitalization
    FROM public.cases 
    WHERE deleted = FALSE 
    and caseclassification != 'NO_CASE'
    and reportdate between '%s' and '%s'
                       ", fromDate, toDate)
  cases <- DBI::dbGetQuery(sormas_db,queryCases)
  
  
  # Person table SQL query
  queryPersons <- base::sprintf("SELECT DISTINCT id AS id_person,
    approximateAge AS age_person,
    sex AS sex_person,
    deathDate AS death_date_person,
    causeOfDeathDisease AS cause_of_death_disease_person
    FROM public.person
    WHERE id IN (%s)", paste("'", base::unique(c(cases$id_person)), "'",collapse=","))
  
  # Only persons with a person_id that also appears in a case 
  # are queried from the database
  persons <- DBI::dbGetQuery(sormas_db,queryPersons)
  
  # Symptoms table SQL query
  querySymptoms <- base::sprintf("SELECT DISTINCT id AS id_symptoms,
    onsetDate AS onset_date_symptoms
    FROM public.symptoms 
    WHERE id IN (%s)", paste("'", base::unique(c(cases$id_symptoms)), "'",collapse=","))
  # Only symptoms with a symptoms_id that also appears in a case 
  # are queried from the database
  symptoms <- DBI::dbGetQuery(sormas_db, querySymptoms)
  
  # Hospitalizations table query 
  queryHospitalizations <- base::sprintf("SELECT DISTINCT id AS id_hospitalization,
    admittedToHealthFacility AS admitted_to_health_facility_hospitalization,
    admissionDate AS admission_date_hospitalization,
    hospitalizationReason AS reason_hospitalization
    FROM public.hospitalization 
    WHERE id IN (%s)", paste("'", base::unique(c(cases$id_hospitalization)), "'",collapse=","))
  # Only hospitalizations with a hospitalization_id that also appears in a
  # case are queried from the database
  hospitalizations <- DBI::dbGetQuery(sormas_db, queryHospitalizations)
  
  # District table SQL query
  queryDistrict <- paste0("SELECT DISTINCT id AS id_district,
    name AS name_district
    FROM public.district
    WHERE archived = FALSE
                          ")
  districts <- DBI::dbGetQuery(sormas_db, queryDistrict)
  
  # Region table SQL query
  queryRegions <- paste0("SELECT DISTINCT id AS id_region,
    name AS name_region
    FROM public.region 
    WHERE archived = FALSE
                         ")
  regions <- DBI::dbGetQuery(sormas_db, queryRegions)
  
  
  # Merging data from cases, persons, hospitalizations, districts and regions 
  # exports into one line listed DataFrame with every row being a case 
  line_list_cases <- cases %>% 
    dplyr::left_join(., persons, by = 'id_person') %>% 
    dplyr::left_join(., symptoms, by = 'id_symptoms') %>% 
    dplyr::left_join(., hospitalizations, by = 'id_hospitalization') %>% 
    dplyr::left_join(., districts, by = 'id_district') %>% 
    dplyr::left_join(., regions, by = 'id_region') %>% 
    dplyr::mutate(report_date_case = as.Date(format(report_date_case, "%Y-%m-%d")),
                  admission_date = as.Date(format(admission_date_hospitalization, "%Y-%m-%d")),
                  onset_date = as.Date(format(onset_date_symptoms, "%Y-%m-%d"))) 
  
  # Return the output table
  return(line_list_cases)
}




