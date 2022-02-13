#' Export Line Listing of Cases
#'
#' @param sormas_db Connection to SORMAS database
#' 
#' @param fromDate Start date of export as character string.
#' @param toDate End date of export as character string.
#'
#' @return  Returns a table containing a line listing of all cases
#'  that fulfill one of the following conditions:
#'  - the report date is in the defined time period
#'  - the hospitalization date is in the defined time period
#'  - the death date is in the defined period
#'  This line listing of cases includes
#'  all the variables as columns that are necessary for the creation of the
#'  SitRep, apart from population and geo shapes data which can be exported 
#'  using the ExportPopulation() & ExportGeoshapes() functions.
#' 
#' @seealso [ExportPopulation()] and [ExportGeoshapes()]
#' 
#' @export
#'
#' @examples
#' 
ExportCaseLineList <- function(sormas_db, fromDate, toDate){
  
  ## Computing default time based on 365 days in the past if not provided by user
  if(missing(fromDate) | missing(toDate)){
    fromDate = as.character(Sys.Date() - delay) 
    toDate = as.character(Sys.Date()) 
  }
  
  ## Loading tables from SORMAS DB
  
  # Person table SQL query
  queryPersons <- base::sprintf("SELECT DISTINCT id AS id_person,
    deathDate AS death_date_person,
    causeOfDeathDisease AS cause_of_death_disease_person
    FROM public.person
    WHERE deathDate between '%s' and '%s'", fromDate, toDate)
  
  persons <- DBI::dbGetQuery(sormas_db,queryPersons)
  
  # Symptoms table SQL query
  querySymptoms <- base::sprintf("SELECT DISTINCT id AS id_symptoms,
    onsetDate AS onset_date_symptoms
    FROM public.symptoms 
    WHERE onsetDate between '%s' and '%s'", fromDate, toDate)
  
  symptoms <- DBI::dbGetQuery(sormas_db, querySymptoms)
  
  # Hospitalizations table query 
  queryHospitalizations <- base::sprintf("SELECT DISTINCT id AS id_hospitalization,
    admittedToHealthFacility AS admitted_to_health_facility_hospitalization,
    admissionDate AS admission_date_hospitalization,
    hospitalizationReason AS reason_hospitalization
    FROM public.hospitalization 
    WHERE admissionDate between '%s' and '%s'", fromDate, toDate)
  
  hospitalizations <- DBI::dbGetQuery(sormas_db, queryHospitalizations)
  
  # Case table SQL query
  queryCases <- base::sprintf("SELECT  DISTINCT id AS id_case,
    responsibledistrict_id AS id_district,
    responsibleregion_id AS id_region,
    caseclassification AS caseclassification_case,
    EndOfIsolationReason AS end_of_iso_reason_case,
    reportdate AS report_date_case,
    disease AS disease_case,
    symptoms_id AS id_symptoms,
    person_id AS id_person,
    hospitalization_id AS id_hospitalization
    FROM public.cases 
    WHERE (deleted = FALSE AND caseclassification != 'NO_CASE')
    AND (reportdate between '%s' and '%s' 
         OR person_id IN (%s) 
         OR hospitalization_id IN (%s) 
         OR symptoms_id IN (%s))",
                              fromDate, toDate,
                              paste("'", base::unique(c(persons$id_person)), "'",collapse=","),
                              paste("'", base::unique(c(hospitalizations$id_hospitalization)), "'",collapse=","),
                              paste("'", base::unique(c(symptoms$id_symptoms)), "'",collapse=","))
  
  cases <- DBI::dbGetQuery(sormas_db,queryCases)
  
  # Separate query for birthdate and sex
  queryBirthdateSex <- base::sprintf("SELECT DISTINCT id AS id_person,
    birthdate_dd as birthdate_dd_person,
    birthdate_mm as birthdate_mm_person,
    birthdate_yyyy as birthdate_yyyy_person,
    sex AS sex_person
    FROM public.person
    WHERE id IN (%s)", paste("'", base::unique(c(cases$id_person)), "'",collapse=","))
  
  birthdate_sex <- DBI::dbGetQuery(sormas_db, queryBirthdateSex)
  
  
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
  
  
  # Merging data from cases, persons, hospitalizations, birhtdate_sex,
  # districts and regions exports into one line listed data frame
  # with every row being a case 
  line_list_cases <- cases %>% 
    dplyr::left_join(., persons, by = 'id_person') %>% 
    dplyr::left_join(., birthdate_sex, by = 'id_person') %>% 
    dplyr::left_join(., symptoms, by = 'id_symptoms') %>% 
    dplyr::left_join(., hospitalizations, by = 'id_hospitalization') %>% 
    dplyr::left_join(., districts, by = 'id_district') %>% 
    dplyr::left_join(., regions, by = 'id_region') %>% 
    dplyr::mutate(report_date_case = as.Date(format(report_date_case, "%Y-%m-%d")),
                  admission_date_hospitalization = as.Date(format(admission_date_hospitalization, "%Y-%m-%d")),
                  onset_date_symptoms = as.Date(format(onset_date_symptoms, "%Y-%m-%d"))) 
  
  # Return the output table
  return(line_list_cases)
}




