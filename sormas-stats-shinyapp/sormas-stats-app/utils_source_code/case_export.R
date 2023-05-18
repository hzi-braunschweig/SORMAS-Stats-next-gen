# This function export cases from sormas db
# only attributes needed for analysis are included
# The case table would be merged with person, region, district, etc to et other attributs of the case
case_export = function(sormas_db, fromDate, toDate){ 
  # reading cases
  sqlCase <-"SELECT distinct id AS case_id, disease, reportdate, creationdate, person_id, responsibleregion_id AS region_id, 
  responsibledistrict_id AS district_id, caseclassification, epidnumber, symptoms_id, healthfacility_id, outcome,caseorigin,quarantine
  FROM public.cases 
  WHERE deleted = FALSE and caseclassification != 'NO_CASE' and reportdate between ?fromDateTemp and ?toDateTemp"
  queryCase = DBI::sqlInterpolate(sormas_db, sqlCase, fromDateTemp=fromDate, toDateTemp=toDate)  
  case = dbGetQuery(sormas_db,queryCase)
  ### reading person data 
  sqlPerson = "SELECT id AS person_id, sex, occupationtype, presentcondition, birthdate_dd, birthdate_mm, birthdate_yyyy
  FROM public.person
  WHERE id IN (SELECT person_id AS id
  FROM public.cases
  WHERE deleted = FALSE AND caseclassification != 'NO_CASE' AND reportdate between ?fromDateTemp and ?toDateTemp)"
  queryPerson = DBI::sqlInterpolate(sormas_db, sqlPerson, fromDateTemp=fromDate, toDateTemp=toDate) 
  person = dbGetQuery(sormas_db, queryPerson)
  ###  reading symptom data corresponding to selected cases only
  sqlSymptom = "SELECT id AS symptoms_id, onsetdate
  FROM public.symptoms
  WHERE id IN
  (SELECT distinct symptoms_id AS id
  FROM public.cases 
  WHERE deleted = FALSE and caseclassification != 'NO_CASE' and reportdate between ?fromDateTemp and ?toDateTemp)"
  querySymptom = DBI::sqlInterpolate(sormas_db, sqlSymptom, fromDateTemp=fromDate, toDateTemp=toDate)
  symptoms = dbGetQuery(sormas_db, querySymptom)
  
  # reading region
  region = dbGetQuery(sormas_db,"select distinct id AS region_id, name AS region_name
                         from public.region ") 
  #loading district
  district = dbGetQuery(sormas_db,"select distinct id AS district_id, name AS district_name
                         from public.district")
  
  ## Cleaning-up tables
  # Computing birthday of person using day, month and year
  # Assigning DOB to person with only year of birth using January 1. This is just a rough estimated birth date and should be improved
  person = fixBirthDate(person) %>% # This method assign the birth date of the person as first January of the year of birth. Improvement will follow
    dplyr::select(person_id, sex, occupationtype,presentcondition, date_of_birth)  #dropping unused variables used to compute date_of_birth
    
  # Fixing date formats
  case = case %>%
    dplyr::mutate(reportdate = as.Date(format(reportdate, "%Y-%m-%d")),
                  creationdate = as.Date(format(creationdate, "%Y-%m-%d")))
  symptoms = symptoms %>%
    dplyr::mutate(onsetdate = as.Date(format(onsetdate, "%Y-%m-%d")))
    
   ## Merging tables
  ret = case %>% 
    dplyr::left_join(., person, by="person_id") %>% 
    dplyr::left_join(., symptoms, by="symptoms_id") %>% 
    dplyr::left_join(., region, by="region_id") %>% 
    dplyr::left_join(., district, by="district_id") %>% 
    #compute derived variables
    dplyr::mutate(age=floor(as.numeric(reportdate - date_of_birth)/365), reportweek = lubridate::week(reportdate), 
                  reportmonth = lubridate::month(reportdate), reportyear=lubridate::year(reportdate),
                  total = rep(1, nrow(case)) # variable total is added for plotting time series
                  )
  return(ret)
}