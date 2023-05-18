infectorInfecteeExport = function(sormas_db, fromDate, toDate){
  # loading tables from sormas db
  # load cases
  sqlCase <- "SELECT  DISTINCT uuid AS case_uuid, id AS case_id, disease, reportdate AS report_date_case, creationdate AS creation_date_case, person_id AS person_id_case,
    responsibleregion_id AS region_id_case, responsibledistrict_id AS district_id_case, caseclassification AS case_classification, caseorigin AS case_origin, symptoms_id
                          FROM public.cases 
                          WHERE deleted = FALSE and caseclassification != 'NO_CASE' and reportdate between ?fromDateTemp and ?toDateTemp" 
  queryCase = DBI::sqlInterpolate(sormas_db, sqlCase, fromDateTemp=fromDate, toDateTemp=toDate) 
  case = dbGetQuery(sormas_db,queryCase)
  
  # load contacts having their source case among the selected cases only.
  sqlContact ="SELECT uuid AS contact_uuid, id AS contact_id, caze_id AS case_id, district_id AS district_id_contact, region_id AS region_id_contact,
    person_id AS person_id_contact, reportdatetime AS report_date_contact, creationdate AS creation_date_contact, lastcontactdate AS lastcontact_date_contact,
    contactproximity AS contact_proximity, resultingcase_id, contactstatus, contactclassification
    FROM public.contact
    WHERE deleted = FALSE and caze_id IS NOT NULL and caze_id IN
     (SELECT id AS caze_id
     FROM public.cases
     WHERE deleted = FALSE and caseclassification != 'NO_CASE' and reportdate between ?fromDateTemp and ?toDateTemp)"
  queryContact = DBI::sqlInterpolate(sormas_db, sqlContact, fromDateTemp=fromDate, toDateTemp=toDate) 
  contact = dbGetQuery(sormas_db, queryContact)
  
  #loading symptom data corresponding to selected cases only
  sqlSymptoms = "SELECT id AS symptom_id, onsetdate AS onset_date
  FROM public.symptoms
  WHERE id IN
  (SELECT symptoms_id AS id
  FROM public.cases
  WHERE deleted = FALSE and caseclassification != 'NO_CASE' and reportdate between ?fromDateTemp and ?toDateTemp)"
  querySymptoms = DBI::sqlInterpolate(sormas_db, sqlSymptoms, fromDateTemp=fromDate, toDateTemp=toDate) 
  symptoms = dbGetQuery(sormas_db, querySymptoms)
 ### loading person data
  ## only persons linked to cases or contacts (not events or event participant persons) are included in this export
  ## since the goal is to get pairs on infectors and infectees with corresponding onset dates to be used to estimate serial interval
  # case person
  sql_person_case = "SELECT id AS person_id, sex, birthdate_dd, birthdate_mm, birthdate_yyyy
                           FROM public.person
                           WHERE id IN (SELECT person_id AS id
                           FROM public.cases
                           WHERE deleted = FALSE AND caseclassification != 'NO_CASE' AND reportdate between ?fromDateTemp and ?toDateTemp)"
  query_person_case = DBI::sqlInterpolate(sormas_db, sql_person_case, fromDateTemp=fromDate, toDateTemp=toDate) 
  person_case = dbGetQuery(sormas_db, query_person_case)
  # contact person
  sql_person_contact = "SELECT id AS person_id, sex, birthdate_dd, birthdate_mm, birthdate_yyyy
                              FROM public.person 
                              WHERE id IN (SELECT person_id AS id
                              FROM public.contact
                              WHERE deleted = FALSE and caze_id IS NOT NULL and caze_id IN (SELECT id AS caze_id 
                              FROM public.cases
                              WHERE deleted = FALSE and caseclassification != 'NO_CASE' and reportdate between ?fromDateTemp and ?toDateTemp ))"
  query_person_contact = DBI::sqlInterpolate(sormas_db, sql_person_contact, fromDateTemp=fromDate, toDateTemp=toDate) 
  person_contact = dbGetQuery(sormas_db, query_person_contact)
  # row bind and keep distinct person id
  person = dplyr::bind_rows(person_case, person_contact) %>%
    dplyr::distinct(person_id, .keep_all = TRUE)                    
  
  # load region
  region = dbGetQuery(
    sormas_db,
    "SELECT id AS region_id, name AS region_name
    FROM public.region"
  ) 
  
  # load district
  district = dbGetQuery(
    sormas_db,
    "SELECT id AS district_id, name AS district_name
    FROM district"
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
  person = fixBirthDate(person) %>% # This method assign the birthdate of the person as first January of the year of birth. Improvement will follow
    dplyr::select(person_id, sex, date_of_birth, ) #dropping unused variables
  
  ## Obtaining dataset to the exported
  # The primary dataset to begin with is the contact since the goal is to export the contact data
  # A contact can be converted to a case having a report date that is not in the date interval
  # This may result to NA values for case_id_infector or case_id_infectee, thus such error records 
  # documented can be dropped. There is no validation in sormas to prevent this
  # Merging tables
  ret = contact %>%
    dplyr::filter(resultingcase_id != "NA")  %>%  #dropping contacts that did not resulted to a case
    dplyr::left_join(., case, by="case_id") %>%  # Merging contact and case data by source case or infector case id
    dplyr::mutate(case_uuid_infector = case_uuid, case_id_infector= case_id,  disease_infector = disease,  report_date_infector = report_date_case,
                  creation_date_case_infector = creation_date_case,  person_id_case_infector = person_id_case, region_id_case_infector = region_id_case,
                  district_id_case_infector = district_id_case, case_classification_infector = case_classification, case_origin_infector = case_origin,
                  symptoms_id_infector = symptoms_id, .keep = "unused" ) %>% # renaming variables by adding infector to them
    dplyr::left_join(., case, by = c( "resultingcase_id" = "case_id") ) %>%  # merging data with case table again by resulting case or infectee id
    dplyr::mutate(case_uuid_infectee = case_uuid,case_id_infectee = resultingcase_id,  disease_infectee = disease,  report_date_infectee = report_date_case,
                  creation_date_case_infectee = creation_date_case,  person_id_case_infectee = person_id_case, region_id_case_infectee = region_id_case,
                  district_id_case_infectee = district_id_case, case_classification_infectee = case_classification, case_origin_infectee = case_origin,
                  symptoms_id_infectee = symptoms_id, .keep = "unused" ) %>% # renaming variables by adding infectee to them
    dplyr::select(., -c(person_id_contact, case_uuid_infector, contact_uuid, case_uuid_infectee, disease_infectee, case_origin_infectee, 
                        region_id_case_infectee, district_id_case_infectee)) %>%  # dropping duplicates or unimportant varibles
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
    dplyr::filter((person_id_case_infectee != "NA") | (person_id_case_infector != "NA") ) %>%  # dropping records with person_id_case_infectee == NA, this can hapen when the report date of resulting case is not in time interval
    # or if the infectee person was infected by a case that is not among the selected cases, resuling in NA infector person id.
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
      # contact variables, rendering by time will be based on report date of contact
      contact_id, report_date_contact,  report_week_contact, report_month_contact, report_year_contact, contact_proximity,
      #infector variables, rendering by disease, region and district will be based on the infrastructure of the infector
      person_id_case_infector, case_id_infector, disease_infector, 
      report_date_infector, region_infector, district_infector, case_classification_infector, case_origin_infector, 
      sex_infector, onset_date_infector, age_at_report_infector,
      #infectee variables
      person_id_case_infectee, case_id_infectee, report_date_infectee, case_classification_infectee, sex_infectee,
      onset_date_infectee, age_at_report_infectee,
      #other variables
      serial_interval, incubation_Period
    )
  return(ret)
} 