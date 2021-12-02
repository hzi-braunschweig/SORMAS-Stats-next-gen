caseExport <- function(sormas_db, fromDate, toDate){
  # This function creates two tables as output.
  # The first table is a line listing of all cases reported in the defined
  # time period. This line listing of cases includes all the variables as
  # columns that are necessary for the creation of the SitRep, apart from
  # population and geo shapes data.
  # The second table contains the population and geo data.
  # It contains data on the population by agegroup and gender in each district.
  # The two tables are output separately, because each case corresponds to one 
  # district, but each district corredponds to multiple population counts
  # (by agegroup and gender). Therefore a line listing by case would be
  # impractical and also not very suitable for the computation of the 
  # subsequent parts of the situation report.
  
  ## Computing default time based on 365 days in the past if not provided by user
  if(missing(fromDate) | missing(toDate)){
    fromDate = as.character(Sys.Date() - delay) 
    toDate = as.character(Sys.Date()) 
  }

  ## Loading tables from SORMAS DB
  
  # Case table SQL query
  # (added vaccination status, because it could be interesting for a 
  # possible extension of the SitRep)
  queryCases <- paste0("SELECT  DISTINCT id AS case_id,
    district_id,
    region_id,
    caseclassification AS case_classification,
    EndOfIsolationReason AS case_end_of_iso_reason,
    reportdate AS report_date_case,
    disease,
    vaccinationstatus AS vaccination_status, 
    symptoms_id,
    person_id,
    hospitalization_id 
  
    FROM public.cases 
    
    WHERE deleted = FALSE 
    and caseclassification != 'NO_CASE'
    and reportdate between '", fromDate, "' and '", toDate, "' 
    and disease = 'CORONAVIRUS' 
                       ")
  cases <- dbGetQuery(sormas_db,queryCases)
  
  
  # Person table SQL query
  queryPersons <- paste0("SELECT DISTINCT id AS person_id,
    approximateAge AS age_person,
    sex AS sex_person,
    deathDate AS death_date_person,
    causeOfDeathDisease AS cause_of_death_disease_person

    FROM public.person

    WHERE id IN (SELECT person_id FROM public.cases
      WHERE deleted = FALSE 
      and caseclassification != 'NO_CASE'
      and reportdate between '", fromDate, "' and '", toDate, "' 
      and disease = 'CORONAVIRUS')
                         ")
  # Only persons with a person_id that also appears in a case 
  # are queried from the database
  persons <- dbGetQuery(sormas_db,queryPersons)
  
  # Symptoms table SQL query
  querySymptoms <- paste0("SELECT DISTINCT id AS symptoms_id,
    onsetDate AS onset_date
    
    FROM public.symptoms
    
    WHERE id IN (SELECT symptoms_id FROM public.cases
      WHERE deleted = FALSE 
      and caseclassification != 'NO_CASE'
      and reportdate between '", fromDate, "' and '", toDate, "' 
      and disease = 'CORONAVIRUS')
                          ")
  # Only symptoms with a symptoms_id that also appears in a case 
  # are queried from the database
  symptoms <- dbGetQuery(sormas_db, querySymptoms)
  
  # Hospitalizations table query 
  queryHospitalizations <- paste0("SELECT DISTINCT id AS hospitalization_id,
    admittedToHealthFacility AS admitted_to_health_facility,
    admissionDate AS admission_date,
    hospitalizationReason AS hospitalization_reason
    
    FROM public.hospitalization
    
    WHERE id in (SELECT hospitalization_id FROM public.cases
      WHERE deleted = FALSE 
      and caseclassification != 'NO_CASE'
      and reportdate between '", fromDate, "' and '", toDate, "' 
      and disease = 'CORONAVIRUS')
                            ")
  # Only phospitalizations with a hospitalization_id that also appears in a
  # case are queried from the database
  hospitalizations <- dbGetQuery(sormas_db, queryHospitalizations)
  
  # District table SQL query
  queryDistrict <- paste0("SELECT DISTINCT id AS district_id,
    name AS district_name
    
    FROM public.district
    
    WHERE archived = FALSE
                          ")
  districts <- dbGetQuery(sormas_db, queryDistrict)
  
  # Region table SQL query
  queryRegions <- paste0("SELECT DISTINCT id AS region_id,
    name AS region_name
    
    FROM public.region 
    
    WHERE archived = FALSE
                         ")
  regions <- dbGetQuery(sormas_db, queryRegions)
  
  # Populationdata table SQL query 
  queryPopulation <- paste0("SELECT DISTINCT id AS population_id,
                            district_id,
                            region_id, 
                            sex AS sex_population,
                            agegroup AS agegroup_population,
                            population
                            
                            FROM public.populationdata
                            ")
  population_data <- dbGetQuery(sormas_db, queryPopulation)
  
  # Merging population data with district names
  population_data <- population_data %>% 
    dplyr::left_join(., districts, by = 'district_id' )
  
  # Merging data from cases, persons, hospitalizations, districts and regions 
  # exports into one line listed DataFrame with every row being a case 
  line_list_cases <- cases %>% 
    dplyr::left_join(., persons, by = 'person_id') %>% 
    dplyr::left_join(., symptoms, by = 'symptoms_id') %>% 
    dplyr::left_join(., hospitalizations, by = 'hospitalization_id') %>% 
    dplyr::left_join(., districts, by = 'district_id') %>% 
    dplyr::left_join(., regions, by = 'region_id') %>% 
    dplyr::mutate(report_date_case = as.Date(format(report_date_case, "%Y-%m-%d")),
                  admission_date = as.Date(format(admission_date, "%Y-%m-%d")),
                  onset_date = as.Date(format(onset_date, "%Y-%m-%d"))) 
    
  
  ## Provisory geo shapes export:
  # geo_shapes data needs to be placed in data/geo folder and table key needs 
  # to be placed in data/key folder.
  
  # Loading geoshapes from folder, not part of SORMAS data for now
  # data source: https://gadm.org/download_country.html
  files_geo <- list.files('data/geo', full.names = TRUE)
  
  # Reading in geo_shapes data
  geo_shapes <- readRDS(files_geo) %>%
    dplyr::select(NAME_2, geometry) %>% 
    dplyr::rename(district_name_geo = NAME_2)
  
  ## Merging Population and geo data using a table key
  
  # Loading key table from folder
  files_key <- list.files('data/key', full.names = TRUE)
  
  # Reading in key table
  key <- readxl::read_xlsx(files_key)
  
  # Merging population data and geo_shapes data
  population_geo_data <- population_data %>%
    dplyr::left_join(., key, by = 'district_name') %>% # joining w/ the matching key
    dplyr::left_join(., geo_shapes, by = 'district_name_geo') # joining w/ the geo_shapes
  
  
  # Population & geo data is output in a separate DataFrame because each 
  # district corresponds to many different population numbers for
  # population groups by agegroup and sex.
  # The geo_shapes are merged onto the population data by district.
  population_geo_data <- population_geo_data %>% 
    dplyr::select(-population_id, -district_name_geo)
  
  # Return the geo output tables
  return(list(line_list_cases=line_list_cases, population_geo_data=population_geo_data))
}




