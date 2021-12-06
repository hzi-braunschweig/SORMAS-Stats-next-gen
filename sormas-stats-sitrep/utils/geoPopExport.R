geoPopExport <- function(sormas_db, fromDate, toDate){
  # This function outputs a table containing population and geo data.
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
  
  # District table SQL query
  queryDistrict <- paste0("SELECT DISTINCT id AS district_id,
    name AS district_name
    
    FROM public.district
    
    WHERE archived = FALSE
                          ")
  districts <- dbGetQuery(sormas_db, queryDistrict)
  
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
  
  ## Merging geo data and sormas district names using a table key
  
  # Loading key table from folder
  files_key <- list.files('data/key', full.names = TRUE)
  
  # Reading in key table
  key <- readxl::read_xlsx(files_key)
  
  # Merging key and geo_shapes data
  geo_data <- key %>%
    dplyr::left_join(., geo_shapes, by = 'district_name_geo') # joining w/ the matching key
  
  # Population and geo data are output in separate DataFrames to avoid
  # unnecessary repetition of data.
  
  return(list(population_data=population_data, geo_data = geo_data))
}