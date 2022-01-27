geoshapesExport <- function(sormas_db){
  # This function outputs a table containing geoshapes of each district.
 
  ## Provisory geo shapes export: 
  # Loading geoshapes from folder, not part of SORMAS data for now
  # geo_shapes data needs to be placed in data/geo folder and table key needs 
  # to be placed in data/key folder.
  # data source: https://gadm.org/download_country.html
  
  files_geo <- list.files('data/geo', full.names = TRUE)
  
  # Reading in geo_shapes data
  geoshapes_data <- readRDS(files_geo) %>%
    dplyr::select(NAME_1, NAME_2, geometry) %>% 
    dplyr::rename(name_region_geo = NAME_1, name_district_geo = NAME_2)
  
  ## Merging geo data and sormas district names using a table key
  
  # Loading key table from folder
  files_key <- list.files('data/key', full.names = TRUE)
  
  # Reading in key table
  key <- readxl::read_xlsx(files_key)
  
  # Merging key and geo_shapes data
  geoshapes_data <- key %>%
    dplyr::left_join(., geoshapes_data, by = 'name_district_geo') # joining w/ the matching key
  
  #returning output table
  return(geoshapes_data)
}