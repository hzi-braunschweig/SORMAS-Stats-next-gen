populationExport <- function(sormas_db){
  # This function outputs a table containing population data.
  # It contains data on the population by agegroup and gender in each district.
  
  # District table SQL query
  queryDistrict <- paste0("SELECT DISTINCT id AS district_id,
    name AS district_name
    
    FROM public.district
    
    WHERE archived = FALSE
                          ")
  districts <- DBI::dbGetQuery(sormas_db, queryDistrict)
  
  # Populationdata table SQL query 
  queryPopulation <- paste0("SELECT DISTINCT id AS population_id,
                            district_id,
                            region_id, 
                            sex AS sex_population,
                            agegroup AS agegroup_population,
                            population
                            
                            FROM public.populationdata
                            ")
  population_data <- DBI::dbGetQuery(sormas_db, queryPopulation)
  
  # Merging population data with district names
  population_data <- population_data %>% 
    dplyr::left_join(., districts, by = 'district_id' )
  
  #return output table
  return(population_data)
}