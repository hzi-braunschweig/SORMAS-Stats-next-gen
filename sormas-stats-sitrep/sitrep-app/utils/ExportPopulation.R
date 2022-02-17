#' Export Population
#'
#' @param sormas_db Connection to SORMAS data base.
#'
#' @return Returns a list contatinig three tables.
#'  First a table containing population data from SORMAS.
#'  It contains data on the population by agegroup and gender in each district.
#'  Additionally the list contains a complete table of all districts and
#'  a complete table of all regions.
#' 
#' @seealso [ExportCaseLineList()] and [ExportGeoshapes()]
#' 
#' @export
#'
#' @examples
ExportPopulation <- function(sormas_db){
  
  # Populationdata table SQL query 
  queryPopulation <- paste0("SELECT DISTINCT id AS id_population,
                            district_id AS id_district,
                            region_id AS id_region, 
                            sex AS sex_population,
                            agegroup AS agegroup_population,
                            population
                            
                            FROM public.populationdata
                            ")
  population_data <- DBI::dbGetQuery(sormas_db, queryPopulation)
  
  # District table SQL query
  queryDistrict <- paste0("SELECT DISTINCT id AS id_district,
    name AS name_district,
    region_id AS id_region
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
  
  
  # Join districts with regions
  geographic_units <- districts %>% 
    dplyr::full_join(regions, by = "id_region")
  
  # Merging population data with district and region names
  population_data <- population_data %>% 
    dplyr::left_join(., districts, by = 'id_district' ) %>% 
    dplyr::select(-id_region.y) %>% 
    dplyr::rename(id_region = id_region.x) %>% 
    dplyr::left_join(., regions, by = 'id_region')
  
  #return output table
  return(list("population_data"=population_data,
              "geographic_units" = geographic_units))
}