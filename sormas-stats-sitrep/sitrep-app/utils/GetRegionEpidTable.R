#' Get table of region level epidemic indicators
#'
#' @param epid_data Output of the GetEpidBase() function. 
#' 
#' @return Returns an overview table of the following 6 indicators
#' on regional level:
#'  - TOTAL CONFIRMED CASES
#'  - NEW CONFIRMED CASES
#'  - TOTAL HOSPITALIZATIONS
#'  - NEW HOSPITALIZATIONS
#'  - TOTAL DEATHS
#'  - NEW DEATHS
#' 
#' @seealso [GetEpidBase()]
#' @export
#'
#' @examples
GetRegionEpidTable <- function(epid_data, geographic_units){
  ## building region epid table
  region_epid_table <- epid_data %>% 
    dplyr::left_join(geographic_units,  by = "id_district") %>% 
    dplyr::group_by(name_region) %>% 
    dplyr::summarise("Total cases" = sum(TOTAL_CONFIRMED_CASES),
                     "New cases" = sum(TOTAL_NEW_CONFIRMED_CASES),
                     "Total hospitalizations" = sum(HOSP_YES),
                     #"New hospitalizations" = sum(NEW_HOSP_YES),
                     "Total deaths" = sum(DEATH_EPIDEMIC_DISEASE)
                     #"New deaths" = sum(NEW_DEATH_EPIDEMIC_DISEASE)
                     ) %>% 
    dplyr::rename(Region = name_region)

  # return regional epid table 
  return(region_epid_table)
}
