#' Display table of region level epidemic indicators
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
DisplayRegionEpidTable <- function(epid_data){
  ## building region epid table
  region_epid_table <- epid_data %>% 
    dplyr::group_by(name_region) %>% 
    dplyr::summarise("Total cases" = sum(TOTAL_CONFIRMED_CASES),
                     "New cases" = sum(TOTAL_NEW_CONFIRMED_CASES),
                     "Total hospitalizations" = sum(HOSP_YES),
                     "New hospitalizations" = sum(NEW_HOSP_YES),
                     "Total deaths" = sum(DEATH_EPIDEMIC_DISEASE),
                     "New deaths" = sum(NEW_DEATH_EPIDEMIC_DISEASE)) %>% 
    dplyr::rename(Region = name_region)

  # displying regional epid table 
  flextable::flextable(region_epid_table)
  
    }
