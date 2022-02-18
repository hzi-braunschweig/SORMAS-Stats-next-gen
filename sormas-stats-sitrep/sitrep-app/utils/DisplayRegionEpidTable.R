#' Display table of region level epidemic indicators
#'
#' @return
#' @export
#'
#' @examples
DisplayRegionEpidTable <- function(){
  ## building region epid table
  region_epid_table <- epid_base %>% 
    dplyr::group_by(name_region) %>% 
    dplyr::summarise("Total cases" = sum(TOTAL_CONFIRMED_CASES),
                     "New cases" = sum(TOTAL_NEW_CONFIRMED_CASES),
                     "Total hospitalizations" = sum(HOSP_YES),
                     "New hospitalizations" = sum(NEW_HOSP_YES),
                     "Total deaths" = sum(DEATH_EPIDEMIC_DISEASE),
                     "New deaths" = sum(NEW_DEATH_EPIDEMIC_DISEASE)) %>% 
    dplyr::rename(Region = name_region)

    
  flextable::flextable(region_epid_table)
  
    }
