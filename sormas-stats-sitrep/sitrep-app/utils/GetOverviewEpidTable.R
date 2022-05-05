#' GEt table of country level epidemic indicators overview
#'
#' @param epid_data Output of the GetEpidBase() function. 
#'
#' @return Returns an overview table of the following 6 indicators
#' on country level:
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
GetOverviewEpidTable <- function(epid_data){
  ## building country overview epid table
  overview_epid_table <- epid_data %>% 
    dplyr::summarise("Total cases" = sum(TOTAL_CONFIRMED_CASES),
                     "New cases" = sum(TOTAL_NEW_CONFIRMED_CASES),
                     "Total hospitalizations" = sum(HOSP_YES),
                     #"New hospitalizations" = sum(NEW_HOSP_YES),
                     "Total deaths" = sum(DEATH_EPIDEMIC_DISEASE)
                     #"New deaths" = sum(NEW_DEATH_EPIDEMIC_DISEASE)
                     )
  
  # returning table
  return(overview_epid_table)
}
