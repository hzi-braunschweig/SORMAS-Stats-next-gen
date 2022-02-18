#' Display table of country level epidemic indicators overview
#'
#' @return
#' @export
#'
#' @examples
DisplayOverviewEpidTable <- function(){
  ## building country overview epid table
  overview_epid_table <- epid_base %>% 
    dplyr::summarise("Total cases" = sum(TOTAL_CONFIRMED_CASES),
                     "New cases" = sum(TOTAL_NEW_CONFIRMED_CASES),
                     "Total hospitalizations" = sum(HOSP_YES),
                     "New hospitalizations" = sum(NEW_HOSP_YES),
                     "Total deaths" = sum(DEATH_EPIDEMIC_DISEASE),
                     "New deaths" = sum(NEW_DEATH_EPIDEMIC_DISEASE))
  
  
  flextable::flextable(overview_epid_table)
  
}
