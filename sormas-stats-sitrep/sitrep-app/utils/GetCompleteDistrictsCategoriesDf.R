#' Get data frame with complete districts as rows and 
#' complete categories as columns
#' 
#' @description This function creates a data frame with all 
#' the columns needed for the creation of the situation report. 
#' This includes all factor levels for the following variables:
#' caseclassification_case,
#' admitted_to_health_facility_hospitalization,
#' cause_of_death_person
#' 
#' For all of those variables the News and Totals are given
#'
#' @return Empty data frame with all all districts as rows and all 
#' necessary factor levels as column names.
#' @export
#'
#' @examples
GetCompleteDistrictsCategoriesDf <- function(){
  
  # Initialize data frame
  df0 <- data.frame(matrix(0, ncol = 31, nrow = nrow(districts)))
  
  
  ####### HARD CODED COMPLETE VARIABLE CATEGORIES #########
  
  colnames(df0) <- c("id_district",
                     "CASE_NOT_CLASSIFIED",
                     "CASE_SUSPECT",
                     "CASE_PROBABLE",
                     "CASE_CONFIRMED",
                     "CASE_CONFIRMED_NO_SYMPTOMS",
                     "CASE_CONFIRMED_UNKNOWN_SYMPTOMS",
                     "CASE_NO_CASE",
                     "TOTAL_CONFIRMED_CASES",
                     "NEW_CASE_NOT_CLASSIFIED",
                     "NEW_CASE_SUSPECT",
                     "NEW_CASE_PROBABLE",
                     "NEW_CASE_CONFIRMED",
                     "NEW_CASE_CONFIRMED_NO_SYMPTOMS",
                     "NEW_CASE_CONFIRMED_UNKNOWN_SYMPTOMS",
                     "NEW_CASE_NO_CASE",
                     "TOTAL_NEW_CONFIRMED_CASES",
                     "HOSP_YES",
                     "HOSP_NO",
                     "HOSP_UNKNOWN",
                     "HOSP_NA",
                     "NEW_HOSP_YES",
                     "NEW_HOSP_NO",
                     "NEW_HOSP_UNKNOWN",
                     "NEW_HOSP_NA",
                     "DEATH_EPIDEMIC_DISEASE",
                     "DEATH_OTHER_CAUSE",
                     "DEATH_NA",
                     "NEW_DEATH_EPIDEMIC_DISEASE",
                     "NEW_DEATH_OTHER_CAUSE",
                     "NEW_DEATH_NA"
    
  )

  ####### COMPLETE DISTRICTS ##############################
  
  ## Use geoshapes data to obtain a complete list of district ids
  df0$id_district <- districts$id_district
  
  return(df0)
}