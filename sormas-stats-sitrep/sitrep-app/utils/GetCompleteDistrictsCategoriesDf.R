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
#' 
#' @seealso [GetEpidBase()]
#' 
#' @export
#'
#' @examples
GetCompleteDistrictsCategoriesDf <- function(){
  
  ####### HARD CODED COMPLETE VARIABLE CATEGORIES #########
  
  district_columns <- c("id_district","name_district")
  
  region_columns <- c("id_region", "name_region")
  
  case_columns <- c("CASE_NOT_CLASSIFIED",
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
                    "TOTAL_NEW_CONFIRMED_CASES")
  
  hospitalization_columns <- c("HOSP_YES",
                               "HOSP_NO",
                               "HOSP_UNKNOWN",
                               "HOSP_NA",
                               "NEW_HOSP_YES",
                               "NEW_HOSP_NO",
                               "NEW_HOSP_UNKNOWN",
                               "NEW_HOSP_NA")
  
  death_columns <- c("DEATH_EPIDEMIC_DISEASE",
               "DEATH_OTHER_CAUSE",
               "DEATH_NA",
               "NEW_DEATH_EPIDEMIC_DISEASE",
               "NEW_DEATH_OTHER_CAUSE",
               "NEW_DEATH_NA")
  
  
  columns <- c(district_columns,
               region_columns,
               case_columns,
               hospitalization_columns,
               death_columns)
  
  # Initialize data frame
  df0 <- data.frame(matrix(0, ncol = length(columns), nrow = nrow(geographic_units)))
  # Assign column names
  colnames(df0) <- columns
  # input id_district, name_district, id_region and name_region
  df0$id_district <- geographic_units$id_district # geographic_units$id_district
  df0$name_district <- geographic_units$name_district
  df0$id_region <- geographic_units$id_region #   geographic_units$id_region
  df0$name_region <- geographic_units$name_region
  
  return(df0)
}
