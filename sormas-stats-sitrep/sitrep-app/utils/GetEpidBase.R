#' Get Epidemic Indicator Base Table
#' 
#' @description Provides an epidemic indicator base table. The columns of the 
#'   table are the following variables:
#'   -Total cases
#'   -New cases
#'   -Total hospitalizations
#'   -New hospitalizations
#'   -Total deaths
#'   -New Deaths
#'   (- 7 day incidence to be added)
#'   (- Change in 7 day incidence to be added)
#'   The rows of the table are districts. 
#'
#'
#'
#' @param data_line_list Line listed data exported from SORMAS using the 
#'   ExportCaseLineList() function.
#'
#' @return Returns the epidemic dynamic base table
#' @export
#' 
#' @seealso [AggregateCountsByVariable()]
#'
#' @examples
GetEpidBase <- function(data_line_list = case_data_line_list){
  
  # TO RENAME ALL COLUMNS EXCEPT ONE  
  # dplyr::rename_with(.cols = !id_district, function(x){paste0("NEW_", x)})
  # dplyr::rename_with(.cols = !id_district, function(x){paste0("TOT_", x)})
  
  # Get base data frame with all districs as rows and the comlplete categories
  # as columns
  epid_base_df <- GetCompleteDistrictsCategoriesDf()
  
  
  # Get dataframe with base indicators for cases
  
  ## Get total cases per district
  total_cases_district <- GetTotalCountsPerDistrict(variable = "caseclassification_case")
  
  ## Get new cases per district based on reporting date
  new_cases_district <- GetNewCountsPerDistrict(variable = "caseclassification_case",
                                                date_type = "report_date_case")

  ## Join on id_district and summarize confirmed cases
  epid_base_cases <- total_cases_district %>% 
    dplyr::full_join(new_cases_district, by = "id_district") %>% 
    dplyr::mutate(across(everything(), ~tidyr::replace_na(.x, 0))) %>%
    dplyr::mutate(TOTAL_CONFIRMED_CASES = rowSums(across(starts_with("TOT_CONFIRMED")), na.rm = TRUE)) %>% 
    dplyr::mutate(TOTAL_NEW_CONFIRMED_CASES = rowSums(across(starts_with("NEW_CONFIRMED")), na.rm = TRUE))
  
  ###########################
  
  # Get dataframe with base indicators for hospitalizations
  
  ## Get total hospitalizations per district
  total_hospitalizations_district <- GetTotalCountsPerDistrict(variable = "admitted_to_health_facility_hospitalization")
    
  ## Get new hospitalizations per district
  new_hospitalizations_district <- GetNewCountsPerDistrict(variable = "admitted_to_health_facility_hospitalization",
                                                           date_type = "admission_date_hospitalization")
  
  ## Join on id_district
  epid_base_hospitalizations <- total_hospitalizations_district %>% 
    dplyr::full_join(new_hospitalizations_district, by = "id_district")
  
  ###########################
  
  # Get dataframe with base indicators for deaths

  ## Get total deaths per district
  total_deaths_district = GetTotalCountsPerDistrict(variable = "cause_of_death_person")
  ## Get new deaths per district
  new_deaths_district = GetNewCountsPerDistrict(variable = "cause_of_death_person",
                                                date_type = "death_date_person")
  ## Join on id_district
  epid_base_deaths <- total_deaths_district %>% 
    dplyr::full_join(new_deaths_district, by = "id_district")
  
  ###########################
  
  # Join the 3 dataframes for cases, hospitalizations and deaths
  # on id_district (full join)
  
  
 return(epid_base) 
}