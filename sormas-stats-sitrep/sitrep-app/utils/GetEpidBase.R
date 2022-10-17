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
GetEpidBase <- function(data_line_list){
  
  # TO RENAME ALL COLUMNS EXCEPT ONE  
  # dplyr::rename_with(.cols = !id_district, function(x){paste0("NEW_", x)})
  # dplyr::rename_with(.cols = !id_district, function(x){paste0("TOT_", x)})
  
  # Get base data frame with all districs as rows and the comlplete categories
  # as columns
  empty_df <- GetCompleteDistrictsCategoriesDf()
  
  
  # Get dataframe with base indicators for cases
  
  ## Get total cases per district
  total_cases_district <- GetTotalCountsPerDistrict(data_line_list = data_line_list,
                                                    variable = "caseclassification_case") %>% 
    dplyr::rename_with(.cols = !id_district, function(x){paste0("CASE_", x)})
  
  ## Get new cases per district based on reporting date
  new_cases_district <- GetNewCountsPerDistrict(data_line_list = data_line_list,
                                                variable = "caseclassification_case",
                                                date_type = "report_date_case",
                                                toDate) %>% 
    dplyr::rename_with(.cols = !id_district, function(x){paste0("NEW_CASE_", x)})

  ## Join on id_district and summarize confirmed cases
  epid_base_cases <- total_cases_district %>% 
    dplyr::full_join(new_cases_district, by = "id_district") 
  
  ###########################
  
  # Get dataframe with base indicators for hospitalizations
  
  ## Get total hospitalizations per district
  total_hospitalizations_district <- GetTotalCountsPerDistrict(data_line_list = data_line_list,
                                                               variable = "admitted_to_health_facility_hospitalization") %>% 
    dplyr::rename_with(.cols = !id_district, function(x){paste0("HOSP_", x)})
    
  ## Get new hospitalizations per district
  new_hospitalizations_district <- GetNewCountsPerDistrict(data_line_list = data_line_list,
                                                           variable = "admitted_to_health_facility_hospitalization",
                                                           date_type = "admission_date_hospitalization",
                                                           toDate) %>% 
    dplyr::rename_with(.cols = !id_district, function(x){paste0("NEW_HOSP_", x)})
  
  ## Join on id_district
  epid_base_hospitalizations <- total_hospitalizations_district %>% 
    dplyr::full_join(new_hospitalizations_district, by = "id_district")
  
  ###########################
  
  # Get dataframe with base indicators for deaths

  ## Get total deaths per district
  total_deaths_district = GetTotalCountsPerDistrict(data_line_list = data_line_list,
                                                    variable = "cause_of_death_person") %>% 
    dplyr::rename_with(.cols = !id_district, function(x){paste0("DEATH_", x)})
  ## Get new deaths per district
  new_deaths_district = GetNewCountsPerDistrict(data_line_list = data_line_list,
                                                variable = "cause_of_death_person",
                                                date_type = "death_date_person",
                                                toDate) %>% 
    dplyr::rename_with(.cols = !id_district, function(x){paste0("NEW_DEATH_", x)})
  ## Join on id_district
  epid_base_deaths <- total_deaths_district %>% 
    dplyr::full_join(new_deaths_district, by = "id_district")
  
  ###########################
  
  # Join the 3 dataframes for cases, hospitalizations and deaths
  # on id_district (full join). 
  # Then summarize total confirmed and new confirmed cases
  epid_base <- epid_base_cases %>% 
    dplyr::full_join(epid_base_hospitalizations, by ="id_district") %>% 
    dplyr::full_join(epid_base_deaths, by = "id_district") %>% 
    dplyr::mutate(TOTAL_CONFIRMED_CASES = rowSums(across(starts_with("CASE_CONFIRMED")), na.rm = TRUE)) %>%
    dplyr::mutate(TOTAL_NEW_CONFIRMED_CASES = rowSums(across(starts_with("NEW_CASE_CONFIRMED")), na.rm = TRUE)) %>% 
    dplyr::select(-any_of(c("CASE_n", "NEW_CASE_n", "HOSP_n", "NEW_HOSP_n", "DEATH_n", "NEW_DEATH_n")))
  
 return(epid_base) 
}