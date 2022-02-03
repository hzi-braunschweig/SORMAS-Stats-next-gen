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
GetEpidBase <- function(data_line_list = case_data_line_list,
                        toDate = toDate){
  
  # Define latest date
  latest_date <- "2021-04-10" # as.character(as.Date(toDate)-1) #  example 
  
  # Get dataframe with base indicators for cases
  
  ## Get total cases per district
  total_cases_district <- AggregateCountsByVariable(data_line_list = data_line_list,
                                                    count_values = "caseclassification_case",
                                                    by_variable = "id_district") %>% 
    dplyr::rename_with(.cols = !id_district, function(x){paste0("TOT_", x)})
  
  ## Get new cases per district
  ### First filter data for new cases based on reporting date
  new_cases_district <- data_line_list %>% 
    dplyr::filter(report_date_case == latest_date)  %>% 
    AggregateCountsByVariable(data_line_list = .,
                              count_values = "caseclassification_case",
                              by_variable = "id_district") %>%
    dplyr::rename_with(.cols = !id_district, function(x){paste0("NEW_", x)})
  
  ## Join on id_district and summarize confirmed cases
  
  epid_base_cases <- total_cases_district %>% 
    dplyr::full_join(new_cases_district) %>% 
    dplyr::mutate(TOTAL_CONFIRMED = rowSums(across(starts_with("TOT_CONFIRMED")), na.rm = TRUE)) %>% 
    dplyr::mutate(TOTAL_NEW = rowSums(across(starts_with("NEW_CONFIRMED")), na.rm = TRUE))
  
  ###########################
  
  # Get dataframe with base indicators for hospitalizations
  
  ## Get total hospitalizations per district
  
  ## Get new hospitalizations per district
  
  ## Join on id_district
  
  ###########################
  
  # Get dataframe with base indicators for deaths
  
  ## Get total deaths per district
  
  ## Get new deaths per district
  
  ## Join on id_district
  
  ###########################
  
  # Join the 3 dataframes for cases, hospitalizations and deaths
  # on id_district (full join)
  
  
 return(epid_base) 
}