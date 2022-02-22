#' Get New Counts per District
#' 
#' @description Get the new counts of a variable of the 
#'  data frame output of the ExportCaseLineList function based on the latest date.
#'  The latest date is defined as yesterday.
#'
#' @param variable Column in the case_data_line_list output of the 
#'  ExportCaseLineList function.
#'
#' @return Returns a data frame with the new counts of the chosen variable 
#'  per district based on the latest date.
#' @export
#'
#' @examples
GetNewCountsPerDistrict <- function(data_line_list,
                                    variable = "caseclassification_case",
                                    date_type = "report_date_case",
                                    toDate){
  
  # Define latest date
  latest_date <- as.character(as.Date(toDate)-1) 
  
  ### First filter data for new counts on date type
  new_counts_district <- data_line_list %>% 
    dplyr::filter(.data[[date_type]] %in% latest_date)  %>%
    AggregateCountsByVariable(data_line_list = .,
                              count_values = variable,
                              by_variable = "id_district")  %>% 
    dplyr::mutate(across(everything(), ~replace_na(.,0))) # %>% 
    # dplyr::select(-contains("n"))
  
  return(new_counts_district)
}