#' Get New Counts per District
#' 
#' @description Get the new counts of a variable of the 
#'  data frame output of the ExportCaseLineList function based on the latest date.
#'  The latest date is defined as the last 24 hours.
#'
#' @param variable Column in the case_data_line_list output of the 
#'  ExportCaseLineList function.
#'
#' @return Returns a data frame with the new counts of the chosen variable 
#'  per district based on the latest date.
#' @export
#'
#' @examples
GetNewCountsPerDistrict <- function(variable = "caseclassification_case", date_type = "report_date_case"){
  
  # Define latest date
  latest_date <- c(Sys.Date(), Sys.Date()-1) # as.character(as.Date(toDate)-1) #  example 
  
  ### First filter data for new counts on date type
  new_counts_district <- case_data_line_list %>% 
    dplyr::filter(.data[[date_type]] %in% latest_date)  %>%
    AggregateCountsByVariable(data_line_list = .,
                              count_values = variable,
                              by_variable = "id_district") %>%
    dplyr::rename_with(.cols = !id_district, function(x){paste0("NEW_", x)})
  
  return(new_counts_district)
}