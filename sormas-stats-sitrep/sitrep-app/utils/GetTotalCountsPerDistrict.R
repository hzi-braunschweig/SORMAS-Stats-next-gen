#' Get Total Counts per District
#' 
#' @description Gets the total counts of a variable in the output table of the 
#'  ExportCaseLineList function on district level.
#'
#' @param variable Column in the case_data_line_list output of the 
#'  ExportCaseLineList function.
#'
#' @return Returns a data frame with the total counts of the chosen variable 
#'  per district.
#' @export 
#'
#' @examples
#' 
GetTotalCountsPerDistrict <- function(data_line_list,
                                      variable = "caseclassification_case"){
  
  total_counts_district <- AggregateCountsByVariable(data_line_list = data_line_list,
                                                     count_values = variable,
                                                     by_variable = "id_district") %>% 
    dplyr::mutate(across(everything(), ~replace_na(.,0))) # %>% 
    # dplyr::select(-contains("n"))
  
  return(total_counts_district)
}