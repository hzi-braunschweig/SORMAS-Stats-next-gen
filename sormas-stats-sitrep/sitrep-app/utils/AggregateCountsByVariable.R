#' AggregateCountsByVariable
#'@description Aggregates the counts of the values of a variable A by another
#' variable B.
#'
#' @param data_line_list Line listed data exported from SORMAS in a data frame
#'   by the ExportCasesLineList() function.
#' @param count_variable Variable whose values are aggregated. 
#'   Defaults to "caseclassification_case".
#' @param by_variable Variable by which is the aggregation is done.
#' @return Returns a data frame where the rows correspond to the values of
#'   variable B and the columns correspond to the values of variable A.
#'   Defaults to "id_district".
#' @seealso [ExportCasesLineList()]
#' @export
#'
#' @examples
#' 
AggregateCountsByVariable <- function(data_line_list = case_data_line_list,
                                      count_values = "caseclassification_case",
                                      by_variable = "id_district"){
  
  # Warning to not use that this function is not suitable to generate time 
  # series data used for sitrep. 
  if (base::grepl("_date", by_variable, fixed = TRUE))
    warning("This function is not suitable to generate complete
            time series data, which is also used for the creation of the SitRep.
            Instead use the function AggregateCountsByDate for this purpose.")
  
  # Reshaping the data to an aggregation of the values of variable A by
  # the variable B                 
  aggregation <- data_line_list %>%
    dplyr::group_by(.data[[by_variable]]) %>%
    dplyr::count(.data[[count_values]])  %>%
    tidyr::pivot_wider(names_from = .data[[count_values]],
                       values_from = n) %>% 
    dplyr::arrange(.data[[by_variable]])
  
  # Returning the reshaped data
  return(aggregation)
}

