#' AggregateCountsByDate
#'@description Aggregates the counts of the values of a variable by date.
#'  Returns a complete set of time series data.
#'
#' @param data_line_list Line listed data exported from SORMAS in a data frame
#'   by the ExportCasesLineList() function.
#' @param count_variable Variable whose values are aggregated. 
#'   Defaults to "caseclassification_case".
#' @param date_variable Date variable by which the aggregation is done.
#' @return Returns a data frame where each row corresponds to a date 
#'   and the columns correspond to the values of count_variable.
#'   
#' @seealso [ExportCasesLineList()]
#' @export
#'
#' @examples
#' 
AggregateCountsByDate <- function(data_line_list = case_data_line_list,
                                  count_values = "caseclassification_case",
                                  by_date = "report_date_case",
                                  start_date = fromDate,
                                  end_date = toDate){
  
  # Error if date_variable is not a date variable.
  base::stopifnot("This function only aggregates by date variables."=
                  class(data_line_list[[by_date]])=="Date")
  
  # Build data frame with one column of dates
  date_df <- data.frame(date = seq.Date(as.Date(start_date),
                                        as.Date(end_date),
                                        by = "day")) 
  # %>% 
    # dplyr::mutate(across(everything(), ~replace_na(.,0)))
  
  # Get data aggregated by date
  date_aggregation <- data_line_list %>% 
    dplyr::group_by(.data[[by_date]]) %>% 
    dplyr::count(.data[[count_values]]) %>% 
    tidyr::pivot_wider(names_from = .data[[count_values]],
                       names_prefix= (base::gsub("date.*", "", by_date)),
                       values_from = n) %>%
    # dplyr::mutate(across(everything(), ~replace_na(.,0))) %>% 
    dplyr::rename(date = .data[[by_date]])  %>% 
    tidyr::drop_na(date)
  
  # Join date_aggregation with date_df
  date_aggregation <- date_df %>% 
    dplyr::left_join(., date_aggregation, by = "date")
  
  # replace NAs with 0 
  date_aggregation <- date_aggregation %>% 
    dplyr::mutate(across(-date, ~replace_na(.,0)))
  
  # Returning the reshaped data
  return(date_aggregation)
}

