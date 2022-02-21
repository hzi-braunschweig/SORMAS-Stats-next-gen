#' Get timeseries data 
#'
#' @param data_line_list Output of the ExportCaseLineList function. 
#'
#' @return Returns a dataframe with time series data for cases by reporting date,
#'  by symptom onset date and for hospitalizations by hospitalization date.
#' @export
#'
#' @examples
GetTimeseriesData <- function(data_line_list, start_date, end_date){
  # get timeseries base data frames
  df_list <- GetTimeseriesDfs(start_date = start_date, end_date = end_date)
  
  # timeseries data total confirmed cases per reporting date:
  # get base df for confirmed cases by reporting date
  base_df_cases <- df_list$df_cases
  # aggregate caseclassifications by reporting date
  caseclassifications_by_reporting_date <- AggregateCountsByDate(data_line_list = data_line_list,
                                                                 count_values = "caseclassification_case",
                                                                 by_date = "report_date_case",
                                                                 start_date = start_date,
                                                                 end_date = end_date)
  
  
}