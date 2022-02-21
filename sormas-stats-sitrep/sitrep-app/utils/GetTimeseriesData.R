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
  ## CONFIRMED CASES BY REPORT DATE
  
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
  
  cases_by_reporting_date_df <- dplyr::rows_update(base_df_cases,
                                                   caseclassifications_by_reporting_date,
                                                   by = "date")
  

  # aggregate confirmed, confirmed_no_symptoms and confirmed_unknown_symptoms
  cases_by_reporting_date_df <- cases_by_reporting_date_df %>% 
    dplyr::mutate(TOTAL_CONFIRMED = rowSums(across(starts_with("CONFIRMED")), na.rm = TRUE)) %>% 
    dplyr::rename_with(.cols = !date, function(x){paste0("REPORT_", x)})
  
  ## CONFIRMED CASES BY ONSET DATE
  # get base df for confirmed cases by onset date
  base_df_onset <- df_list$df_cases
  
  # aggregate caseclassification by onset date
  caseclassifications_by_onset_date <- AggregateCountsByDate(data_line_list = data_line_list,
                                                             count_values = "caseclassification_case",
                                                             by_date = "onset_date_symptoms",
                                                             start_date = start_date,
                                                             end_date = end_date)
  
  cases_by_onset_date_df <- dplyr::rows_update(base_df_cases,
                                               caseclassifications_by_onset_date,
                                               by = "date")
  
  
  # aggregate confirmed, confirmed_no_symptoms and confirmed_unknown_symptoms
  cases_by_onset_date_df <- cases_by_onset_date_df %>% 
    dplyr::mutate(TOTAL_CONFIRMED = rowSums(across(starts_with("CONFIRMED")), na.rm = TRUE)) %>% 
    dplyr::rename_with(.cols = !date, function(x){paste0("ONSET_", x)})
  
  
  ## HOSPITALIZATIONS BY HOSPITALIZATION DATE
  
  # get base df fot confirmed cases by onset date
  base_df_hosp <- df_list$df_hospitalizations
  
  # aggregate hospitalizations by admission_date_hospitalization
  hospitalizations_by_admission_date <- AggregateCountsByDate(data_line_list = case_data_line_list,
                                                              count_values = "reason_hospitalization",
                                                              by_date = "admission_date_hospitalization",
                                                              start_date = start_date,
                                                              end_date = end_date)
  
  hospitalizations_by_admission_date_df <- dplyr::rows_update(base_df_hosp,
                                                              hospitalizations_by_admission_date,
                                                              by = "date") %>% 
    dplyr::rename_with(.cols = !date, function(x){paste0("ADMISSION_", x)})
  
  # build output timeseries data frame
  
  timeseries_df <- cases_by_reporting_date_df %>% 
    dplyr::left_join(cases_by_onset_date_df, by = "date") %>% 
    dplyr::left_join(hospitalizations_by_admission_date_df, by = "date")
  
  
  return(timeseries_df)
}