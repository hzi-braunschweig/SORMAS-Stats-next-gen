#' Get Base Timeseries Data Frames
#'
#' @param start_date Start date of the timeseries.
#' @param end_date  End date of the timeseries.
#'
#' @return Returns a list of 2 data frames.
#' - data frame w/ a "date" column and all categories of the
#' caseclassification variable and a column named "NA"
#' - data frame w/ a "date" column and all categories of the
#' reason_hospitalization variable and a column named "NA"
#' @export
#'
#' @examples
GetTimeseriesDfs <- function(start_date, end_date){
  
  dates <- seq.Date(as.Date(start_date), as.Date(end_date), by = "day") # seq.Date includes both start date and end date
  
  ###### HARD CODED COMPLETE CATEGORIES CASECLASSIFICATION ######
  
  columns_cases <- c("date",
               "NOT_CLASSIFIED",
               "SUSPECT",
               "PROBABLE",
               "CONFIRMED",
               "CONFIRMED_NO_SYMPTOMS",
               "CONFIRMED_UNKNOWN_SYMPTOMS",
               "NO_CASE",
               "CONFIRMED_CASES")
  
  # Initialize data frame
  df0_cases <- data.frame(matrix(0, ncol = length(columns_cases), nrow = (as.integer(end_date-start_date) +1))) # +1 added because otherwise start date is not included 
  # Assign column names
  colnames(df0_cases) <- columns_cases
  # input id_district, name_district, id_region and name_region
  df0_cases$date <- dates
  
  ###### HARD CODED COMPLETE CATEGORIES REASON HOSPITALIZATION######
  
  columns_hospitalization <- c("date",
                     "REPORTED_DISEASE",
                     "OTHER",
                     "UNKNOWN",
                     "NA",
                     "ISOLATION")
  
  # Initialize data frame
  df0_hospitalization <- data.frame(matrix(0, ncol = length(columns_hospitalization), nrow = (as.integer(end_date-start_date) +1))) # +1 added because otherwise start date is not included 
  # Assign column names
  colnames(df0_hospitalization) <- columns_hospitalization
  # input id_district, name_district, id_region and name_region
  df0_hospitalization$date <- dates
  
  return(list("df_cases" = df0_cases,
              "df_hospitalizations" = df0_hospitalization))
}
