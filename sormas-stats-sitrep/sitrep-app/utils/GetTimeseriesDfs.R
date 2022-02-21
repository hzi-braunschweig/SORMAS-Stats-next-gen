GetTimeseriesDfs <- function(start_date, end_date){
  
  dates <- seq.Date(as.Date(start_date), as.Date(end_date), by = "day") # seq.Date includes both start date and end date
  
  ###### HARD CODED COMPLETE CATEGORIES CASECLASSIFICATION ######
  
  columns_cases <- c("date",
               "CASE_NOT_CLASSIFIED",
               "CASE_SUSPECT",
               "CASE_PROBABLE",
               "CASE_CONFIRMED",
               "CASE_CONFIRMED_NO_SYMPTOMS",
               "CASE_CONFIRMED_UNKNOWN_SYMPTOMS",
               "CASE_NO_CASE",
               "TOTAL_CONFIRMED_CASES")
  
  # Initialize data frame
  df0_cases <- data.frame(matrix(0, ncol = length(columns_cases), nrow = (as.integer(end_date-start_date) +1))) # +1 added because otherwise start date is not included 
  # Assign column names
  colnames(df0_cases) <- columns_cases
  # input id_district, name_district, id_region and name_region
  df0_cases$date <- dates
  
  
  ###### HARD CODED COMPLETE CATEGORIES ONSET ######
  
  columns_onset <- c("date",
                     "n")
  
  # Initialize data frame
  df0_onset <- data.frame(matrix(0, ncol = length(columns_onset), nrow = (as.integer(end_date-start_date) +1))) # +1 added because otherwise start date is not included 
  # Assign column names
  colnames(df0_onset) <- columns_onset
  # input id_district, name_district, id_region and name_region
  df0_onset$date <- dates
  
  
  ###### HARD CODED COMPLETE CATEGORIES REASON HOSPITALIZATION######
  
  columns_hospitalization <- c("date",
                     "REPORTED_DISEASE",
                     "OTHER",
                     "UNKNOWN")
  
  # Initialize data frame
  df0_hospitalization <- data.frame(matrix(0, ncol = length(columns_hospitalization), nrow = (as.integer(end_date-start_date) +1))) # +1 added because otherwise start date is not included 
  # Assign column names
  colnames(df0_hospitalization) <- columns_hospitalization
  # input id_district, name_district, id_region and name_region
  df0_hospitalization$date <- dates
  
  return(list("df_cases" = df0_cases,
              "df_onset" = df0_onset,
              "df_hospitalizations" = df0_hospitalization))
}
