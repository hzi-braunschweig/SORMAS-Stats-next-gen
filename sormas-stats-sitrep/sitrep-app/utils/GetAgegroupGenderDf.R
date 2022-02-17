#' Get data frame with complete districts as rows and 
#' complete categories as columns
#' 
#' @description This function creates a data frame with all
#' neceassary agegroups as rows and all necessary genders as 
#' columns. 
#' 
#' @return Returns an empty dataframe with all agegroups (0-80, 5 year steps)
#' as rows, plus an unknown agegroup.
#' The columns of the data frame are the genders.
#' 
#' @export
#'
#' @examples
#' 
GetAgegroupGenderDf <- function(){
  
  genders <- c("MALE",
                  "FEMALE",
                  "OTHER",
                  "UNKNOWN")
  
  agegroups <- c("0-4",
               "5-9",
               "10-14",
               "15-19",
               "20-24",
               "25-29",
               "30-34",
               "35-39",
               "40-44",
               "45-49",
               "50-54",
               "55-59",
               "60-64",
               "65-69",
               "70-74",
               "75-79",
               "80+",
               "UNKNOWN")
  
  # initialize data frame
  df0 <- data.frame(matrix(0, ncol = length(genders), nrow = length(agegroups)))
  
  # Assign column and row names
  row.names(df0) <- agegroups
  colnames(df0) <- genders
  
  return(df0)
  
}