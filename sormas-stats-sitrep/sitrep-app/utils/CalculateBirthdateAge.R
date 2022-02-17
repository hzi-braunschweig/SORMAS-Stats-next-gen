#' Calculate Age based on Birthdate and a reference date
#' 
#' @description This funciton calculates the age of a person at a certain
#' reference date based on the person's birth date.
#'
#' @param birthdate Must be a date in format yyyy-mm-dd: as.Date(format("%Y-%m-%d"))
#' @param reference_date Must be a date in format yyyy-mm-dd: as.Date(format("%Y-%m-%d")) 
#'
#' @return Returns an age in whole years, which is based on a rounded number of days.
#' @export
#'
#' @examples
CalculateBirthdateAge <- function(birthdate, reference_date){
  # compute exact age in days
  age_in_days <- as.numeric(base::difftime(reference_date,birthdate))
  # compute rounded age in years
  rounded_age_in_years <- base::round(age_in_days/365, digits = 0) 
  
  return(rounded_age_in_years)
}