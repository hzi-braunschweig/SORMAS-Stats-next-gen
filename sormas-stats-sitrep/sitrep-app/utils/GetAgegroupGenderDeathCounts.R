#' Get Death counts per Agegroup and Gender
#'
#' @return Returns a data frame containing death counts per agegroup
#' (rows) and genders (columns)
#' 
#' @seealso [GetAgegroupGenderDf()]
#' @export
#'
#' @examples
GetAgegroupGenderDeathCounts() <- function(){
  # get empty agegroup gender data frame
  empty_df <- GetAgegroupGenderDf()
  
  # compute death counts per age group and gender
  death_counts <-   case_data_line_list %>% 
    dplyr::group_by(age_group = cut(death_age, breaks = c(seq(-1,79,5),150))) %>% 
    dplyr::count()
  
  # assign death counts to age / gender groups via function
  
  # update empty_data frame with the death counts
  
    
}