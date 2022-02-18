#' Assign counts to agegroup and gender
#'
#' @age_variable "report_date_age", "admission_date_age", "onset_date_age"
#'  or "death_age".
#'  Defaults to "report_date_age".
#'
#' @return returns a data frame where each row is an agegroup 
#' and each column is a gender
#' 
#' @export
#' 
#' @seealso [GetAgegroupGenderDf()]
#'
#' @examples
GetDeathCountsPerAgegroupGender <- function(){
  
  # get agegroup gender data frame 
  empty_df <- GetAgegroupGenderDf()
  
  # get agegroup counts by gender
  # and chaning string in age_group variable to match values in empty_df
  agegroup_gender_counts <- case_data_line_list %>% 
    dplyr::group_by(sex_person, age_group = cut(death_age, breaks = c(seq(0,80,5),150))) %>% 
    dplyr::count(cause_of_death_person) %>% 
    dplyr::filter(cause_of_death_person == "EPIDEMIC_DISEASE") %>%
    dplyr::select(-cause_of_death_person) %>% 
    dplyr::mutate(age_group = as.character(age_group)) %>% 
    dplyr::mutate(age_group = tidyr::replace_na(age_group, "UNKNOWN"))
    
  # join agegroup_gender_counts with empty_df
  agegroup_gender_df <- empty_df %>% 
    dplyr::left_join(agegroup_gender_counts, by = c("age_group"="age_group", "gender"="sex_person")) %>% 
    dplyr::mutate(n = replace_na(n,0))
  

  return(agegroup_gender_df)
}
