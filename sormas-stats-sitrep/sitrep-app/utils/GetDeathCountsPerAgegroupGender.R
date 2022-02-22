#' Assign counts to agegroup and gender
#'
#' @param data_line_list Output of the GetEpidBase() function.
#' @param cause_of_death Cause of death:
#' -"EPIDEMIC_DISEASE"
#' -"OTHER_CAUSE"
#' -NA
#'
#' @return returns a data frame with the three columns "gender", "age_group"
#'  and "n". "n" is the number of deaths for each population group 
#'  by age and gender.
#'  Each row is a population group by age and gender.
#' 
#' @export
#' 
#' @seealso [GetAgegroupGenderDf()]
#'
#' @examples
GetDeathCountsPerAgegroupGender <- function(data_line_list,
                                            cause_of_death = "EPIDEMIC_DISEASE"){
  
  # get agegroup gender data frame 
  empty_df <- GetAgegroupGenderDf()
  
  # get agegroup counts by gender
  # and chaning string in age_group variable to match values in empty_df
  agegroup_gender_counts <- data_line_list %>% 
    dplyr::group_by(sex_person, age_group = cut(death_age, breaks = c(seq(0,80,5),Inf),
                                                include.lowest = TRUE,
                                                right = FALSE)) %>% 
    dplyr::count(cause_of_death_person) %>% 
    dplyr::filter(cause_of_death_person == cause_of_death) %>%
    dplyr::select(-cause_of_death_person) %>% 
    dplyr::mutate(age_group = as.character(age_group)) %>% 
    dplyr::mutate(age_group = tidyr::replace_na(age_group, "UNKNOWN"))
    
  # join agegroup_gender_counts with empty_df
  agegroup_gender_df <- empty_df %>% 
    dplyr::left_join(agegroup_gender_counts, by = c("age_group"="age_group", "gender"="sex_person")) %>% 
    dplyr::mutate(n = replace_na(n,0))
  

  return(agegroup_gender_df)
}
