#' Display Death counts per agegroup and gender
#'
#' @agegroup_gender_data Output of the GetDeathCountsPerAgegroupGender.R function.
#'
#' @return 4 bar plot for the death count for MALE, FEMALE, OTHER and UNKNOWN genders.
#' @seealso [BarplotCountsPerAgegroup(), GetDeathCountsPerAgegroupGender()]
#' 
#' @export
#'
#' @examples
DisplayDeathCountsPerAgegroupGender <- function(agegroup_gender_data = death_counts_agegroup_gender){
  # generate barplot for male
  barplot_male <- BarplotCountsPerAgegroup(agegroup_gender_data, "MALE")
  
  # generate barplot for female
  barplot_female <- BarplotCountsPerAgegroup(agegroup_gender_data, "FEMALE")
  
  # generate barplot for other
  barplot_other <- BarplotCountsPerAgegroup(agegroup_gender_data, "OTHER")
  
  # generate barplot for unknown
  barplot_unknown <- BarplotCountsPerAgegroup(agegroup_gender_data, "UNKNOWN")
  
  # plot all four barplots
  gridExtra::grid.arrange(barplot_male, barplot_female, barplot_other, barplot_unknown)
}