#' Display Counts Per Agegroup and Gender Graph
#'
#' @description This function is dependent on the output of the 
#' GetDeathCountsPerAgegroupGender.R' function because it provides a plot based on
#' the agegroup_gender_data output of this function.
#' 
#' @agegroup_gender_data Output of the GetDeathCountsPerAgegroupGender.R function.
#'
#' @gender Gender from which counts are. Can be "MALE", "FEMALE", "OTHER" or "UNKNOWN". 
#'
#' @return Bar plot of the counts per agegroup of a certain gender.
#' 
#' @seealso [GetDeathCountsPerAgegroupGender()]
#' @export
#'
#' @examples
BarplotCountsPerAgegroup <- function(agegroup_gender_data, gender){
  # build 3 bar plots, one for each gender
  
  barplot_data <- agegroup_gender_data %>% 
    dplyr::filter(.data[["gender"]] == {{gender}})
  
  
    barplot <-  ggplot(data = barplot_data,
                 aes(x=age_group, y = n ))+
                 geom_bar(stat = "identity")+
                 labs(x="", y="", subtitle = as.character({{gender}}))+
                 theme(axis.text.x = element_text(angle = 90, hjust =1))+
                 geom_hline(yintercept = 0) +
                 expand_limits(y=0)

  return(barplot)
}