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
#' @seealso [GetDeathCountsPerAgegroupGender()]
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
  
  age_groups <- c("[0,5)",
               "[5,10)",
               "[10,15)",
               "[15,20)",
               "[20,25)",
               "[25,30)",
               "[30,35)",
               "[35,40)",
               "[40,45)",
               "[45,50)",
               "[50,55)",
               "[55,60)",
               "[60,65)",
               "[65,70)",
               "[70,75)",
               "[75,80)",
               "[80,Inf]",
               "UNKNOWN")
  
  # initialize data frame
  df0 <- data.frame(gender = rep(genders, each = length(age_groups)),
                    age_group = rep(age_groups, 4))
  
  return(df0)
  
}