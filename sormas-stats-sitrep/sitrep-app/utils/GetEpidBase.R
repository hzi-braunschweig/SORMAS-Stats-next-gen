#' Get Epidemic Indicator Base Table
#' 
#' @description Provides an epidemic indicator base table. The columns of the 
#'   table are the following variables:
#'   -Total cases
#'   -New cases
#'   -Total hospitalizations
#'   -New hospitalizations
#'   -Total deaths
#'   -New Deaths
#'   (- 7 day incidence to be added)
#'   (- Change in 7 day incidence to be added)
#'   The rows of the table are districts. 
#'
#'
#'
#' @param data_line_list Line listed data exported from SORMAS using the 
#'   ExportCaseLineList() function.
#'
#' @return Returns the epidemic dynamic base table
#' @export
#'
#' @examples
GetEpidBase <- function(data_line_list = case_data_list){
  
  # Get dataframe with base indicators for cases
  
  ## Get total cases per district
  
  ## Get new cases per district
  
  ## Join on id_district
  
  ###########################
  
  # Get dataframe with base indicators for hospitalizations
  
  ## Get total hospitalizations per district
  
  ## Get new hospitalizations per district
  
  ## Join on id_district
  
  ###########################
  
  # Get dataframe with base indicators for deaths
  
  ## Get total deaths per district
  
  ## Get new deaths per district
  
  ## Join on id_district
  
  ###########################
  
  # Join the 3 dataframes on id_district (full join)
  
  
 return(epid_base) 
}