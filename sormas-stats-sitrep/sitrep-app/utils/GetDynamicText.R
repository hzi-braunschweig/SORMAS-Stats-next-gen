#' Get dynamic text by replacing table keys in text with replacements 
#'
#' @param text Text read in from word files, in a dataframe.
#' @param overview_table Output of the GetOverviewEpidTable function.
#' @param region_table Output of the GetRegionEpidTable function.
#'
#' @return Returns a dataframe containing the text, but with the replacement values 
#' from the two input tables instead of the table text keys from before.
#' @export
#'
#' @examples
GetDynamicText <- function(text, overview_table, region_table, geographic_units){
  
  # get text table keys for overview table and region table
  text_table_keys <- GetTextTableKeys(overview_table = overview_table, 
                                      region_table = region_table,
                                      geographic_units = geographic_units)
  
  # get replacements, by which the text keys will be replaced in the text
  text_replacements <- GetTextReplacements(overview_table = overview_table, 
                                           region_table = region_table)
  
  # replace text keys with replacements
  text$text_files....1. <- mgsub::mgsub(text$text_files....1.,
                                pattern = text_table_keys,
                                replacement = text_replacements) 
 
  
  return(text) 
}