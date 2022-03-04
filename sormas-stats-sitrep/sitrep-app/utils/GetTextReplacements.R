#' Get Text replacements for overview and region table
#' 
#'
#' @param overview_table Output of the GetOverviewEpidTable().
#' @param region_table Output of the GetOverviewEpidTable().
#'
#' @return Returns a vector with the replacements for the overview and 
#' region tables.
#' @export
#'
#' @examples
GetTextReplacements <- function(overview_table, region_table){
  
  ####### OVERVIEW TABLE #########
  
  overview_rows <- list()
  
  # get rows of overview table as vectors
  for (i in 1:nrow(overview_table)){
    overview_rows[[i]] <- as.character(as.vector(overview_table[i,]))
  }
  
  # join all rows into one long vector of replacements
  overview_replacements <- Reduce(c, overview_rows)
  
  ####### REGION TABLE #########
  
  region_rows <- list()
  
  # get rows of region table as vectors
  for (i in 1:nrow(region_table)){
    region_rows[[i]] <- as.character(as.vector(region_table[i,-1]))
  }
  
  # join all rows into one long vector of replacements
  region_replacements <- Reduce(c, region_rows)
  
  ############# MERGING ########################
  
  # merge overview_replacements and region_replacements into one long vector
  # text_replacements
  
  text_replacements <- c(overview_replacements, region_replacements)
  
  
  return(text_replacements)
  }
