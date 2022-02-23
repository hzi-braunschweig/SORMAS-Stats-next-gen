#' Get text keys for table
#'
#' @param overview_table Output of the GetOverviewEpidTable().
#' @param region_table Output of the GetRegionEpidTable().
#'
#' @return Returns a vector with the table text keys for the overview and 
#' region tables.
#' @export
#'
#' @examples
GetTextTableKeys <- function(overview_table, region_table){
  
  ########### OVERVIEW TEXT TABLE KEYS ###########################
  
  
  # column keys for overview table
  column_keys_overview <-c("#Overview_TC",
                           "#Overview_NC",
                           "#Overview_TH",
                           "#Overview_NH",
                           "#Overview_TD",
                           "#Overview_ND") 
  
  # overview table keys
  list_overview_table_keys <- rep(list(column_keys_overview), times = nrow(overview_table))
  
  # get row counts to complete overview table keys
  for (i in 1:length(list_overview_table_keys)){
    vector_row <- list_overview_table_keys[[i]]
    # add number indicating table row
    vector_row <- paste0(vector_row,"_", i)
    # reassign vector to list
    list_overview_table_keys[[i]] <- vector_row
  }
  
  # merge list of region table keys into one vector
  overview_table_keys <- Reduce(c, list_overview_table_keys)
 
  
  ######### REGION TEXT TABLE KEYS ####################################
  
   
  # column keys for region table
  column_keys_region <-c("#Region_TC",
                           "#Region_NC",
                           "#Region_TH",
                           "#Region_NH",
                           "#Region_TD",
                           "#Region_ND")
  
  # region table keys
  list_region_table_keys <- rep(list(column_keys_region), times = nrow(region_table))
  
  # get row counts to complete overview table keys
  for (i in 1:length(list_region_table_keys)){
    vector_row <- list_region_table_keys[[i]]
    # add number indicating table row
    vector_row <- paste0(vector_row,"_", i)
    # reassign vector to list
    list_region_table_keys[[i]] <- vector_row
  }
  
  # merge list of region table keys into one vector
  region_table_keys <- Reduce(c, list_region_table_keys)
  
  
  #################### MERGING #########################################
  
  # merge overview_table_keys and region_table_keys into one long vector: text_table_keys
  text_table_keys <- c(overview_table_keys, region_table_keys)

  return(text_table_keys)
}