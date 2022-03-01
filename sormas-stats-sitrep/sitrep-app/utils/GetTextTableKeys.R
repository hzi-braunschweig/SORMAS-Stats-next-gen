#' Get text keys for table
#'
#' @param overview_table Output of the GetOverviewEpidTable() function.
#' @param region_table Output of the GetRegionEpidTable() function.
#' @param geographic_units "geographic_unit" dataframe in the output list
#'  of the ExportPopulation() function. 
#'
#' @return Returns a vector with the table text keys for the overview and 
#' region tables.
#' @export
#'
#' @examples
GetTextTableKeys <- function(overview_table, region_table, geographic_units){
  
  ########### OVERVIEW TEXT TABLE KEYS ###########################
  
  
  # column keys for overview table
  column_keys_overview <-c("#Country_TotalCases",
                           "#Country_NewCases",
                           "#Country_TotalHospitalizations",
                           "#Country_NewHospitalizations",
                           "#Country_TotalDeaths",
                           "#Country_NewDeaths") 
  
  # overview table keys
  list_overview_table_keys <- rep(list(column_keys_overview), times = nrow(overview_table))

  # merge list of region table keys into one vector
  overview_table_keys <- Reduce(c, list_overview_table_keys)
 
  
  ######### REGION TEXT TABLE KEYS ####################################
  region_names <- sort(unique(geographic_units$name_region))
   
  # column keys for region table
  column_keys_region <-c("#Region_TotalCases",
                           "#Region_NewCases",
                           "#Region_TotalHospitalizations",
                           "#Region_NewHospitalizations",
                            "#Region_TotalDeaths",
                           "#Region_NewDeaths")
  
  # region table keys
  list_region_table_keys <- rep(list(column_keys_region), times = nrow(region_table))
  
  # add string for corresponding region at the end of each table key
  for (i in 1:length(list_region_table_keys)){
    list_region_table_keys[[i]] <- paste0(list_region_table_keys[[i]],"_", region_names[[i]])
  }
  
  # merge list of region table keys into one vector
  region_table_keys <- Reduce(c, list_region_table_keys)
  
  # replace spaces and "-" in the region names
  region_table_keys <- gsub("-", "", (gsub(" ", "", region_table_keys)))
  
  
  #################### MERGING #########################################
  
  # merge overview_table_keys and region_table_keys into one long vector: text_table_keys
  text_table_keys <- c(overview_table_keys, region_table_keys)

  return(text_table_keys)
}
