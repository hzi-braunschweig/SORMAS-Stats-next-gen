# cuntbyRegionDistrictCase takes the data of cases and return the case count by region or district and certain case varaibles defined by factorLevelCount function
cuntbyRegionDistrictCase = function(data, byRegion = TRUE){  # depends on factorLevelCount function
  rowTtotal = factorLevelCount(data = data, rowName = "Global")  #forst row on table containing total counts for all regions
  
  if(byRegion == FALSE){
    districtName = unique(data$district_name) 
    for(i in districtName){
      tempData = data[data$district_name == i,]
      rowTtotal = rbind(rowTtotal, factorLevelCount(data = tempData, rowName = i))
    }
  }else{
    regionName = unique(data$region_name)
    for(i in regionName){
      tempData = data[data$region_name == i,]
      rowTtotal = rbind(rowTtotal, factorLevelCount(data = tempData, rowName = i) )
    }
    
  }
  
  countByRegionVaribles = rowTtotal[order(rowTtotal$Total, decreasing = T), ]
  countByRegionVaribles$CONFIRMED = countByRegionVaribles$CONFIRMED
  countByRegionVaribles = countByRegionVaribles %>%
    dplyr::mutate(CONFIRMED = CONFIRMED+ CONFIRMED_NO_SYMPTOMS+ CONFIRMED_UNKNOWN_SYMPTOMS ) %>%
    dplyr::select(-CONFIRMED_NO_SYMPTOMS, -CONFIRMED_UNKNOWN_SYMPTOMS  )
  countByRegionVaribles = countByRegionVaribles %>% 
    dplyr::rename(Total_last24hrs  = nlast24hrs, confirmed = CONFIRMED,  Unclassified = NOT_CLASSIFIED, Probable=PROBABLE, Suspected=SUSPECT,
                  Deseased =DECEASED, No_Outcome= NO_OUTCOME, Recovered = RECOVERED, UnK_Outcome =UNKNOWN, In_Country =IN_COUNTRY, Imported = POINT_OF_ENTRY,
                  Home_Q=HOME, Institutional_Q=INSTITUTIONELL, No_Q=NONE, Other_Q=OTHER, Unk_Q=UNKNOWN.1 )
  
  return(countByRegionVaribles)
}