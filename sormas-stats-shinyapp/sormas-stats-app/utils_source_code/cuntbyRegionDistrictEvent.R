# cuntbyRegionDistrictEvent takes  eventData and return the count by region or district and certain event varaibles defined in the  
#factorLevelCountEvent function
cuntbyRegionDistrictEvent = function(data, byRegion = TRUE){  
  rowTtotal = factorLevelCountEvent(data = data, rowName = "Global")  #first row on table containing total counts for all regions
  if(byRegion == FALSE){
    districtName = unique(data$district_name) 
    for(i in districtName){
      tempData = data[data$district_name == i,]
      rowTtotal = rbind(rowTtotal, factorLevelCountEvent(data = tempData, rowName = i))
    }
  }else{
    regionName = unique(data$region_name)
    for(i in regionName){
      tempData = data[data$region_name == i,]
      rowTtotal = rbind(rowTtotal, factorLevelCountEvent(data = tempData, rowName = i))
    }
  }
  countByJurisdictionVaribles = rowTtotal[order(rowTtotal$Total, decreasing = T), ]
  countByJurisdictionVaribles = countByJurisdictionVaribles %>% 
    dplyr::select(-NA..1, -NA..7) %>% #dropping missing event status and archived status since it is never missing
    dplyr::rename(Total_last24hrs  = nlast24hrs, ONGOING_IS = ONGOING, PENDING_IS = PENDING,          
                  DISCARDED_IS = DISCARDED, DONE_IS = DONE, MISSING_IS = NA., CLUSTER_S = CLUSTER, EVENT_S = EVENT,              
                  SCREENING_S=SCREENING, SIGNAL_S =SIGNAL, ONGOING_MS = ONGOING.1, PENDING_MS = PENDING.1,           
                  DONE_MS = DONE.1, CLOSED_MS = CLOSED, MISSING_MS = NA..2, FACILITY_TP = FACILITY, FESTIVITIES_TP = FESTIVITIES,         
                  HOME_TP = HOME, TRANSPORT_TP = MEANS_OF_TRANSPORT, PUBLIC_TP = PUBLIC_PLACES, SCATTERED_TP = SCATTERED, MISSING_TP = NA..3,               
                  YES_NOSOCOMIAL = YES, NO_NOSOCOMIAL= NO, MISSING_NOSOCOMIAL = NA..4,  HOTLINE_PERSON_ST = HOTLINE_PERSON,
                  INSTITUTIONAL_PARTNER_ST = INSTITUTIONAL_PARTNER, MEDIA_NEWS_ST = MEDIA_NEWS, NOT_APPLICABLE_ST = NOT_APPLICABLE,
                  MISSING_ST = NA..5, LOW_RISK = LOW, HIGH_RISK = HIGH, MODERATE_RISK = MODERATE, MISSING_RISK = NA..6,
                  ARCHIVED = TRUE.,  UNARCHIVED = FALSE.)  
  return(countByJurisdictionVaribles)
}
# Test tun
# eventByJurisdiction = cuntbyRegionDistrictEvent(data = eventData, byRegion = FALSE) 