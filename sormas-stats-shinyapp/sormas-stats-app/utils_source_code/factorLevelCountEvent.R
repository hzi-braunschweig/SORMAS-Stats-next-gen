# Return table of count of event by jurisdiction and other varibales -----
# region must not have NA for this method to work
factorLevelCountEvent = function(data,  rowName){ #takes a data and rerun the counts of the variables in the function
  Name = rowName
  Total = nrow(data)
  nlast24hrs = nrow(data[data$reportdatetime_event %in% c(Sys.Date()-1, Sys.Date()), ])
  eventinvestigationstatusTotal = data.frame(t(as.matrix(  table( addNA(factor(data$eventinvestigationstatus,levels = c("ONGOING", "PENDING", "DISCARDED", "DONE") ), ifany = FALSE) ) )))
  eventstatusTotal = data.frame(t(as.matrix(  table( addNA(factor(data$eventstatus,levels = c("CLUSTER", "EVENT", "SCREENING", "SIGNAL") ), ifany = FALSE) ) )))
  typeofplaceTotal = data.frame(t(as.matrix(  table( addNA(factor(data$typeofplace_event,
                                                                  levels = c("FACILITY", "FESTIVITIES", "HOME", "MEANS_OF_TRANSPORT", "PUBLIC_PLACES","SCATTERED") ), ifany = FALSE) ) )))
  archivedTotal = data.frame(t(as.matrix(  table( addNA(factor(data$archived_event,levels = c("TRUE", "FALSE") ), ifany = FALSE) ) )))
  nosocomialTotal = data.frame(t(as.matrix(  table( addNA(factor(data$nosocomial_event,levels = c("YES", "NO") ), ifany = FALSE) ) )))
  srctypeTotal = data.frame(t(as.matrix(  table( addNA(factor(data$srctype_event,levels = c("HOTLINE_PERSON", "INSTITUTIONAL_PARTNER", "MEDIA_NEWS", "NOT_APPLICABLE") ), ifany = FALSE) ) )))
  risklevelTotal = data.frame(t(as.matrix(  table( addNA(factor(data$risklevel_event,levels = c("LOW", "HIGH", "MODERATE") ), ifany = FALSE) ) )))
  managementstatusTotal = data.frame(t(as.matrix(  table( addNA(factor(data$eventmanagementstatus,levels = c("ONGOING", "PENDING", "DONE", "CLOSED") ), ifany = FALSE) ) )))
  globalCount = data.frame(Name,Total, nlast24hrs, eventinvestigationstatusTotal, eventstatusTotal,managementstatusTotal, typeofplaceTotal, nosocomialTotal, srctypeTotal,risklevelTotal, archivedTotal)
  return(globalCount)
}
