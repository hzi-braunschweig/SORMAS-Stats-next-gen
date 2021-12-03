# case count by region and one or more categorical variable
# uses casePersonRegionDist as input data
factorLevelCount = function(data,  rowName){ #takes a data and rerun the counts of the variables in the function
  Name = rowName
  Total = nrow(data)
  nlast24hrs = nrow(data[data$reportdate %in% c(Sys.Date()-1, Sys.Date()), ])
  caseByClassTotal = data.frame(t(as.matrix( table(factor(data$caseclassification, levels = c("CONFIRMED", "CONFIRMED_NO_SYMPTOMS", "CONFIRMED_UNKNOWN_SYMPTOMS",
                                                                                              "NOT_CLASSIFIED", "PROBABLE", "SUSPECT") ) ) )))
  caseByOutcomeTotal = data.frame(t(as.matrix( table(factor(data$outcome, levels = c("DECEASED","NO_OUTCOME","RECOVERED","UNKNOWN") )) )))
  caseByQuarantineTotal = data.frame(t(as.matrix( table(factor(data$quarantine, levels =  c("HOME","INSTITUTIONELL","NONE","OTHER","UNKNOWN") )) )))
  caseByOriginTotal = data.frame(t(as.matrix( table(factor(data$caseorigin, levels = c("IN_COUNTRY","POINT_OF_ENTRY"))) )))
  globalCount = data.frame(Name,Total, nlast24hrs, caseByClassTotal, caseByOutcomeTotal, caseByOriginTotal, caseByQuarantineTotal)
  return(globalCount)
}