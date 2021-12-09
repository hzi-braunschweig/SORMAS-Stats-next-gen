# This function takes the table of number of cases by retion returned by cuntbyRegion function and computes the proportion for each cell
proportionByregion = function(data){
  temp = data[, !(colnames(data) %in% c("Name"))] # excluding region/district name, and totals
  rowPercent = (temp[1,][-1])/(as.numeric(temp[1,][1])) * 100
  rowPercent = format(round(rowPercent, digits = 2), nsmall =2) # to always maintain 2 dp after rounding to 2dp
  n = nrow(temp)
  for(i in 2:n){
    res = (temp[i,][-1])/(as.numeric(temp[i,][1])) * 100
    res = format(round(res, digits = 2), nsmall =2) # to always maintain 2 dp after rounding to 2dp
    rowPercent = rbind(rowPercent, res)
  }
  temp1 = data[, (colnames(data) %in% c("Name", "Total"))] 
  percentage_by_region = cbind(temp1, rowPercent)
  return(percentage_by_region)
}