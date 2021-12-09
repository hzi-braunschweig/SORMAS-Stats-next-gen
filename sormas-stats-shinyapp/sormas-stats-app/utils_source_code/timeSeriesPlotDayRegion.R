timeSeriesPlotDayRegion = function(data, cum){
  f <- list(
    family = "Courier New, monospace",
    size = 18,
    color = "#7f7f7f"
  )
  x <- list(
    title = "Date of report",
    titlefont = f
  )
  y <- list(
    title = "Number of cases",
    titlefont = f
  )
  if (cum == T)
  {
    temp = data.frame()
    regionUnique = unique(data$region_name)
    for(i in regionUnique)
    {
      temp2 = data[data$region_name == i,]
      temp2 = temp2[order(temp2$reportdate),]
      temp2$cumulative = cumsum(temp2$total)
      temp = rbind(temp, temp2)
    }
    fig = temp %>% plot_ly(x = ~reportdate, y = ~ cumulative, type="scatter",mode = "none") %>% 
      add_lines(linetype = ~ region_name)  %>% layout(xaxis = x, yaxis = y)
    
  } else{
    fig = data %>% plot_ly(x = ~ reportdate, y = ~ total, type="scatter",mode = "none") %>%
      add_lines(linetype = ~ region_name)  %>% layout(xaxis = x, yaxis = y)
  }
  return(fig)
}