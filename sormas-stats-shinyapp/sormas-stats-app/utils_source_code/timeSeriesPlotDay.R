timeSeriesPlotDay = function(data, cum){
  #defining legends
  f <- list(
    family = "Courier New, monospace",
    size = 18,
    color = "#7f7f7f"
  )
  y <- list(
    title = "Number of cases",
    titlefont = f
  )
  x <- list(
    title = "Date of report",
    titlefont = f
  )
  
  if (cum == T)
  {
    data$cumulative = cumsum(data$total)
    fig = data %>% plot_ly(x = ~reportdate, y = ~ cumulative, type="scatter",mode = "lines+markers") %>%
      layout(xaxis = x, yaxis = y)
  } else{
    fig =data %>% plot_ly(x = ~reportdate, y = ~ total, type="scatter",mode = "lines+markers") %>%
      layout(xaxis = x, yaxis = y)
  }
  return(fig)
}