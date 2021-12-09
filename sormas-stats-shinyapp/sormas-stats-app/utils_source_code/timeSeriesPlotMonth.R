timeSeriesPlotMonth = function(data){
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
    title = "Month of report",
    titlefont = f
  )
  fig = plot_ly(x =data$reportmonth, y = data$total, type="scatter",mode = "markers") %>%
    add_lines(linetype = ~ data$reportyear)
  fig <- fig %>% layout(xaxis = x, yaxis = y)
  return(fig)
}