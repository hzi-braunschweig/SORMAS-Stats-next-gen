timeSeriesPlotWeek = function(data){
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
    title = "Week of report",
    titlefont = f
  )
  fig = plot_ly(x =data$reportweek, y = data$total, type="scatter",mode = "markers") %>%
    add_lines(linetype = ~ data$reportyear)
  fig <- fig %>% layout(xaxis = x, yaxis = y)
  return(fig)
}