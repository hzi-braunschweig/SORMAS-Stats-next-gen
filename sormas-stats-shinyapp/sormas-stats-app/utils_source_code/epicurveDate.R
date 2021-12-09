epicurveDate = function(data){
  #sorting data by case clessification nefore plotting to have the legends right
  data$caseclassification  <- reorder.factor(data$caseclassification, new.order=c("CONFIRMED", "PROBABLE",  "SUSPECT", "NOT_CLASSIFIED"))                  
  data = data[order(data$caseclassification), ]
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
  fig = data %>%
    plot_ly(x = ~reportdate, y = ~total, color = ~factor(caseclassification), colors = c("#f70707", "#ffa500", "#ffff00", "#706c67" )) %>%
    add_bars() %>%
    layout(barmode = "stack")
  fig <- fig %>% layout(xaxis = x, yaxis = y)
  return(fig)
}