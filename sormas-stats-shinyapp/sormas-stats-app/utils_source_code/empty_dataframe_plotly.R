# This is a ggplotly output to be used when the  data to be plotted is empty.
# This can happen when the user chose filter options that has no data to be analysed
empty_dataframe_plotly <- function(){
  df <- data.frame()
  p <- ggplot(df) + geom_point() + xlim(0, 10) + ylim(0, 10) + 
    annotate("text", x=3.9, y=5.0, size=40, col="red", label="(" ) +
    annotate("text", x=5, y=5.6, size=12, col="red", label="o  o" ) +
    annotate("text", x=6.1, y=5.0, size=40, col="red", label=")" ) +
    annotate("text", x=5, y=5.1, size=12, col="red", label="|" ) +
    geom_segment(aes(x = 4.7, xend = 5.3, y = 4.4, yend = 4.4), size=2, color="red") +
    annotate("text", x=5, y=3, size=6, col="red", label="No Data, please choose another filter combination") 
  #bp <- ggplotly(p)
  #bp
  return(p)
}