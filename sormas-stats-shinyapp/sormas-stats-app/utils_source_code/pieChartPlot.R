# Takes a character vector and  plot a pie chart
pieChartPlot = function(variable){
  temp = data.frame(table(variable))
  labels = as.character(temp$variable)
  values = temp$Freq
  fig = plot_ly(type='pie', labels=labels, values=values, 
                textinfo='label+percent',
                insidetextorientation='radial') 
  return(fig)
}
