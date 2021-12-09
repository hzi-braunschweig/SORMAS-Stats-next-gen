# To plot district shapes based on case count or incidence
districtMapPlot = function(data , districtShapes){
  temp =  data.frame(table(as.factor(data$district_name))) # calculatinf number of case by region
  colnames(temp) = c("LGAName", "Number of cases")
  ## joining sahp file data with surveillance data 
  districtShapes@data <- left_join(districtShapes@data, temp, by = "LGAName")   #c('name' = 'Borough')
  p =  qtm(shp = districtShapes, fill = "Number of cases", fill.palette = "Reds", 
           text.size = 1, style = "white", format = "World") + tm_compass(type="8star", position=c("right", "bottom")) 
  # fill.palette = "-Blues" to qhow in reverse order, type = "4star", "8star", "radar", "rose"
  return(p)
}