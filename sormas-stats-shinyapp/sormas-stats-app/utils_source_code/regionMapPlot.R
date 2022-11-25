### To plot the map of case incidence by region
regionMapPlot = function(data , lnd)
{
  casePerRegion =  data.frame(table(as.factor(data$region_name))) # calculatinf number of case by region
  colnames(casePerRegion) = c("StateName", "Number of cases")
  ## joining sahp file data with surveillance data 
  lnd@data <- left_join(lnd@data, casePerRegion, by = "StateName")   #c('name' = 'Borough')
  p = tmap::qtm(shp = lnd, fill = "Number of cases", fill.palette = "Reds", text = "StateName", 
          text.size = 1, style = "white", format = "World") + tm_compass(type="8star", position=c("right", "bottom")) + tmap_options(check.and.fix = TRUE) 
  # fill.palette = "-Blues" to qhow in reverse order, type = "4star", "8star", "radar", "rose"
  return(p)
  
}