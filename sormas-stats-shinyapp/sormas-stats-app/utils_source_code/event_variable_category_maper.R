## event_variable_category_maper: adding a mapper to map event varaibles to categories
event_variable_category_maper = function(cuntbyRegionTableEvent){
  # Creating a dataframe to map categories of events to their varaibles, This is needed for filtering later
  # This function should be updated each time new variables are added in cuntbyRegionDistrictEvent
  event_variable = c("Name","Total", "Total_last24hrs", "Investigation status", "Investigation status", "Investigation status", "Investigation status", "Investigation status",
                     "Event status", "Event status", "Event status", "Event status",
                     "Management status", "Management status", "Management status", "Management status", "Management status",
                     "Type of place", "Type of place","Type of place","Type of place","Type of place","Type of place","Type of place", 
                     "Nosocomial", "Nosocomial", "Nosocomial", 
                     "Source type", "Source type", "Source type", "Source type", "Source type",
                     "Risk level", "Risk level", "Risk level", "Risk level",
                     "Archive status" , "Archive status")
  event_variable_category = data.frame(event_variable, category = colnames(cuntbyRegionTableEvent))
  return(event_variable_category)
}