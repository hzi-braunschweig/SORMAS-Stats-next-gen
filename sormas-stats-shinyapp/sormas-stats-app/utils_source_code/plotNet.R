# This plots the network of the transmission chain
plotNet = function(nodeLineList, elist, IgraphLayout=TRUE)
{
  # IgraphLayout helps to reduce ploting time but the nodes are placed on fixed positions
  defaultFont="font-family:'Open Sans', sans-serif, 'Source Sans Pro'"
  mainStyle = paste(defaultFont, "color: #6591C4", ";font-weight: 600", "font-size: 1.6em", "text-align:center;", sep="; ")
  submainStyle = paste(defaultFont, "text-align:center;", sep="; ")
  footerStyle = defaultFont
  addNodesS <- data.frame(label = c("Healthy","Not_classified" ,"Suspected", "Probable", "Confirmed", "Not case", "1 = High risk", "2 = Low risk", "Event"), shape = "icon",
                          icon.code = c("f007", "f007", "f007", "f007", "f007","f007", "f178", "f178", "f013"),
                          icon.size = c(25, 25, 25, 25, 25,25,25,25,25), icon.color = c("#17bd27", "#706c67", "#ffff00", "#ffa500", "#f70707","#99bd17", "#0d0c0c", "#0d0c0c", "#0000ff"))
  if(IgraphLayout ==FALSE){
    g= visNetwork(nodeLineList, elist,  main = list(text = "Disease network diagram", style = mainStyle),
                  submain = list(text = "The arrows indicate the direction of transmission", style = submainStyle), 
                  # footer = list(text = "Zoom in to see the IDs and contact category", style = footerStyle), 
                  background = "white", annot = T, width = "100%", height = "100vh") %>%  
      visEdges(arrows = "to", color = "black", smooth = FALSE) %>% 
      visOptions(selectedBy = NULL,highlightNearest = TRUE, nodesIdSelection = FALSE) %>% 
      visGroups(groupname = "SUSPECT", size = 10, shape = "icon", icon = list( face ='FontAwesome', code = c( "f007"), color="#ffff00")) %>%
      visGroups(groupname = "PROBABLE", size = 10, shape = "icon", icon = list( face ='FontAwesome', code = c( "f007"), color="#ffa500")) %>%
      visGroups(groupname = "CONFIRMED", size = 10, shape = "icon", icon = list( face ='FontAwesome', code = c( "f007"), color="#f70707")) %>%
      visGroups(groupname = "NOT_CLASSIFIED", size = 10, shape = "icon", icon = list( face ='FontAwesome', code = c( "f007"), color="#706c67" )) %>%
      visGroups(groupname = "HEALTHY", size = 10, shape = "icon", icon = list( face ='FontAwesome', code = c( "f007"), color="#17bd27")) %>%
      visGroups(groupname = "NO_CASE", size = 10, shape = "icon", icon = list( face ='FontAwesome', code = c( "f007"), color="#99bd17")) %>%
      visGroups(groupname = "EVENT", size = 10, shape = "icon", icon = list( face ='FontAwesome', code = c( "f013"), color= "#0000ff")) %>% 
      visNetwork::addFontAwesome(name = "sormas-fontawesome") %>% # addFontAwesome(name = "font-awesome-visNetwork") %>% # because shiny uses fontAwesome 5 and not 4
      visLegend(addNodes = addNodesS, useGroups = F, position = "right", width = 0.1, ncol = 1, stepX = 100, stepY = 100, main = "Legend") %>%  
      visPhysics(stabilization = F) %>%
      visInteraction(dragNodes = T, dragView = T, zoomView = T, hideEdgesOnDrag = T, hideNodesOnDrag=F, hover = T, navigationButtons=T)
    
  } else{
    g= visNetwork(nodeLineList, elist,  main = list(text = "Disease network diagram", style = mainStyle),
                  submain = list(text = "The arrows indicate the direction of transmission", style = submainStyle), 
                  # footer = list(text = "Zoom in to see the IDs and contact category", style = footerStyle), 
                  background = "white", annot = T, width = "100%", height = "100vh") %>%
      visIgraphLayout() %>%  # to improve performance ie reduce plotting time
      visEdges(arrows = "to", color = "black", smooth = FALSE) %>% 
      visOptions(selectedBy = NULL,highlightNearest = TRUE, nodesIdSelection = FALSE) %>% 
      visGroups(groupname = "SUSPECT", size = 10, shape = "icon", icon = list( face ='FontAwesome', code = c( "f007"), color="#ffff00")) %>%
      visGroups(groupname = "PROBABLE", size = 10, shape = "icon", icon = list( face ='FontAwesome', code = c( "f007"), color="#ffa500")) %>%
      visGroups(groupname = "CONFIRMED", size = 10, shape = "icon", icon = list( face ='FontAwesome', code = c( "f007"), color="#f70707")) %>%
      visGroups(groupname = "NOT_CLASSIFIED", size = 10, shape = "icon", icon = list( face ='FontAwesome', code = c( "f007"), color="#706c67" )) %>%
      visGroups(groupname = "HEALTHY", size = 10, shape = "icon", icon = list( face ='FontAwesome', code = c( "f007"), color="#17bd27")) %>%
      visGroups(groupname = "NO_CASE", size = 10, shape = "icon", icon = list( face ='FontAwesome', code = c( "f007"), color="#99bd17")) %>%
      visGroups(groupname = "EVENT", size = 10, shape = "icon", icon = list( face ='FontAwesome', code = c( "f013"), color= "#0000ff")) %>% 
      visNetwork::addFontAwesome(name = "sormas-fontawesome") %>%
      visLegend(addNodes = addNodesS, useGroups = F, position = "right", width = 0.1, ncol = 1, stepX = 100, stepY = 100, main = "Legend") %>%  
      visPhysics(stabilization = F) %>%
      visInteraction(dragNodes = T, dragView = T, zoomView = T, hideEdgesOnDrag = T, hideNodesOnDrag=F, hover = T, navigationButtons=T)
  }
  return(g) #print(g)
}