## EVENT DATA ANALYSIS-----
# This sub section of the server.r file renders the EVENT data analysis tab
# All back-end methods related to this tab should be added in this file
# Event filter ----
# filtering district name from eventData based of users selected region and parse it back to the frontend user
output$pickerInputdistrictEventUi <- renderUI({
  if(!is.null(input$regionEventUi))
  {
    temp = eventData %>%
      dplyr::filter(region_name %in% input$regionEventUi)  %>%
      distinct(district_name) %>%
      .$district_name
  }else{
    temp = NULL
  }
  pickerInput(inputId = 'districtEventUi', label =i18n$t('District of event'),
              choices = temp, 
              options = list(
                `actions-box` = TRUE, 
                size = 12
              ),
              selected = NULL,
              multiple = TRUE
  )
})
# filter eventData by disease, region, and time
selEventRegionUi = reactive({
  if(!is.null(input$regionEventUi)){
    eventData[((eventData$region_name  %in% input$regionEventUi) & (eventData$disease_event == input$diseaseEventUi) & (eventData$relevantdate_event >= (min(input$reportdateEventUi) )  ) & (eventData$relevantdate_event <= (max(input$reportdateEventUi) ))),]
  } else{
    eventData[((eventData$disease_event == input$diseaseEventUi) & (eventData$relevantdate_event >= (min(input$reportdateEventUi))) & (eventData$relevantdate_event <= (max(input$reportdateEventUi)) )), ]
  }
}) 

# filter by district of event
selEventRegionDistUi = reactive({
  if(is.null(input$districtEventUi))
  {
    temp = selEventRegionUi()
  } else{
    temp = selEventRegionUi() %>%
      dplyr::filter(district_name %in% input$districtEventUi)
  }
  return(temp)
  
})
# filtering event by event identification source
selEventRegionDistIdentificationSourceUi = reactive({
  if(is.null(input$eventIdentificationSourceUi))
  {
    temp = selEventRegionDistUi()
  } else{
    temp = selEventRegionDistUi() %>%
      dplyr::filter(event_identification_source %in% input$eventIdentificationSourceUi)
  }
  return(temp)
}) 

# Authenticating and renaming selected data 
# This authentication would apply to all previous filters that this object depends on
eventDataDiseaseRegionTimeAuthFilter = reactive({
   
  selEventRegionDistIdentificationSourceUi()
})
# Adding control based on eventDataAnalysisAction icon on front ui
# Any output or computation that depend on eventDataDiseaseRegionTimeAuthFilter wouuld run only when input$eventDataAnalysisAction is clicked
eventDataDiseaseRegionTimeFilter = eventReactive(input$eventDataAnalysisAction, { 
  eventDataDiseaseRegionTimeAuthFilter() 
}, ignoreNULL = FALSE)
# End of event filter

## event bar plot -----
output$eventBarplotUi <- renderPlotly({
  temp = eventDataDiseaseRegionTimeFilter( )
  if(input$EventIndicatorTypeUi == "Count"){
    if(!is.null(input$regionEventUi)){ 
      if(!is.null(input$districtEventUi)){  
        temp = temp %>%
          dplyr::filter( (region_name %in% input$regionEventUi) & (district_name %in% input$districtEventUi) ) 
        fig = barplotEventStatusByJurisdiction(data = temp, Var1 = "district_name", count = TRUE )
      } else {
        temp = temp %>%
          dplyr::filter( (region_name %in% input$regionEventUi) ) 
        fig = barplotEventStatusByJurisdiction(data = temp, Var1 = "region_name", count = TRUE)
      }
    } else {
      fig = barplotEventStatusByJurisdiction(data = temp, Var1 = "region_name", count = TRUE)
    }
  }
if(input$EventIndicatorTypeUi == "Proportion"){
    if(!is.null(input$regionEventUi)){ 
      if(!is.null(input$districtEventUi)){  
        temp = temp %>%
          dplyr::filter( (region_name %in% input$regionEventUi) & (district_name %in% input$districtEventUi) ) 
        fig = barplotEventStatusByJurisdiction(data = temp, Var1 = "district_name", count = FALSE )
      } else {
        temp = temp %>%
          dplyr::filter( (region_name %in% input$regionEventUi) ) 
        fig = barplotEventStatusByJurisdiction(data = temp, Var1 = "region_name", count = FALSE)
      }
    } else {
      fig = barplotEventStatusByJurisdiction(data = temp, Var1 = "region_name", count = FALSE)
    }
  }
  fig = ggplotly(fig)
  return(fig)
})
# Event dashboard KPI -----
## Total events
output$totalEvent <- renderInfoBox({
  infoBox("", nrow(eventDataDiseaseRegionTimeFilter() ), icon = icon("cog"),  color = colEvent,
    fill = FALSE, subtitle =  "Total Events")
})  
# total event participant
output$totalEventParticipants <- renderInfoBox({
  infoBox("", sum(eventDataDiseaseRegionTimeFilter()$eventPart_sum ), icon = icon("users"),
    color = colEvent, fill = FALSE, subtitle = "Event Participants")
})
# total resulting cases from events
output$totalEventResultingCases <- renderInfoBox({
  infoBox("", sum(eventDataDiseaseRegionTimeFilter()$resulting_case_sum ), icon = icon("procedures"),
    color = colCase, fill = FALSE, subtitle = "Resulting Cases")
})
# total events by management status
output$totalEventManagementPending <- renderInfoBox({
  temp = eventDataDiseaseRegionTimeFilter() %>%
    dplyr::filter(eventmanagementstatus == "PENDING") 
  infoBox("", nrow(temp),
    icon = icon("cog"), color = colEvent, fill = FALSE, subtitle = "Management: Pending" ) 
})
output$totalEventManagementOngoing <- renderInfoBox({
  temp = eventDataDiseaseRegionTimeFilter() %>%
    dplyr::filter(eventmanagementstatus == "ONGOING")
  infoBox("", nrow(temp), icon = icon("cog"), color = colEvent, fill = FALSE, subtitle = "Management: Ongoing")
})
output$totalEventManagementDone <- renderInfoBox({
  temp = eventDataDiseaseRegionTimeFilter() %>%
    dplyr::filter(eventmanagementstatus == "DONE")
  infoBox("", nrow(temp), icon = icon("cog"), color = colEvent, fill = FALSE, subtitle = "Management: Done" )
})
output$totalEventManagementClosed <- renderInfoBox({
  temp = eventDataDiseaseRegionTimeFilter() %>%
    dplyr::filter(eventmanagementstatus == "CLOSED")
  infoBox("", nrow(temp), icon = icon("cog"),  
    color = colEvent, fill = FALSE, subtitle = "Management: Closed" )
})
output$totalEventManagementMissing <- renderInfoBox({
  temp = eventDataDiseaseRegionTimeFilter() %>%
    dplyr::filter(is.na(eventmanagementstatus))
  infoBox("", nrow(temp), icon = icon("cog"),  
    color = colEvent, fill = FALSE, subtitle = "Management: Missing " )
})
# total events by event status
output$totalEventstatusSignal <- renderInfoBox({
  temp = eventDataDiseaseRegionTimeFilter() %>%
    dplyr::filter(eventstatus == "SIGNAL" )
  infoBox("", nrow(temp), icon = icon("cog"),  
    color = colEvent, fill = FALSE, subtitle = "Signal" ) 
})
output$totalEventstatusEvent <- renderInfoBox({
  temp = eventDataDiseaseRegionTimeFilter() %>%
    dplyr::filter(eventstatus == "EVENT" )
  infoBox("", nrow(temp), icon = icon("cog"),  
    color = colEvent, fill = FALSE, subtitle = "Event")
})
output$totalEventstatusCluster <- renderInfoBox({
  temp = eventDataDiseaseRegionTimeFilter() %>%
    dplyr::filter(eventstatus == "CLUSTER" )
  infoBox("", nrow(temp), icon = icon("cog"),  
    color = colEvent, fill = FALSE, subtitle = "Cluster") 
}) 
output$totalEventstatusScreening <- renderInfoBox({
  temp = eventDataDiseaseRegionTimeFilter() %>%
    dplyr::filter(eventstatus == "SCREENING" )
  infoBox("", nrow(temp), icon = icon("cog"),  
    color = colEvent, fill = FALSE, subtitle = "Screening")
})
## event status by type of place -----
output$eventCuntbytyplaceTable <- DT::renderDataTable({
  if(input$EventIndicatorTypeUi == "Count"){
    temp = twoByTwoTablefunction(data = eventDataDiseaseRegionTimeFilter(), Var1 = "eventstatus", Var2 =  "typeofplace_event", 
                                 spread = TRUE, Proportion = FALSE)
    res =  DT::datatable( temp,
      filter = 'top', extensions = c('Buttons', 'Scroller'),
      options = list(scrollY = 200, #550
                     scrollX = 500,
                     deferRender = TRUE,
                     scroller = TRUE,
                     # paging = TRUE,
                     # pageLength = 25,
                     buttons = list(c('excel','csv'),
                                    list(extend = 'colvis', targets = 0, visible = FALSE)),
                     dom = 'lBfrtip',
                     fixedColumns = TRUE,
                     autoWidth = TRUE,
                     columnDefs = list(list(className = 'dt-center', targets = "_all"))
      ), 
      rownames = FALSE)
  }
  if(input$EventIndicatorTypeUi == "Proportion"){
    temp = twoByTwoTablefunction(data = eventDataDiseaseRegionTimeFilter() , Var1 = "eventstatus", Var2 = "typeofplace_event", 
                                 spread = TRUE, Proportion = TRUE)
    res =  DT::datatable(temp,
      filter = 'top', extensions = c('Buttons', 'Scroller'),
      options = list(scrollY = 200,
                     scrollX = 500,
                     deferRender = TRUE,
                     scroller = TRUE,
                     # paging = TRUE,
                     # pageLength = 25,
                     buttons = list(c('excel','csv'),
                                    list(extend = 'colvis', targets = 0, visible = FALSE)),
                     dom = 'lBfrtip',
                     fixedColumns = TRUE,
                     autoWidth = TRUE,
                     columnDefs = list(list(className = 'dt-center', targets = "_all"))
      ), 
      rownames = FALSE)
  }
  return(res)
})
# event status by eventmanagementstatus  -----
output$eventStatusByMangemantStatusTable <- DT::renderDataTable({
  if(input$EventIndicatorTypeUi == "Count"){
    temp = twoByTwoTablefunction(data = eventDataDiseaseRegionTimeFilter() , Var1 = "eventstatus", Var2 = "eventmanagementstatus", 
                                 spread = TRUE, Proportion = FALSE)
    res = DT::datatable(temp,
      filter = 'top', extensions = c('Buttons', 'Scroller'),
      options = list(scrollY = 200,
                     scrollX = 500,
                     deferRender = TRUE,
                     scroller = TRUE,
                     # paging = TRUE,
                     # pageLength = 25,
                     buttons = list(c('excel','csv'),
                                    list(extend = 'colvis', targets = 0, visible = FALSE)),
                     dom = 'lBfrtip',
                     fixedColumns = TRUE,
                     autoWidth = TRUE,
                     columnDefs = list(list(className = 'dt-center', targets = "_all"))
      ), 
      rownames = FALSE)
  }
  if(input$EventIndicatorTypeUi == "Proportion"){
    temp = twoByTwoTablefunction(data = eventDataDiseaseRegionTimeFilter() , Var1 = "eventstatus", Var2 = "eventmanagementstatus",
                                 spread = TRUE, Proportion = TRUE)
    res =  DT::datatable(temp,
      filter = 'top', extensions = c('Buttons', 'Scroller'),
      options = list(scrollY = 200,
                     scrollX = 500,
                     deferRender = TRUE,
                     scroller = TRUE,
                     # paging = TRUE,
                     # pageLength = 25,
                     buttons = list(c('excel','csv'),
                                    list(extend = 'colvis', targets = 0, visible = FALSE)),
                     dom = 'lBfrtip',
                     fixedColumns = TRUE,
                     autoWidth = TRUE,
                     columnDefs = list(list(className = 'dt-center', targets = "_all"))
      ), 
      rownames = FALSE)
  }
  return(res)
})

# event status by risklevel_event -----
output$eventStatusByRisklevelTable <- DT::renderDataTable({
  if(input$EventIndicatorTypeUi == "Count"){
    temp = twoByTwoTablefunction(data = eventDataDiseaseRegionTimeFilter() , Var1 = "eventstatus", Var2 = "risklevel_event", 
                                 spread = TRUE, Proportion = FALSE)
    res = DT::datatable(temp,
      filter = 'top', extensions = c('Buttons', 'Scroller'), # filter = 'none',
      options = list(scrollY = 200,
                     scrollX = 500,
                     deferRender = TRUE,
                     scroller = TRUE,
                     # paging = TRUE,
                     # pageLength = 25,
                     buttons = list(c('excel','csv'),
                                    list(extend = 'colvis', targets = 0, visible = FALSE)),
                     dom = 'lBfrtip',
                     fixedColumns = TRUE,
                     autoWidth = TRUE,
                     columnDefs = list(list(className = 'dt-center', targets = "_all", searchable = TRUE))
      ), 
      rownames = FALSE)
    
  }
  if(input$EventIndicatorTypeUi == "Proportion"){
    temp = twoByTwoTablefunction(data = eventDataDiseaseRegionTimeFilter() , Var1 = "eventstatus", Var2 = "risklevel_event", spread = TRUE, Proportion = TRUE)
    res =  DT::datatable(
      temp,
      filter = 'top', extensions = c('Buttons', 'Scroller'),
      options = list(scrollY = 200,
                     scrollX = 500,
                     deferRender = TRUE,
                     scroller = TRUE,
                     # paging = TRUE,
                     # pageLength = 25,
                     buttons = list(c('excel','csv'),
                                    list(extend = 'colvis', targets = 0, visible = FALSE)),
                     dom = 'lBfrtip',
                     fixedColumns = TRUE,
                     autoWidth = TRUE,
                     columnDefs = list(list(className = 'dt-center', targets = "_all"))
      ), 
      rownames = FALSE)
  }
  return(res)
})
# dynamic 2x2 table for events ----
output$dynamic2x2TableEventUi <- DT::renderDataTable({
  # filter by variables selected by user
  temp = eventDataDiseaseRegionTimeFilter()
  if( length(input$twoByTwotableEventVariblesUi) == 2 ){
    temp = temp[ , (colnames(temp) %in% input$twoByTwotableEventVariblesUi) ]
  } else {
    temp = temp[, (colnames(temp) %in% c("location_category","eventstatus")) ]
  }
  rowVar = colnames(temp)[1]
  colVar = colnames(temp)[2]
  # computing table values
  if(input$EventIndicatorTypeUi == "Count"){
    if(input$transpose2x1TableEventUi == TRUE){
      tempCount = twoByTwoTablefunction(data = temp , Var1 = rowVar, Var2 = colVar, spread = TRUE, Proportion = FALSE, spreadVar = "Var1" )
    } else{
      tempCount = twoByTwoTablefunction(data = temp , Var1 = rowVar, Var2 = colVar, spread = TRUE, Proportion = FALSE ) # spreadVar = "Var2" is the default
    }
    res = DT::datatable(
      data.frame(tempCount, Total = rowSums(tempCount[,-1], na.rm = TRUE)), # computing row sum
      filter = 'top',   # filter = 'none',
      extensions = c('Buttons', 'Scroller'),
      options = list(scrollY = '50vh',  #400
                     scrollX = TRUE, # 800
                     deferRender = TRUE,
                     scroller = TRUE,
                     # paging = TRUE,
                     # pageLength = 25,
                     buttons = list(c('excel','csv'),
                                    list(extend = 'colvis', targets = 0, visible = FALSE)
                     ),
                     dom = 'lBfrtip',  #dom = 'lrtip'
                     fixedColumns = TRUE,
                     autoWidth = TRUE,
                     columnDefs = list(list(className = 'dt-center', targets = "_all", searchable = TRUE))
      ),
      rownames = FALSE)
  }
  if(input$EventIndicatorTypeUi == "Proportion"){
    if(input$transpose2x1TableEventUi == TRUE){
      tempProp = twoByTwoTablefunction(data = temp, Var1 = rowVar, Var2 = colVar, spread = TRUE, Proportion = TRUE, spreadVar = "Var1")
    } else{
      tempProp = twoByTwoTablefunction(data = temp, Var1 = rowVar, Var2 = colVar, spread = TRUE, Proportion = TRUE) # spreadVar = "Var2" is the default
    }
    res =  DT::datatable(
      data.frame(tempProp, Total = rowSums(tempProp[,-1], na.rm = TRUE)), # computing row sum
      filter = 'top', extensions = c('Buttons', 'Scroller'),
      options = list(scrollY = '50vh',  #400
                     scrollX = TRUE,  #800
                     deferRender = TRUE,
                     scroller = TRUE,
                     # paging = TRUE,
                     # pageLength = 25,
                     buttons = list(c('excel','csv'),
                                    list(extend = 'colvis', targets = 0, visible = FALSE)),
                     dom = 'lBfrtip', #, use #dom = 'lrtip',
                     fixedColumns = TRUE,
                     autoWidth = TRUE,
                     columnDefs = list(list(className = 'dt-center', targets = "_all"))
      ), 
      rownames = FALSE)
  }
  return(res)
})
# event status pie chart ----- 
output$pieCdhartEventStatusUi <- renderPlotly({
  fg =  pieChartPlot(variable = eventDataDiseaseRegionTimeFilter()$eventstatus)
  return(fg)
})
# event source type pie chart -----
output$pitChartEventSourceTypeUi <- renderPlotly({
  fg =  pieChartPlot(variable = eventDataDiseaseRegionTimeFilter()$srctype_event)
  return(fg)
}) 
# event district pie chart -----
output$pitChartEventDidtrictUi <- renderPlotly({
  fg =  pieChartPlot(variable = eventDataDiseaseRegionTimeFilter()$district_name)
  return(fg)
}) 
# dynamic pie chart based on user specified variable -----
output$pieCdhartEventUi <- renderPlotly({
  if(input$piechartEventVaribleUi == "Event Status"){
    fg =  pieChartPlot(variable = eventDataDiseaseRegionTimeFilter()$eventstatus)
  } else{
    pieVar = input$piechartEventVaribleUi
    temp = eventDataDiseaseRegionTimeFilter()
    pieVar = as.character(temp[, colnames(temp) == pieVar])
    fg =  pieChartPlot(variable = pieVar )
  }
  return(fg)
})
# dynamic bar chart ----
output$barChartEventUi <- renderPlotly({
  if(input$EventIndicatorTypeUi == "Count"){ 
    if(input$barplotEventVaribleUi != "Location category"){
      barVar = input$barplotEventVaribleUi
      temp = eventDataDiseaseRegionTimeFilter()
      barVar = as.character(temp[, colnames(temp) == barVar])
      fg = univariate_barplot(var = barVar, count=TRUE, x_verticalLayout = TRUE )
    } else{
      fg = univariate_barplot(var = eventDataDiseaseRegionTimeFilter()$location_category, count=TRUE, x_verticalLayout = TRUE )
    }
  }
  if(input$EventIndicatorTypeUi == "Proportion"){ 
    if(input$barplotEventVaribleUi != "Location category"){
      barVar = input$barplotEventVaribleUi
      temp = eventDataDiseaseRegionTimeFilter()
      barVar = as.character(temp[, colnames(temp) == barVar])
      fg = univariate_barplot(var = barVar, count=FALSE, x_verticalLayout = TRUE)
    } else{
      fg = univariate_barplot(var = eventDataDiseaseRegionTimeFilter()$location_category, count=FALSE, x_verticalLayout = TRUE)
    }
  }
  return(fg)
})  

## event count by jurisdiction -----
output$eventCountbyJurisdictionTable <- DT::renderDataTable({
  eventVarSelUi =  event_variable_data %>%
    dplyr::filter(event_variable %in% input$eventTableColumnVaribleUi ) # getting category of selected variables
  if(is.null(input$regionEventUi) )
  {
    cuntbyRegionTableEvent = cuntbyRegionDistrictEvent(data = eventDataDiseaseRegionTimeFilter() , byRegion = TRUE )
    cuntbyRegionTableEvent = cuntbyRegionTableEvent[ , (colnames(cuntbyRegionTableEvent) %in%  eventVarSelUi$category) ] #returning only selected columns
    if(input$EventIndicatorTypeUi == "Count"){
      res =  DT::datatable(
        cuntbyRegionTableEvent,
        filter = 'top', extensions = c('Buttons', 'Scroller'),
        options = list(scrollY = 550,
                       scrollX = 500,
                       deferRender = TRUE,
                       scroller = TRUE,
                       # paging = TRUE,
                       # pageLength = 25,
                       buttons = list(c('excel','csv'),
                                      list(extend = 'colvis', targets = 0, visible = FALSE)),
                       dom = 'lBfrtip',
                       fixedColumns = TRUE,
                       autoWidth = TRUE,
                       columnDefs = list(list(className = 'dt-center', targets = "_all"))
        ), 
        rownames = FALSE)
      
    }
    if(input$EventIndicatorTypeUi == "Proportion"){
      res =  DT::datatable(
        #caseCountbyRegionVar(),
        proportionByregion(data = cuntbyRegionTableEvent),
        filter = 'top', extensions = c('Buttons', 'Scroller'),
        options = list(scrollY = 550,
                       scrollX = 500,
                       deferRender = TRUE,
                       scroller = TRUE,
                       # paging = TRUE,
                       # pageLength = 25,
                       buttons = list(c('excel','csv'),
                                      list(extend = 'colvis', targets = 0, visible = FALSE)),
                       dom = 'lBfrtip',
                       fixedColumns = TRUE,
                       autoWidth = TRUE,
                       columnDefs = list(list(className = 'dt-center', targets = "_all"))
        ), 
        rownames = FALSE)
    }
    
  } else{
    cuntbyRegionTableEvent = cuntbyRegionDistrictEvent(data = eventDataDiseaseRegionTimeFilter() , byRegion = FALSE )
    cuntbyRegionTableEvent = cuntbyRegionTableEvent[ , (colnames(cuntbyRegionTableEvent) %in%  eventVarSelUi$category) ] #retaining only selected columns
    if(input$EventIndicatorTypeUi == "Count"){
      res =  DT::datatable(
        cuntbyRegionTableEvent,
        filter = 'top', extensions = c('Buttons', 'Scroller'),
        options = list(scrollY = 550,
                       scrollX = 500,
                       deferRender = TRUE,
                       scroller = TRUE,
                       # paging = TRUE,
                       # pageLength = 25,
                       buttons = list(c('excel','csv'),
                                      list(extend = 'colvis', targets = 0, visible = FALSE)),
                       dom = 'lBfrtip',
                       fixedColumns = TRUE,
                       autoWidth = TRUE,
                       columnDefs = list(list(className = 'dt-center', targets = "_all"))
        ), 
        rownames = FALSE)
    }
    if(input$EventIndicatorTypeUi == "Proportion"){
      res =  DT::datatable(
        proportionByregion(data = cuntbyRegionTableEvent),
        filter = 'top', extensions = c('Buttons', 'Scroller'),
        options = list(scrollY = 550,
                       scrollX = 500,
                       deferRender = TRUE,
                       scroller = TRUE,
                       # paging = TRUE,
                       # pageLength = 25,
                       buttons = list(c('excel','csv'),
                                      list(extend = 'colvis', targets = 0, visible = FALSE)),
                       dom = 'lBfrtip',
                       fixedColumns = TRUE,
                       autoWidth = TRUE,
                       columnDefs = list(list(className = 'dt-center', targets = "_all"))
        ), 
        rownames = FALSE)
    }
  }
  return(res)
})
# event map by administrative area ----
# Commented because this tab is under development
# output$eventMapUi <- renderPlot({
#   map_text_size = input$eventMapTextSizeUi
#   if(input$eventMapShapesUi == "Region")
#   {
#     fg = qtm(shp = regionShapesFrance, text = "libgeo", 
#              text.size = map_text_size, style = "white", format = "World") + tm_compass(type="8star", position=c("left", "top")) # fill.palette = "-Blues" to qhow in reverse order, type = "4star", "8star", "radar", "rose"
#   }
#   if(input$eventMapShapesUi == "Departement")
#   {
#     fg = qtm(shp = departementShapesFrance, text = "codgeo", 
#              text.size = map_text_size, style = "white", format = "World") + tm_compass(type="8star", position=c("left", "top")) 
#   }
#   if(input$eventMapShapesUi == "Commune") 
#   {
#     fg = qtm(shp = CommuneFrance, style = "white", format = "World") + tm_compass(type="8star", position=c("left", "bottom")) 
#   }
#   return(fg)
# })

# # event leaflet map ---- Commented because development is not complete
# # https://rstudio.github.io/leaflet/shiny.html 
# #https://github.com/atmajitg/bloodbanks  to get sample app
# # Reactive expression for the data subsetted to what the user selected
# filteredData <- reactive({
#   eventData[eventData$n_ep >= input$range[1] & eventData$n_ep <= input$range[2],]
# })
# # This reactive expression represents the palette function,
# # which changes as the user makes selections in UI.
# colorpal <- reactive({
#   colorNumeric(input$colors, eventData$n_ep)
# })
# output$map <- renderLeaflet({
#   # Use leaflet() here, and only include aspects of the map that
#   # won't need to change dynamically (at least, not unless the
#   # entire map is being torn down and recreated).
#   leaflet(eventData) %>% addTiles() %>%
#     fitBounds(~min(long), ~min(lat), ~max(long), ~max(lat))
# })
# # Incremental changes to the map (in this case, replacing the
# # circles when a new color is chosen) should be performed in
# # an observer. Each independent set of things that can change
# # should be managed in its own observer.
# 
# observe({
#   pal <- colorpal()
#   leafletProxy("map", data = filteredData()) %>%
#     clearShapes() %>%
#     addCircles(radius = ~10^n_ep/10, weight = 1, color = "#777777",
#                fillColor = ~pal(n_ep), fillOpacity = 0.7, popup = ~paste(n_ep)
#     )
# })
# # Use a separate observer to recreate the legend as needed.
# observe({
#   proxy <- leafletProxy("map", data = eventData)
#   # Remove any existing legend, and only if the legend is
#   # enabled, create a new one.
#   proxy %>% clearControls()
#   if (input$legend) {
#     pal <- colorpal()
#     proxy %>% addLegend(position = "bottomright",
#                         pal = pal, values = ~n_ep )
#   }
# })
# End of event analysis 
