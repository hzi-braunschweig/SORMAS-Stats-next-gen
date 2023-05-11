###### Transmission chain analysis ##########
# This sub section of the server.r file renders the transmission network diagram tab
# All back-end methods related to this tab should be added in this file

# ui element to filter transmission chain by district based of users selected region 
output$pickerInputDistrict2 <- renderUI({
  if(!is.null(input$regionNetworkUi))
  {
    choicesRegionNetworkUI = elist %>%
      dplyr::filter(region_name %in% input$regionNetworkUi)  %>%
      distinct(district_name) %>%
      .$district_name
  }else{
    choicesRegionNetworkUI = NULL
  }
  pickerInput(inputId = 'districtUi2', label = i18n$t('District of infection'),
              choices = choicesRegionNetworkUI, 
              options = list(
                `actions-box` = TRUE, 
                size = 12
              ),
              selected = NULL,
              multiple = TRUE
  )
})
## Filter elist by region, disease, time, etc ----
selElistRegionUI = reactive({
  if(!is.null(input$regionNetworkUi))
  {
    temp =  elist[((elist$region_name %in% input$regionNetworkUi) & (elist$disease == input$diseaseUi) & (elist$reportdatetime >= (min(input$reportdateUi) )  ) & (elist$reportdatetime <= (max(input$reportdateUi) ))), ]
  } else{
    temp = elist[((elist$disease == input$diseaseUi) & (elist$reportdatetime >= (min(input$reportdateUi))) & (elist$reportdatetime <= (max(input$reportdateUi)) )),  ]
  }
  return(temp)
})
##
selElistRegionDistUI = reactive({
  if(is.null(input$districtUi2))
  {
    temp = selElistRegionUI()
  } else{
    temp = selElistRegionUI() %>%
      dplyr::filter(district_name %in% input$districtUi2)
  }
  return(temp)
})
# filtering selElistRegionDistUI by source entity type
selElistRegionEntityTypeUI = reactive({
  if(!is.null(input$contactEntitiyTypeUi))
  {
    temp = selElistRegionDistUI() %>%
      dplyr::filter(entityType %in% input$contactEntitiyTypeUi)
  } else{
    temp = selElistRegionDistUI()
  }
  return(temp)
})
# # filter selElistRegionEntityTypeUI by contact relationship type or  contact settings
# testing code relationCaseUi
selElistRegionEntityTypeSettingUI = reactive({
  if(!is.null(input$relationCaseUi))
  {
    temp = selElistRegionEntityTypeUI() %>%
      dplyr::filter(relationtocase %in% input$relationCaseUi)
  } else{
    temp = selElistRegionEntityTypeUI()
  }
  return(temp)
})

# # filter selElistRegionEntityTypeUI by eventstatus. By default only chains linked to events are retained.
selElistRegionEntityTypeSettingEventstatusUI = reactive({
  if(!is.null(input$eventstatusUI))
  {
    temp = selElistRegionEntityTypeSettingUI() %>%
      dplyr::filter(eventstatus %in% input$eventstatusUI)
  } else{
    temp = selElistRegionEntityTypeSettingUI()
  }
  return(temp)
})

## filter by event risk level
selElistRegionEntityTypeSettingEventstatusRisklevelUI = reactive({
  if(!is.null(input$risklevelUI))
  {
    temp = selElistRegionEntityTypeSettingEventstatusUI() %>%
      dplyr::filter(risklevel_event %in% input$risklevelUI)
  } else{
    temp = selElistRegionEntityTypeSettingEventstatusUI()
  }
  return(temp)
})

# #filtering contacts of a single node
selElistRegionEntityTypeSettingEventstatusRisklevelUISingleNode = reactive({ 
  temp = selElistRegionEntityTypeSettingEventstatusRisklevelUI()
  node_uuidUi = stri_replace_all_fixed(toupper(as.character(input$visSingleNodeUi)), " ", "") # replace all input charecters to upper case
  if(input$visSingleNodeUi !=""){
    temp = temp %>%
      dplyr::filter((from_uuid_person == node_uuidUi) | (to_uuid_person == node_uuidUi))
  } 
  return(temp)
})

# filter to exclude helthy event participants
selElistRegionEntityTypeSettingEventstatusRisklevelUISingleNodeEvenPartHealthy = reactive({
  if(input$excludeHealthyEventPartUi == TRUE){
    temp = selElistRegionEntityTypeSettingEventstatusRisklevelUISingleNode() %>%
      # selecting healthy ep only
      dplyr::filter(is.na(resultingcase_id) & (entityType == "Event")) 
    ret = selElistRegionEntityTypeSettingEventstatusRisklevelUISingleNode() %>%
      dplyr::filter( !(id %in% temp$id)) # dropping dropping edges of healthy ep
  } else{
    ret = selElistRegionEntityTypeSettingEventstatusRisklevelUISingleNode()
  }
  return(ret)
})

# filter  elist  by degree of source node and retain uuid of source nodes only
selElistRegionEntityTypeSettingEventstatusRisklevelUISingleNodeEvenPartHealthySourseCaseUUID = reactive({
  elistSel = selElistRegionEntityTypeSettingEventstatusRisklevelUISingleNodeEvenPartHealthy()  
  ## computing node degree adn add it a column to dataframe
  # For "from nodes" that are not source nodes, the degree would be NA
  elistSel = sourceNodeDegreeCounter(elist=elistSel, nodeLineList = nodeLineList) %>%
    # Comput degree for source nodes for the complete network
    # the source node degree would later be used to filter the network
    dplyr::select(from_uuid_person, deg) %>%
    dplyr::right_join(., elistSel,  c( "from_uuid_person" = "from_uuid_person"))
  # getting source nodes person uuid
  if(!(is.na(input$nodeDegreeMinUi)) & input$nodeDegreeMinUi > 1){
    sourceNodes_uuid = elistSel %>%
      dplyr::filter(deg >= input$nodeDegreeMinUi) %>% # selecting only contacts with source node degree greater than or equal to nodeDegreeMinUi
      dplyr::select(from_uuid_person) %>%
      dplyr::distinct_at(., vars(from_uuid_person), .keep_all = TRUE) %>%
      dplyr::pull(from_uuid_person)
    # getting contact ids and use it to filter elist
    contIdTemp = contIdsForMultipleChains(elist = elistSel, uuid_vector = as.character(sourceNodes_uuid)) # retain list of contact ids
    ret = elistSel[elistSel$id %in% contIdTemp,]
  }else{
    ret = elistSel
  }
  return(ret)
})

## For other developers, 
# you can add further filter here using the object of elist ==  selElistRegionEntityTypeSettingEventstatusRisklevelUISingleNodeEvenPartHealthySourseCaseUUID

# All  dependent filters should come before this section ie should be conditioned on 
# selElistRegionEntityTypeSettingEventstatusRisklevelUISingleNodeEvenPartHealthySourseCaseUUID

# further filtering based on resultingCaseOnlyUi, activeEventsOnlyUi
elistSel2ResCaseSourseCaseArch  = reactive({ 
  elistSel <- elist[elist$id_elist %in% selElistRegionEntityTypeSettingEventstatusRisklevelUISingleNodeEvenPartHealthySourseCaseUUID()$id_elist, ]  
  #Filter elist based on resulting cases varaible
  if(input$resultingCaseOnlyUi == FALSE){
    elistSel2ResCase = elistSel
  } else{
    elistSel2ResCase =  elistSel[is.na(elistSel$resultingcase_id) == FALSE,]
  }
  ##
  if(input$activeEventsOnlyUi == FALSE){
    elistSel2ResCaseArchived = elistSel2ResCase
  } else{
    elistSel2ResCaseArchived =  elistSel2ResCase %>%
      dplyr::filter(archivedEvent == "f")
  }
  
  # filter elist based on source nodes ids
  if(!is.null(input$visSelectedChainsUi)){
    contIdTemp = contIdsForMultipleChains(elist = elistSel2ResCaseArchived, uuid_vector = as.character(input$visSelectedChainsUi)) # retain list of contact ids
    ret = elistSel2ResCaseArchived[elistSel2ResCaseArchived$id %in% contIdTemp,]
  } else{
    ret <- elistSel2ResCaseArchived
  }
  return(ret)
}) 

# Take a dependency on input$transChainAction ie render for elistSel2ResCaseSourseCase only works when the action icon is clicked
# Any output or computation that depend on elistSel2ResCaseSourseCase wouuld run only when input$transChainAction is clicked
elistSel2ResCaseSourseCase = eventReactive(input$transChainAction, { 
  elistSel2ResCaseSourseCaseArch()
}, ignoreNULL = TRUE)

# Ordering column of elist to be plotted, this is needed to be in this specific order
elistToPlot = reactive({
  #renaming node id with node uuid
  ret = elistSel2ResCaseSourseCase() %>%  #        
    dplyr::select(-from, -to)  %>% 
    dplyr::mutate(from = from_uuid_person, to = to_uuid_person, .keep = "all") %>%
    dplyr::relocate(from, to)
  return(ret)
})
##
nodeToPlot = reactive({
  uuid_nodeSel = unique(c(elistToPlot()$from_uuid_person, elistToPlot()$to_uuid_person))
  ret <- nodeLineList %>% 
    dplyr::filter(uuid_node %in% uuid_nodeSel) %>% 
    dplyr::mutate(id = uuid_node, .keep = "unused")  %>%
    dplyr::relocate(id, label, group, value, shape,  code,  title)
  return(ret)
})

## plotting network
output$transChain <- renderVisNetwork({
  if(input$visNetworkDiagramUi == TRUE){
    plotNet(nodeLineList= nodeToPlot(), elist = elistToPlot(), IgraphLayout= input$IgraphLayoutUi) 
  } 
})

## computation of network parameters using transmission network data ----
## total number of contacts
output$totalEdges <- renderInfoBox({
  infoBox(
    title = NULL, 
    value =  nrow(elistSel2ResCaseSourseCase() ),
    icon = icon("long-arrow-alt-right"), 
    color = "black", fill = FALSE, 
    subtitle = "∑ Contact & EP"
  )
})  
## Total number of edges (contacts or event participants) resulting to cases
output$totalReultingcasesEdges <- renderInfoBox({
  temp = elistSel2ResCaseSourseCase() %>%
    dplyr::filter(resultingcase_id != "NA")
  infoBox(
    title = NULL, 
    value = nrow(temp),
    icon = icon("long-arrow-alt-right"), color = colCase, fill = FALSE,
    subtitle = "∑ Contact & EP converted to case"
  )
})   

## Total number of unique infector-infectee pair
# this is the same as contacts resulting to cases
output$totalInfectorInfecteePair <- renderInfoBox({
  temp =  elistSel2ResCaseSourseCase() %>%
    dplyr::filter(resultingcase_id != "NA") # deleting records with missing person id for infector
  # no need to count unique pairs of person since elist has unique pairs of nodes when exported from sormas db
  infoBox(
    title = NULL, 
    value = nrow(temp),
    icon = icon("people-arrows"), color = colCase, fill = FALSE,
    subtitle = "∑ Infector-infectee pair"
  )
})  

## total number of nodes: ie person and events
output$totalNodes <- renderInfoBox({
  infoBox(
    title = NULL, 
    value = length(unique(c(elistSel2ResCaseSourseCase()$from_uuid_person, elistSel2ResCaseSourseCase()$to_uuid_person ))),
    #icon = icon("users")
    icon = icon("user-cog")
    , color = "green", fill = FALSE, 
    subtitle = "∑ Person & event (all nodes)"
  )
}) 
## total number of event nodes
output$totalEventNodes <- renderInfoBox({
  temp = as.numeric(
    elistSel2ResCaseSourseCase() %>%
      dplyr::select(from_uuid_person, entityType ) %>%
      dplyr::filter(entityType == "Event") %>%
      dplyr::distinct_at(. , vars(from_uuid_person))  %>%
      dplyr::summarise(n = n())
  )
  infoBox(
    title = NULL,
    value = temp,
    icon = icon("cog"), color = colEvent, fill = FALSE,
    subtitle = "∑ Event"
  )
}) 
## total number of person nodes
output$totalPersonNodes <- renderInfoBox({
  id_vec = compute_person_node_uuid(elist = elistSel2ResCaseSourseCase()) 
  infoBox(
    title =NULL, 
    value = length(id_vec),
    icon = icon("user"), color = colPerson, fill = FALSE,
    subtitle = "∑ Case & contact & EP person"
  )
}) 
## Total number of nodes (persons) converted to cases
output$totalReultingcasesNodes <- renderInfoBox({
  temp = elistSel2ResCaseSourseCase() %>%
    dplyr::filter(resultingcase_id != "NA") %>%
    dplyr::distinct_at(., vars(to_uuid_person), .keep_all = TRUE) 
  infoBox(
    #title = "∑ Converted", 
    title = NULL, 
    value = nrow(temp),
    icon = icon("user"), color = colCase, fill = FALSE,
    subtitle = "∑ Person converted to case"
  )
})  
## Total number of ep and contact persons 
output$totalContactEpPersonNodes <- renderInfoBox({
  temp = as.numeric(prop_cont_ep_person_convertedToCase(elist = elistSel2ResCaseSourseCase() )$n_contact_ep_nodes)
  infoBox(
    # title = "∑ Person", 
    title = NULL, 
    value = temp,
    icon = icon("user"), color = colCont, fill = FALSE,
    subtitle = "∑ Contact & EP person"
  )
})  
## Proportion of ep and contact person converted to case
output$propContactEpPersonConverted <- renderInfoBox({
  temp = as.numeric(prop_cont_ep_person_convertedToCase(elist = elistSel2ResCaseSourseCase() )$prop_converted)
  infoBox(
    #title = "% Converted", 
    title = NULL,
    value = temp ,
    icon = icon("user"), color = colCase, fill = FALSE,
    subtitle = "% Person converted to case"
  )
}) 

### Computing network parameters using igraph----
graphObject <- reactive({
  net = convertNetworkToGraph(elist=elistSel2ResCaseSourseCase(), nodeLineList=nodeLineList) # converting network to graph object
  return(net)
})
# computing summary of node degree 
output$nodeDegreeSummary <- renderPrint({
  net = graphObject()
  deg <- degree(net, mode="all") 
  summary(deg)  # output
})
# computing summary of node betweeness
output$nodeBetweenessSummary <- renderPrint({
  net = graphObject()
  summary(betweenness(net, directed=F)) 
})
# degree histogram
output$nodeDegreeHist <- renderPlot({
  net = graphObject() 
  deg <- degree(net, mode="all") # 
  hist(deg, breaks=20, xlim = c(1, max(deg)), main = NULL, xlab = "Contacts per case", col = "grey") # breaks=1:vcount(net)-1
})	
# variance-to-mean ratio (VMR) for node degree: 
# https://en.wikipedia.org/wiki/Index_of_dispersion
output$nodeVMR <- renderInfoBox({
  net = graphObject() 
  deg <- degree(net, mode="all") # extract degree
  node_mvr = round(var(deg) / mean(deg), 2)
  infoBox(
    title = NULL, 
    value = node_mvr,
    icon = icon("arrows-alt-h"), color = colEdge, fill = FALSE,
    subtitle = "Variance-to-mean ratio of node degree"
  )
}) 
# Betweeness histogram
output$nodeBetweenessHist <- renderPlot({
  net = graphObject()
  betweenness_score = betweenness(net, directed=F, weights=NA)
  hist(betweenness_score,  breaks=20, xlim = c(0, max(betweenness_score) + 2),main = NULL, col = "grey", xlab = "Betweeness score" )
})

# Edge density: The proportion of present edges from all possible edges in the network.
output$edgeDensity <- renderInfoBox({
  net = graphObject() 
  edge_density = round(edge_density(net, loops=F), 2)
  infoBox(
    title = NULL, 
    value = edge_density ,
    icon = icon("project-diagram"), color = colEdge, fill = FALSE,
    subtitle = "Edge (contact & EP) density"
  )
}) 
# Diameter: longest geodesic distance ie longest undirected chain, thus extract the source case
output$diameterDirected <- renderInfoBox({
  net = graphObject()
  temp = diameter(net, directed=TRUE) # # number of contact in longest chain of infector-infectee pairs
  infoBox(
    title = NULL, 
    value = temp,
    icon = icon("long-arrow-alt-right"), color = colEdge, fill = FALSE,
    subtitle = "Longest directed chain (edge)"
    
  )
}) 
output$diameterUndirected <- renderInfoBox({
  net = graphObject() 
  temp = diameter(net, directed=FALSE) # # number of contact in longest undirected chain
  infoBox(
    title = NULL,
    value = temp,
    icon = icon("expand-alt"), color = colEdge, fill = FALSE,
    subtitle = "Longest undirected chain (edge)"
  )
}) 
## summary of source/ infector nodes for cases and evnet
output$totInfectorCaseEventNodes <- renderInfoBox({
  temp = prop_missing_source_case_nodes(elist= elistSel2ResCaseSourseCase(), nodeLineList = nodeLineList)$sum_caseEvent_nodes
  infoBox(
    title = NULL, 
    value = temp,
    icon = icon("user-cog"), color = colCase, fill = FALSE,
    subtitle = "∑ Infector (case & event nodes)"
  )
}) 
#
output$totSourceInfectorCaseEventNodes <- renderInfoBox({
  temp = prop_missing_source_case_nodes(elist= elistSel2ResCaseSourseCase(), 
                                        nodeLineList = nodeLineList)$sum_missing_source_case_nodes
  infoBox(
    title = NULL,
    value = temp,
    icon = icon("user-cog"), color = colCase, fill = FALSE,
    subtitle = "∑ Source infector (case & event nodes)"
  )
}) 
##
output$propInfectorCaseEventNodes <- renderInfoBox({
  temp = prop_missing_source_case_nodes(elist=elistSel2ResCaseSourseCase(),
                                        nodeLineList = nodeLineList)$prop_missing_source_case_nodes
  infoBox(
    title = NULL, 
    value = temp,
    icon = icon("user-cog"), color = colCase, fill = FALSE,
    subtitle = "% Source infector (case & event nodes)"
  )
}) 
# summary pronting uuid of app parant nodes
output$sourceNodeUUID <- renderPrint({
  temp = prop_missing_source_case_nodes(elist=elistSel2ResCaseSourseCase(),
                                        nodeLineList = nodeLineList)$source_node_uuid
  cat(temp)
})

# Sum of parent source nodes ie infectors (cases, event) with unknown infectors
output$transChainSumUI <- renderInfoBox({
  temp = length(prop_missing_source_case_nodes(elist=elistSel2ResCaseSourseCase(), nodeLineList = nodeLineList)$source_node_uuid)
  infoBox(
    #title = "∑ chains", 
    title = NULL,
    value = temp,
    icon = icon("project-diagram"), color = colCase, fill = FALSE,
    subtitle = "∑ Transmission chains"
  )
}) 
##
output$transitivityScore <- renderInfoBox({
  net = graphObject() 
  temp = round(transitivity(net), 2)
  infoBox(
    title = NULL,
    value = temp,
    icon = icon("project-diagram"), color = colPerson, fill = FALSE,
    subtitle = "Transiticity score (clustering coefficient)"
  )
}) 

# computing case counts by case classification from graphObject 
output$nodeClassificationCountTable <- DT::renderDataTable({
  net =  graphObject() 
  temp = as.data.frame(table(V(net)$group))
  temp = temp %>% rename("Node classification" = Var1, Total = Freq)
  res =  DT::datatable(
    temp,
    options = list(
      dom = 't',
      fixedColumns = TRUE,
      #autoWidth = TRUE,
      columnDefs = list(list(className = 'dt-center', targets = "_all")),
      searching = FALSE
    ), 
    rownames = FALSE)
  
  # }
  return(res)
})