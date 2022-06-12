

#####  server function #######
shinyServer(
  function(input, output,session) { 
    msg <- try({
      showModal(modalDialog(title = NULL, "Loading data from the database.", "Just a minute.", tags$br(), "This message will close automatically when this has finished.", easyClose = TRUE, footer = NULL))
      # connect to sormas_db
      sormas_db = dbConnect(PostgreSQL(), user=DB_USER,  dbname=DB_NAME, password = DB_PASS, host=DB_HOST, port=DB_PORT)
      
      # Extracting user data, Hashing Passwords with sodium
      users = userExport(sormas_db=sormas_db, authenticat_user= authenticat_user_global)
      
      ## Extracting eventData -----
      eventData = eventExport(sormas_db,fromDate = event_fromDate, toDate = event_toDate)
      # adding random lat, lng, and ep to event to event data
      #  to be added to event data export later
      eventData = eventData %>%
        dplyr::mutate(n_ep = rep(5,nrow(eventData)), lat = 10.53128 + rnorm(nrow(eventData)), long = 52.21099 + nrow(eventData) ) %>%
        dplyr::select(-c(latitude, longitude ))
      eventData = as.data.frame(eventData)
      
      ## creating event_variable_data dateset that maps event variables to their categories. This mapping would be used for selecting columns on event table by jurisdiction
      event_variable_data = event_variable_category_maper(cuntbyRegionTableEvent = cuntbyRegionDistrictEvent(data = eventData , byRegion = TRUE ))
      
      # compute location_category varaible for events
      eventData = compute_eventlocation_category(eventData = eventData)
      
      ## Extracting infectorInfecteeData -----
      infectorInfecteeData = infectorInfecteeExport(sormas_db, fromDate = fromDate, toDate = toDate)
      
      ## Extracting contact data ---- 
      # only data frames that matched the configuration in loading_data_config_vector are exported
      # mergingDataFromDB extracts network data, default contact data and serial interval data
      importDataFrontEndOutput = mergingDataFromDB(sormas_db = sormas_db, fromDate = fromDate, 
                                                   toDate = toDate , uniquePersonPersonContact = TRUE)
      if("contRegionDist" %in% loading_data_config_vector){contRegionDist = importDataFrontEndOutput$contRegionDist}
      nodeLineList = importDataFrontEndOutput$nodeLineList  # id here is person id
      elist = importDataFrontEndOutput$elist  # id here is contact id
      #siDat = importDataFrontEndOutput$siDat 
      
      ## The code below would need to be transformed in to independent functions later
      #### loading case data-----
      casePersonRegionDist = case_export(sormas_db, fromDate, toDate)
      
      ## Loading sample data if the feature is activated
      if("sample_table" %in% loading_data_config_vector){sample_table = sample_export(sormas_db, fromDate, toDate)}
      
      #disconnect from db ---- 
      dbDisconnect(sormas_db)
      removeModal()
      
      ## Update UI filters
      ##
      # elist
      updatePickerInput(session = session, inputId = "regionNetworkUi", choices = sort(levels(as.factor(elist$region_name))))
      updatePickerInput(session = session, inputId = "contactEntitiyTypeUi", choices = sort(unique(elist$entityType)))
      updatePickerInput(session = session, inputId = "relationCaseUi", choices = sort(levels(as.factor(elist$relationtocase))))
      updatePickerInput(session = session, inputId = "eventstatusUI", choices = sort(levels(as.factor(elist$eventstatus))))
      updatePickerInput(session = session, inputId = "risklevelUI", choices = sort(levels(as.factor(elist$risklevel_event))))
      # casePersonRegionDist
      updatePickerInput(session = session, inputId = "regionCaseMapUi", choices = sort(levels(as.factor(casePersonRegionDist$region_name))))
      updatePickerInput(session = session, inputId = "regionCaseUi", choices = sort(levels(as.factor(casePersonRegionDist$region_name))))
      updatePickerInput(session = session, inputId = "classificationCaseUi", choices = sort(levels(as.factor(casePersonRegionDist$caseclassification))))
      updatePickerInput(session = session, inputId = "facilityCaseUi", choices = sort(levels(as.factor(casePersonRegionDist$healthfacility_id))))
      updatePickerInput(session = session, inputId = "diseaseCaseUi", choices = sort(levels(as.factor(casePersonRegionDist$disease))), selected = c("CORONAVIRUS"))
      # event_variable_data
      updatePickerInput(session = session, inputId = "eventTableColumnVaribleUi", choices = c(levels(as.factor(event_variable_data$event_variable))), selected = c("Name", "Total", "Event status"))
      # eventData
      updatePickerInput(session = session, inputId = "twoByTwotableEventVariblesUi", choices = c(levels(as.factor(colnames(eventData)))))
      updatePickerInput(session = session, inputId = "piechartEventVaribleUi", choices = c("Event Status",levels(as.factor(colnames(eventData)))), selected = "Event Status")
      updatePickerInput(session = session, inputId = "barplotEventVaribleUi", choices = c("Location category",levels(as.factor(colnames(eventData)))), selected = "Location category")
      updatePickerInput(session = session, inputId = "regionEventUi", choices = sort(levels(as.factor(eventData$region_name))))
      updatePickerInput(session = session, inputId = "eventIdentificationSourceUi", choices = sort(levels(as.factor(eventData$event_identification_source))))  
      updateSliderInput(session = session, inputId = "range", min =  min(eventData$n_ep), max = max(eventData$n_ep), value = range(eventData$n_ep))
      updatePickerInput(session = session, inputId = "diseaseEventUi", choices = sort(levels(as.factor(eventData$disease_event))), selected = c("CORONAVIRUS"))
      # contRegionDist
      updatePickerInput(session = session, inputId = "regionContactUi", choices = sort(levels(as.factor(contRegionDist$region_name))))
      # sample_table
      updatePickerInput(session = session, inputId = "diseaseSampleUi", choices = sort(levels(as.factor(sample_table$disease_sample))), selected = c("CORONAVIRUS"))
      updatePickerInput(session = session, inputId = "reportdateSampleUi", choices = sort(levels(as.factor(sample_table$date_of_report))))
      updatePickerInput(session = session, inputId = "regionSampleUi", choices = sort(levels(as.factor(sample_table$region_name))))
      updatePickerInput(session = session, inputId = "bargraphSampleVariableUi", choices = sort(c("pathogentestresult","samplingreason","samplepurpose","shipped","received",
                                                                                                  "specimencondition","samplesource","samplematerial"))  )
      updatePickerInput(session = session, inputId = "samplepurposeUi", choices = sort(levels(as.factor(sample_table$samplepurpose))))
      })
    if(inherits(msg, "try-error")){
      showModal(modalDialog(title = "Startup error", as.character(msg), easyClose = TRUE))
    }
    #################################################
    # Call login module and supplying it with the user dataframe, 
    # username and password calls and reactive trigger
    credentials <- shinyauthr::loginServer(
      id = "login",
      data = users,
      user_col = username,
      pwd_col = password,
      sodium_hashed = TRUE,
      log_out = reactive(logout_init())
    )
    
    # call the logout module with reactive trigger to hide/show
    
    logout_init <- shinyauthr::logoutServer(
      id = "logout",
      active = reactive(credentials()$user_auth)
    )
    ########################################
    
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
      pickerInput(inputId = 'districtUi2', label = 'District of infection',
                  choices = choicesRegionNetworkUI, 
                  options = list(
                    `actions-box` = TRUE, 
                    size = 12
                  ),
                  selected = NULL,
                  multiple = TRUE
      )
    })
    
  ###### Trasmission chain analysis ######################################

# plotting network -----
## Filter elist by region, disease,  time, etc
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
####################
# Filtering elist by node degree
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
  elistSel2ResCaseSourseCaseArch() #elistSel2ResCaseSourseCaseUUID()
}, ignoreNULL = FALSE)

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
  req(credentials()$user_auth)
  if(input$visNetworkDiagramUi == TRUE){
    plotNet(nodeLineList= nodeToPlot(), elist = elistToPlot(), IgraphLayout= input$IgraphLayoutUi) 
  } 
})

  ## computation of network parameters using transmission network data ----
  ## total number of contacts
  output$totalEdges <- renderInfoBox({
    req(credentials()$user_auth)
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
    req(credentials()$user_auth)
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
    req(credentials()$user_auth)
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
    req(credentials()$user_auth)
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
    req(credentials()$user_auth)
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
    req(credentials()$user_auth)
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
    req(credentials()$user_auth)
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
    req(credentials()$user_auth)
    temp = as.numeric(prop_cont_ep_person_convertedToCase(elist = elistSel2ResCaseSourseCase() )$n_contact_ep_nodes)
    infoBox(
     # title = "∑ Person", 
      title = NULL, 
      value = temp,
      icon = icon("user"), color = colCont, fill = FALSE,
      subtitle = "∑ Contact & EP person"
      
    )
  })  

  ## Prpportion of ep and contact person converted to case
  output$propContactEpPersonConverted <- renderInfoBox({
    req(credentials()$user_auth)
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
     req(credentials()$user_auth)
    net = convertNetworkToGraph(elist=elistSel2ResCaseSourseCase(), nodeLineList=nodeLineList) # converting network to graph object
    return(net)
  })
 
  # computing summary of node degree 
  output$nodeDegreeSummary <- renderPrint({
     req(credentials()$user_auth)
    net = graphObject()
    deg <- degree(net, mode="all") 
    summary(deg)  # output
  })
  
  # computing summary of node betweeness
  output$nodeBetweenessSummary <- renderPrint({
     req(credentials()$user_auth)
    net = graphObject()
    summary(betweenness(net, directed=F)) 
  })
  
  # degree histogram
  output$nodeDegreeHist <- renderPlot({
     req(credentials()$user_auth)
    net = graphObject() 
    deg <- degree(net, mode="all") # 
    hist(deg, breaks=20, xlim = c(1, max(deg)), main = NULL, xlab = "Contacts per case", col = "grey") # breaks=1:vcount(net)-1
  })	
  # variance-to-mean ratio (VMR) for node degree: 
  # https://en.wikipedia.org/wiki/Index_of_dispersion
  output$nodeVMR <- renderInfoBox({
     req(credentials()$user_auth)
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
     req(credentials()$user_auth)
    net = graphObject()
    betweenness_score = betweenness(net, directed=F, weights=NA)
    hist(betweenness_score,  breaks=20, xlim = c(0, max(betweenness_score) + 2),main = NULL, col = "grey", xlab = "Betweeness score" )
  })
  
  # Edge density: The proportion of present edges from all possible edges in the network.
  output$edgeDensity <- renderInfoBox({
     req(credentials()$user_auth)
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
     req(credentials()$user_auth)
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
     req(credentials()$user_auth)
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
     req(credentials()$user_auth)
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
    req(credentials()$user_auth)
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
     req(credentials()$user_auth)
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
     req(credentials()$user_auth)
    temp = prop_missing_source_case_nodes(elist=elistSel2ResCaseSourseCase(),
                   nodeLineList = nodeLineList)$source_node_uuid
    cat(temp)
  })
  
  # Sum of parent source nodes ie infectors (cases, event) with unknown infectors
  output$transChainSumUI <- renderInfoBox({
     req(credentials()$user_auth)
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
     req(credentials()$user_auth)
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
    req(credentials()$user_auth)
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
## end of transmission network analysis

## CONTACT DATA ANALYSIS-----
  
# begin of contact filter
# Filter contact data  based on region district time and disease
contRegionDistDiseaseDate = reactive({
   req(credentials()$user_auth)
  if(is.null(input$regionContactUi))
  {
    contRegionDist[((contRegionDist$disease == input$diseaseContactUi) & (contRegionDist$reportdatetime >= (min(input$reportdateContactUi))) & (contRegionDist$reportdatetime <= (max(input$reportdateContactUi)) )), ]
  } else{
    contRegionDist[((contRegionDist$region_name %in% input$regionContactUi) & (contRegionDist$disease == input$diseaseContactUi) & (contRegionDist$reportdatetime >= (min(input$reportdateContactUi) )  ) & (contRegionDist$reportdatetime <= (max(input$reportdateContactUi) ))),]
  }
})
# Adding control based on contactDataAnalysisAction icon on front ui
# Any output or computation that depend on contRegionDistDiseaseDate wouuld run only when input$contactDataAnalysisAction is clicked
d = eventReactive(input$contactDataAnalysisAction, { 
  contRegionDistDiseaseDate() 
}, ignoreNULL = FALSE)
  
# end of contact filter  
# Begin of contact analysis    
## Bar plot
output$plot <- renderPlot({
  if(nrow(d( ) ) == 0)
  {
    plot(NULL, xlim=c(0,1), ylim=c(0,1), ylab="Number of contacts", xlab=" ",
         main = "No data exist based on your selection, please choose another selection for which data exist")
  }else{
    if(is.null(input$regionContactUi) )
    {
      par(las=2, mar=c(7,4,4,2)) # make label text perpendicular to axis
      barplot (table(as.factor(d( )$region_name)), ylab = "Number of contacts",main = "Bar plot for number of contacts")
    } else {
      par(las=2, mar=c(7,4,4,2)) # make label text perpendicular to axis
      barplot (table(as.factor(d( )$district_name)), ylab = "Number of contacts",main = "Bar plot for number of contacts")
    }
  }
}, height=700)

    ## exporting bar plot
    output$downloadBarplot = downloadHandler(
      filename = function(){
        paste("barPlotContacts", "png", sep = ".")
      },
      content = function(file){
        png(file)
        ##
          if(is.null(input$regionContactUi) )
          {
            par(las=2, mar=c(7,4,4,2)) # make label text perpendicular to axis
            barplot (table(as.factor(d( )$region_name)), ylab = "Number of contacts",main = "Bar plot for number of contacts")
          } else {
            par(las=2, mar=c(7,4,4,2)) # make label text perpendicular to axis
            barplot (table(as.factor(d( )$district_name)), ylab = "Number of contacts",main = "Bar plot for number of contacts")
          }
        ##
        dev.off()
      }
    )

### Begining of  contact KPI ###########
    KPIValueBox_Server("contact", data= d,panel="contact",boxfun=box)

##########  end of KPI #################""
  
#Contacts per case analysis begin----
if(contact_per_case_plot=="t"){
  
  p = reactive({
    data.frame(as.table(summary(as.factor(d()$caze_id), maxsum = 5000000)))
  })
    minLim = reactive({
      c( min(p()$Freq), max(p()$Freq)+20)
    })
    output$plotContPerCase <- renderPlot({
      if(nrow(d( ) ) == 0)
      {
        plot(NULL, xlim=c(0,1), ylim=c(0,1), ylab="Number of contacts", xlab=" ",
             main = "No data exist based on your selection, please choose another selection for which data exist")
      }else{
        par(mar=c(5,4,4,1), mfrow = c(1,2))
        hist(p()$Freq, breaks = 50,  xlim = minLim(), col = "grey", main = "Histogram of number of contacts per case", xlab = "Number of contacts per case")
        plot(sort(p()$Freq), col="black", cex=1, pch=16, xlab = "Case index", ylab = "Number of contacts",
             main = "Number of contacts per case")
      }
    }, height=700)
    ## exporting contact per case plot
    output$downloadContPerCasePlot = downloadHandler(
      filename = function(){
        paste("contactPerCasePlot", "png", sep = ".")
      },
      content = function(file){
        png(file)
        ##
        par(mar=c(5,4,4,1), mfrow = c(1,2))
        hist(p()$Freq, breaks = 50,  xlim = minLim(), col = "grey", main = "Histogram of number of contacts per case", xlab = "Number of contacts per case")
        plot(sort(p()$Freq), col="black", cex=1, pch=16, xlab = "Case index", ylab = "Number of contacts",
             main = "Number of contacts per case")
        ##
        dev.off()
      })}
#Contacts per case analysis ends----
#Contacts per case export begin -----
if(contact_per_case_export == "t"){
conPerCaseExp = reactive({
      p() %>% dplyr::rename(Case_id = Var1, Nunber_of_contacts = Freq )
    })
output$conPerCaseExpCsv <- downloadHandler(
      filename = function() {
        paste("sormas_contactPerCaseExp_", Sys.Date(), ".csv", sep="")
      },
      content = function(file) {
        write.csv(conPerCaseExp(), file)})
output$conPerCaseExpTable <- renderPrint({
      orig <- options(width = 1000)
      print(head(conPerCaseExp(), input$maxrows), row.names = FALSE)
      options(orig)
    })}
#Contacts per case export ends -----
#Contact per region export begin ----
    conPerGerionExp = reactive({
      temp =  data.frame(table(as.factor(d( )$region_name)))
      colnames(temp) = c("Region_name", "Number_of_Contacts")
      temp = temp[order(temp$Number_of_Contacts, decreasing = T), ]
      return(temp)
    })
    output$conPerGerionExpCsv <- downloadHandler(
      filename = function() {
        paste("sormas_conPerGerionExp_", Sys.Date(), ".csv", sep="")
      },
      content = function(file) {
        write.csv(conPerGerionExp(), file)
      }
    )
    output$conPerGerionExpTable <- renderPrint({
      orig <- options(width = 1000)
      print(head(conPerGerionExp(), input$maxrowsContByRegion), row.names = FALSE)
      options(orig)
    })
#Contact per region export ends----
# UI output for "contact data analysis" tab ----
# This output object "contact_analysis_output" should be placed below the computation of elements needed in it
output$contact_analysis_output <- renderUI({
panels <- list(
tabPanel("Contact dashboard",
#wellPanel(style = "background: white", 
 fluidRow(width = 12,
          KPIValueBox_UI("contact")
#)
),
plotOutput("plot", width = "100%", height = "90vh"),downloadButton("downloadBarplot", "Download this plot")
, tags$br(),tags$br(),
" Each bar in this plot represents a region or district and the height of the bar corresponds to the number of contacts 
                 in the region or district."
),
tabPanel("Contact per region export", icon = icon("table"),
 width = 10,
 dashboardPage( # the use of shiny dashboard is to make sure that all icons are fine, do not remove or deactivate this tab.
   dashboardHeader( ),
   dashboardSidebar( disable = TRUE,
                     pickerInput("conPerson", "Contact entity type", choices = c("Contact", "Contact person"), # option to view contact or contact person
                                 selected = c("Contact"),
                                 multiple = FALSE)
   ),
   dashboardBody(
     numericInput("maxrowsContByRegion", "Rows to show", 20),
     verbatimTextOutput("conPerGerionExpTable"),
     downloadButton("conPerGerionExpCsv", "Download as CSV"),tags$br(),tags$br(),
     "Each row in this data is a region with corresponding number of contacts.
                     The data was obtained by summing the number of contacts in each region.
                     The resgion of the source case was used in case the region of the contact was missing." )
 ))   )
  if(contact_per_case_plot=="t"){
    panels[[3]]  = tabPanel("Contact per case plot", plotOutput("plotContPerCase", width = "100%", height = "90vh"),  downloadButton("downloadContPerCasePlot", "Download this plot"), tags$br(),tags$br(),
                                     " Contact per case.")}
  if(contact_per_case_export == "t"){
    panels[[4]] <-tabPanel("Contacts per case export",
      numericInput("maxrows", "Rows to show", 20),
      verbatimTextOutput("conPerCaseExpTable"),
      downloadButton("conPerCaseExpCsv", "Download as CSV"),tags$br(),tags$br(),
      "Each row in this data is a case. The data was obtained by summing the number of contacts for each case. Cases with no contact are not included in this table")
  }
  base::do.call(tabsetPanel, panels)
})     
         
#### CASE DATA ANALYSIS  #################----
# ui element to filter casePersonRegionDist by district based on user selected region 
output$pickerInputdistrictCaseUi <- renderUI({
  if(!is.null(input$regionCaseUi))
  {
    temp = casePersonRegionDist %>%
      dplyr::filter(region_name %in% input$regionCaseUi)  %>%
      distinct(district_name) %>%
      .$district_name
  }else{
    temp = NULL
  }
  pickerInput(inputId = 'districtCaseUi', label = 'District of case',
              choices = temp, 
              options = list(
                `actions-box` = TRUE, 
                size = 12
              ),
              selected = NULL,
              multiple = TRUE
  )
})

    # Filtering casepersonRegion
    casePersonRegionFilter = reactive({
      if(is.null(input$regionCaseUi))
      {
        casePersonRegionDist[((casePersonRegionDist$disease == input$diseaseCaseUi) & (casePersonRegionDist$reportdate >= (min(input$reportdateCaseUi))) & (casePersonRegionDist$reportdate <= (max(input$reportdateCaseUi)) )), ]
      } else{
        casePersonRegionDist[((casePersonRegionDist$region_name %in% input$regionCaseUi) & (casePersonRegionDist$disease == input$diseaseCaseUi) & (casePersonRegionDist$reportdate >= (min(input$reportdateCaseUi) )  ) & (casePersonRegionDist$reportdate <= (max(input$reportdateCaseUi) ))),]
      }

    })
  
    # fiter by district of cose
    casePersonRegionDistFilter = reactive({
       req(credentials()$user_auth)
      if(is.null(input$districtCaseUi))
      {
        temp = casePersonRegionFilter()
      } else{
        temp = casePersonRegionFilter() %>%
          dplyr::filter(district_name %in% input$districtCaseUi)
      }
      return(temp)
      
    })
    
    # Adding control based on apply changes icon on front ui
    # Any output or computation that depend on casePersonFilter wouuld run only when input$caseDataAnalysisAction is clicked
    casePersonFilter = eventReactive(input$caseDataAnalysisAction, { 
      casePersonRegionDistFilter() 
    }, ignoreNULL = FALSE)
    
    #### case KPI ################
    KPIValueBox_Server("case",data=casePersonFilter, panel="case",boxfun=tabBox)

    ############### Case tables  ###########
    # This option to ecport cases line-list is disable for now till further notice
    # # casePersonRegionDist export begin
    # casebyRegionVar = reactive({
    #   casePersonFilter()
    # })
    # # output to download data
    # output$caseRegionVarExpCsv <- downloadHandler(
    #   filename = function() {
    #     paste("sormas_export_case_region_", Sys.Date(), ".csv", sep="")
    #   },
    #   content = function(file) {
    #     write.csv(casePersonFilter(), file)
    #   }
    # )
    # output$casebyRegionVarTable <- renderPrint({
    #   orig <- options(width = 1000)
    #   print(head(casebyRegionVar(), input$maxrowsCase), row.names = FALSE)
    #   options(orig)
    # })
    # # casePersonRegionDist export end
    
    ## Case sum by region and other variables ######
    # tabulate cases by region jurisdiction
    output$caseCountbyRegionTable <- DT::renderDataTable({
      
      if(is.null(input$regionCaseUi) ){
        cuntbyRegionTable = cuntbyRegionDistrictCase(data = casePersonFilter(), byRegion = TRUE )
        if(input$caseByRegionIndicatorTypeUi == "Count"){
          res =  DT::datatable(
            cuntbyRegionTable,
            #cuntbyRegion(data = casePersonFilter() ),
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
        if(input$caseByRegionIndicatorTypeUi == "Proportion"){
          res =  DT::datatable(
            #caseCountbyRegionVar(),
            proportionByregion(data = cuntbyRegionTable),
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
        
      } else {
        cuntbyRegionTable = cuntbyRegionDistrictCase(data = casePersonFilter(), byRegion = FALSE )
        if(input$caseByRegionIndicatorTypeUi == "Count"){
          res =  DT::datatable(
            cuntbyRegionTable,
            #cuntbyRegion(data = casePersonFilter() ),
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
        if(input$caseByRegionIndicatorTypeUi == "Proportion"){
          res =  DT::datatable(
            #caseCountbyRegionVar(),
            proportionByregion(data = cuntbyRegionTable),
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
    
    ## plotting case pyramid ########
    output$casePyramidPlot <- renderPlotly({
      temp = casePersonFilter()
      if(input$sexCategoryUi == "Male X Female")
      {
        fg =  pyramidPlotFunction(data = temp, sexCat = "MaleFemale")
      }
      if(input$sexCategoryUi == "Male X Others")
      {
        fg =  pyramidPlotFunction(data = temp, sexCat = "MaleOther")
      }
      if(input$sexCategoryUi == "Female X Others")
      {
        fg = pyramidPlotFunction(data = temp, sexCat = "FemaleOther")
      }
      return(fg)
    })

   #### Plotting time series plot for cases ###
    output$caseTimeSeriesPlot <- renderPlotly({
      temp = casePersonFilter()

      if (input$timeUnitUi == "Day")
      {
        if(input$byRegiontimeUnitUi == F)
        {
          dateSumCase = stats::aggregate(total ~ reportdate, data = temp, sum, na.rm = F)
          fg=  timeSeriesPlotDay(data = dateSumCase, cum = input$cumUi )

        } else{
          dateSumCaseRegion = stats::aggregate(total ~ reportdate + region_name, data = temp, sum, na.rm = F)
          #fg = timeSeriesPlotDayRegion(data = dateSumCaseRegion)
          fg = timeSeriesPlotDayRegion(data = dateSumCaseRegion, cum = input$cumRegionUi)

        }

      }
      if (input$timeUnitUi == "Epi-week")
      {
        weekSumCase = stats::aggregate(total ~ reportweek+ reportyear, data = temp, sum, na.rm = F)
        fg=   timeSeriesPlotWeek(data = weekSumCase )
      }
      if (input$timeUnitUi == "Month")
      {
        monthSumCase = stats::aggregate(total ~ reportmonth+ reportyear, data = temp, sum, na.rm = F)
        fg=  timeSeriesPlotMonth(data = monthSumCase )
      }
      return(fg)

    })
   ###

    ## Plotting epicurve
    output$caseEpicurvePlot <- renderPlotly({
      temp = casePersonFilter()
      if (input$timeUnitEpicurveUi == "Day")
      {
        dateSumCaseClass = stats::aggregate(total ~ reportdate + caseclassification, data = temp, sum, na.rm = F)
        fg=  epicurveDate(data = dateSumCaseClass)
      }
      if (input$timeUnitEpicurveUi == "Epi-week")
      {
        dateSumCaseClass = stats::aggregate(total ~ reportdate + caseclassification, data = temp, sum, na.rm = F)
        fg=  epicurveDate(data = dateSumCaseClass)
      }
      if (input$timeUnitEpicurveUi == "Month")
      {
        fg =   epicurveMonth(data = temp)
      }
      return(fg)
    })

    ### Maps of cases ####
    ## map plot
    output$regionMapCaseCount <- renderPlot({
      if(input$caseMapshapesUi == "By region")
      {
        fg = regionMapPlot(data = casePersonFilter(), lnd = regionShapes)
      }
      if(input$caseMapshapesUi == "By district")
      {
        fg = districtMapPlot(data = casePersonFilter(), districtShapes =districtShapes)
      }
      return(fg)
    })

    ## Rt analysis and plotting-----
    #####
    # using casePersonFilter() and infectorInfecteeData
    # Filtering by disease, time, region of infector and  deduplication (unique infector-infectee persons)
    infectorInfecteeDataDiseaseRegionFilter = reactive({
      req(credentials()$user_auth)
      if(is.null(input$regionCaseUi))
      {
        ret = infectorInfecteeData %>% 
          dplyr::filter(disease_infector == input$diseaseCaseUi) %>% 
          dplyr::filter(report_date_infector >= input$reportdateCaseUi[1] & report_date_infector <= input$reportdateCaseUi[2] )
      } else{
        ret = infectorInfecteeData %>% 
          dplyr::filter( disease_infector == input$diseaseCaseUi & region_infector %in% input$regionCaseUi) %>% 
          dplyr::filter((report_date_infector >= input$reportdateCaseUi[1]) & (report_date_infector <= input$reportdateCaseUi[2]) )
      }
      # deduplication,  keeping only unique infector-infectee persons
      ret = ret %>% 
        dplyr::distinct_at(. , vars(person_id_case_infector, person_id_case_infectee), .keep_all = TRUE) 
      return(ret)
    })  
    
    # filter by district of infector_case  
    infectorInfecteeDataDiseaseRegionDist = reactive({
      if(is.null(input$districtCaseUi))
      {
        ret = infectorInfecteeDataDiseaseRegionFilter() 
      } else{
        ret = infectorInfecteeDataDiseaseRegionFilter() %>%
          dplyr::filter(district_infector %in% input$districtCaseUi)
      }
      return(ret)
    })
    
    #Rendering and showing ui element for mean and sd  based on user defined choice of "SI estimation method": parmetric or si_from_data
    # Only show mean and sd when method == parametric
    observe({
          if(input$rtMethodUi == "Parametric distribution"){
          shinyjs::show(id = "showMeanSdSIUI", anim = TRUE)
        } 
        else {
          shinyjs::hide(id = "showMeanSdSIUI", anim = TRUE)
        } 
    })
    
    # filter by serial interval range 
    # Adding control based on apply changes icon on front ui
    ################ begin debuging
    ## Wrapper for debugging errors 
    base::try({
      # The withLogErrors call ensures that stack traces are captured
      # and that errors that bubble up are logged using warning().
      shiny::withLogErrors({
        # tryCatch and withVisible are just here to add some noise to
        # the stack trace.
        base::tryCatch(
          base::withVisible({
            # add function here whose output should be traced 
            infectorInfecteeDataDiseaseRegionDistSerialIntFilter  =  eventReactive(input$caseDataAnalysisAction, {
              temp = infectorInfecteeDataDiseaseRegionDist() %>%
                tidyr::drop_na(serial_interval) %>%    # Dropping rows with NA fo SI
                dplyr::filter(., serial_interval %in% c(input$serialIntervalRangeUi[1] : input$serialIntervalRangeUi[2]))  # filter by SI range
              return(temp)
            }, ignoreNULL = FALSE)
            
          })
        )
      })
    })
    ################ end debuging
    
    #Preparing data and estimate rt
    rt_data = eventReactive(input$caseDataAnalysisAction, {
      # This is a list of all the data needed to estimate and plot Rt
      # The two data sets used are the complete case line listing casePersonFilter and SI data 
      # preparing si_data data
      rt_analysis_data = casePersonFilter()  
      rt_analysis_data$total = 1
      dateSumCase = stats::aggregate(total ~ reportdate, data = rt_analysis_data, FUN = sum, na.rm = T)  
      # completting missing dates
      dateSumCase =  dateSumCase %>%
        dplyr::mutate(Date = as.Date(reportdate)) %>%
        tidyr::complete(Date = seq.Date(min(Date), max(Date), by="day"), fill = list(total = 0))
      dateSumCase = dateSumCase[,c(1,3)] # dropping old uncompletted date
      colnames(dateSumCase) = c("dates","I")
      
      temp = infectorInfecteeDataDiseaseRegionDistSerialIntFilter()   # data having SI as column
      # extracting SI values that are not NA, negative and fall in the range specifird by user
      temp =  temp %>%
        dplyr::filter((serial_interval > 0) & (serial_interval <= input$siUi))
      n = nrow(temp)
      si_data = data.frame(matrix(0,n,5))
      si_data[,2] = 1
      si_data[,3] = c(temp$serial_interval -1)
      si_data[,4] = temp$serial_interval
      colnames(si_data) =  c("EL", "ER", "SL", "SR", "type")
      si_data[,-5] = apply(si_data[,-5], 2, as.integer) # all columns except type should be integer
      
      #estimating and computing summary of mean rt
      if(input$rtMethodUi == "Parametric distribution"){
        distVec = c(Gamma = "G", Weibull = "W", Lognormal = "L") 
        distUI = distVec[names(distVec) == input$si_rt_UI] # getting short form of distribution names to be used in modelling
        ret = RtPlot(mean_si = input$mean_siUI, std_si = input$std_siUI, method = "parametric_si",  burnin = 1000, dateSumCase = dateSumCase,
                          si_data = si_data, rsi = input$rsiUi, dist = distUI, rt_legend = input$rtLegandUi) # method = "parametric_si" or "si_from_data"; rsi = "all", "R", "SI"
      }
      if(input$rtMethodUi == "Transmission data" ){
        ret =  RtPlot(dateSumCase = dateSumCase, method = "si_from_data",  burnin = 1000,  si_data = si_data, rsi = input$rsiUi, rt_legend = input$rtLegandUi) # method = "parametric_si" or "si_from_data"; rsi = "all", "R", "SI"
      }
      return(ret) # retuned vector of estimated rt and plot of estimated rt
    }, ignoreNULL = FALSE) 
    
    #plotting Rt
    output$rtPlot <- renderPlot({
      fig = rt_data()$rt_fig # extracting figure component in rt_data
      return(fig)
    })
    # Computing summary stats for rt
     output$rtSummary_table <- DT::renderDataTable({
       temp = round(summary(rt_data()$rt_mean), 2) # computing summary of extracted bootstrap means
       #converting summary output to dataframe
       temp2 = data.frame(x=t(matrix(temp)))
       names(temp2) = names(temp)
       rownames(temp2) = c("")
       res = DT::datatable(temp2,
                           options = list(
                             dom = 't',
                             fixedColumns = TRUE,
                             #autoWidth = TRUE,
                             columnDefs = list(list(className = 'dt-center', targets = "_all")),
                             searching = FALSE
                           ), 
                           rownames = FALSE )
       return(res)
     })
    
## SI analysis
    # model selection for SI  
# Addting congtrols to print errors
################ begin debuging
 base::try({
   # The withLogErrors call ensures that stack traces are captured
   # and that errors that bubble up are logged using warning().
   shiny::withLogErrors({
     # tryCatch and withVisible are just here to add some noise to
     # the stack trace.
     base::tryCatch(
       base::withVisible({
         # add function here whose output should be traced 
         
         # fiting normal, weibull, gamma, lnorm distributions to serial intervals 
         output$si_model_fitTable <- DT::renderDataTable({
           temp = fit_distribution(serial_interval = infectorInfecteeDataDiseaseRegionDistSerialIntFilter()$serial_interval)
           res = DT::datatable(temp,
                               options = list(
                                 dom = 't',
                                 fixedColumns = TRUE,
                                 #autoWidth = TRUE,
                                 columnDefs = list(list(className = 'dt-center', targets = "_all")),
                                 searching = FALSE
                               ), 
                               rownames = FALSE )
           return(res)
         }) 
         
       })
     )
   })
 })
########### end debuging
    
# summary statistics for SI
output$si_summaryTable <- DT::renderDataTable({
  temp = summary_statistics(x = infectorInfecteeDataDiseaseRegionDistSerialIntFilter()$serial_interval)
  res = DT::datatable(
    data = temp,
    options = list(
      dom = 't',
      fixedColumns = TRUE,
      #autoWidth = TRUE,
      columnDefs = list(list(className = 'dt-center', targets = "_all")),
      searching = FALSE
    ), 
    rownames = FALSE)
  return(res)
})
    
# plotting distributions for model selection
# plotting distribution of data
output$SI_hist_model_plot <- renderPlot({
  temp = fitdist_plot(x = infectorInfecteeDataDiseaseRegionDistSerialIntFilter()$serial_interval)
  temp$density
})
#qq plot for si
output$SI_model_qq_plot <- renderPlot({
  temp = fitdist_plot(x = infectorInfecteeDataDiseaseRegionDistSerialIntFilter()$serial_interval)
  temp$qq
})

#cdf plot for si
output$SI_model_cdf_plot <- renderPlot({
  temp = fitdist_plot(x = infectorInfecteeDataDiseaseRegionDistSerialIntFilter()$serial_interval)
  temp$cdf
})
    
# computing mean CI based on user specified distribution
# Delay rectivity to comput Meam estimate and 95% CI for serial interval based on distribtion with best fit to the data
# bases on serial interval range, distribution, etc
si_mean_CI_table_data = eventReactive(input$caseDataAnalysisAction, { 
  ret_mean_CI = serial_interval_mean_CI(infectorInfecteePair = infectorInfecteeDataDiseaseRegionDistSerialIntFilter(),
                                        distr = input$siDistMethodUi, input$serialIntervalRangeUi[1], maxSi = input$serialIntervalRangeUi[2])   
}, ignoreNULL = FALSE)
    
output$si_mean_CI_table <- DT::renderDataTable({
  res = DT::datatable(
    data = si_mean_CI_table_data() ,
    options = list(
      dom = 't',
      fixedColumns = TRUE,
      #autoWidth = TRUE,
      columnDefs = list(list(className = 'dt-center', targets = "_all")),
      searching = FALSE
    ), 
    rownames = FALSE)
  return(res)
})
     
# fitting user chosen distribution to SI
# Any output or computation that depend on siRet (ie data and parameters used to generate siRet) wouuld run only when input$caseDataAnalysisAction is clicked
##### beging bebug
## Wrapper for debugging errors 
base::try({
  # The withLogErrors call ensures that stack traces are captured
  # and that errors that bubble up are logged using warning().
  shiny::withLogErrors({
    # tryCatch and withVisible are just here to add some noise to
    # the stack trace.
    base::tryCatch(
      base::withVisible({
        # add function here whose output should be traced 
        siRet = eventReactive(input$caseDataAnalysisAction, { 
          temp = infectorInfecteeDataDiseaseRegionDistSerialIntFilter()
          siRet = serialIntervalPlot(infectorInfecteePair = temp,  distr = input$siDistMethodUi, niter = input$niter_SI_UI,
                                     minSi = input$serialIntervalRangeUi[1], maxSi = input$serialIntervalRangeUi[2] ) 
          return(siRet)
        }, ignoreNULL = FALSE)
      })
    )
  })
})
##### end bebug

# plotting SI
output$distribution_SI_plot <- renderPlot({
  temp = siRet()
  temp$siDistributionPlot
})

# exporting estimates of SI
output$SI_estimate_table <- DT::renderDataTable({
  temp = siRet()
  res = DT::datatable(
    data =  temp$siEstmate,
    options = list(
      dom = 't',
      fixedColumns = TRUE,
      #autoWidth = TRUE,
      columnDefs = list(list(className = 'dt-center', targets = "_all")),
      searching = FALSE
    ), 
    rownames = TRUE)
  return(res)
})

# Offspring distribution and estimation of dispersion parameter k
# conditioning all estimates from kRet to deleay response based on the caseDataAnalysisAction icon
# Since infectore-infectee pairs with NA for serial interval can be included in offspring analysis
# we do not need to use infectorInfecteeDataDiseaseRegionDistSerialIntFilter() but to use infectorInfecteeDataDiseaseRegionDist()

# summary statistics for offsring distribtion
nodedegree_summaryTab = eventReactive(input$caseDataAnalysisAction, { 
  # compute node degree
  temp = offspringDistPlot(infectorInfecteePair = infectorInfecteeDataDiseaseRegionDist(), polyDegree = input$polyDegree_RtK_UI)
  # compute summary statistics table
  ret = summary_statistics(x = temp$offspringDegree )
  return(ret)
}, ignoreNULL = FALSE)
# exporting nodedegree_summaryTab table to ui
output$nodedegree_summaryTable <- DT::renderDataTable({
  res = DT::datatable(
    data = nodedegree_summaryTab() ,
    options = list(
      dom = 't',
      fixedColumns = TRUE,
      #autoWidth = TRUE,
      columnDefs = list(list(className = 'dt-center', targets = "_all")),
      searching = FALSE
    ), 
    rownames = FALSE)
  return(res)
})

# Estimation of dispersion parameter k and R
##### beging bebug
base::try({
  # The withLogErrors call ensures that stack traces are captured
  # and that errors that bubble up are logged using warning().
  shiny::withLogErrors({
    # tryCatch and withVisible are just here to add some noise to
    # the stack trace.
    base::tryCatch(
      base::withVisible({
        # add function here whose output should be traced 
        kRet <- eventReactive(input$caseDataAnalysisAction, {
          temp =  infectorInfecteeDataDiseaseRegionDist() # infectorInfecteeDataDiseaseRegionDistSerialIntFilter()
          kRet = offspringDistPlot(infectorInfecteePair = temp, polyDegree = input$polyDegree_RtK_UI)
          return(kRet)
        }, ignoreNULL = FALSE) 
      })
    )
  })
})
##### end bebug
# plotting k
output$distribution_k_plot <- renderPlot({
  temp = kRet()
  plot(temp$offspringDistributionPlot)
})
# exporting estimate of k
output$k_estimate_table <- DT::renderDataTable({
  temp = kRet()
  res = DT::datatable(
    data =  temp$rkEstmate,
      options = list(
      dom = 't',
      fixedColumns = TRUE,
      #autoWidth = TRUE,
      columnDefs = list(list(className = 'dt-center', targets = "_all")),
      searching = FALSE
    ), 
    rownames = TRUE)
  return(res)
})
## end of case data analysis ##
    
##### EVENT DATA ANALYSIS  ##################

## EVENT dashboard and tables ----
# Begin of event filter
# filtering disdrict name from eventData based of users selected region and parse it back to the frontend user
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
  pickerInput(inputId = 'districtEventUi', label = 'District of event',
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
    
# fiter by district of event
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
# fitering event by event identification source
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
 req(credentials()$user_auth)
  selEventRegionDistIdentificationSourceUi()
})

# Adding control based on eventDataAnalysisAction icon on front ui
# Any output or computation that depend on eventDataDiseaseRegionTimeAuthFilter wouuld run only when input$eventDataAnalysisAction is clicked
eventDataDiseaseRegionTimeFilter = eventReactive(input$eventDataAnalysisAction, { 
  eventDataDiseaseRegionTimeAuthFilter() 
}, ignoreNULL = FALSE)
# End of event filter

# Begin of analysis
## event batplot
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
      ###
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
      ##
      fig = ggplotly(fig)
      return(fig)
    })
    
    # Event dashboard indicators 
    KPIValueBox_Server("event",data=eventDataDiseaseRegionTimeFilter, panel="event",boxfun=box)
    
    ## evnet status by type of place
    output$eventCuntbytyplaceTable <- DT::renderDataTable({
      if(input$EventIndicatorTypeUi == "Count"){
        temp = twoByTwoTablefunction(data = eventDataDiseaseRegionTimeFilter() , Var1 = "eventstatus", Var2 =  "typeofplace_event", spread = TRUE, Proportion = FALSE)
        res =  DT::datatable(
          temp,
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
        temp = twoByTwoTablefunction(data = eventDataDiseaseRegionTimeFilter() , Var1 = "eventstatus", Var2 = "typeofplace_event", spread = TRUE, Proportion = TRUE)
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
    
    # event status by eventmanagementstatus -----
    output$eventStatusByMangemantStatusTable <- DT::renderDataTable({
      if(input$EventIndicatorTypeUi == "Count"){
        temp = twoByTwoTablefunction(data = eventDataDiseaseRegionTimeFilter() , Var1 = "eventstatus", Var2 = "eventmanagementstatus", spread = TRUE, Proportion = FALSE)
        res = DT::datatable(
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
      if(input$EventIndicatorTypeUi == "Proportion"){
        temp = twoByTwoTablefunction(data = eventDataDiseaseRegionTimeFilter() , Var1 = "eventstatus", Var2 = "eventmanagementstatus", spread = TRUE, Proportion = TRUE)
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
    
    # event status by risklevel_event -----
    output$eventStatusByRisklevelTable <- DT::renderDataTable({
      if(input$EventIndicatorTypeUi == "Count"){
        temp = twoByTwoTablefunction(data = eventDataDiseaseRegionTimeFilter() , Var1 = "eventstatus", Var2 = "risklevel_event", spread = TRUE, Proportion = FALSE)
        res = DT::datatable(
          temp,
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
     
      # fiter by variables selected by user
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
    
#### dynamic pie chart based on user specified varaible -----
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
        dplyr::filter(event_variable %in% input$eventTableColumnVaribleUi ) # getting category of selected varaibles
    
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
        
      }

      return(res)
    })

### event map by administrative area ----
    output$eventMapUi <- renderPlot({
       req(credentials()$user_auth)
      map_text_size = input$eventMapTextSizeUi
      if(input$eventMapShapesUi == "Region")
      {
        fg = qtm(shp = regionShapesFrance, text = "libgeo", 
                 text.size = map_text_size, style = "white", format = "World") + tm_compass(type="8star", position=c("left", "top")) # fill.palette = "-Blues" to qhow in reverse order, type = "4star", "8star", "radar", "rose"
      }
      if(input$eventMapShapesUi == "Departement")
      {
        fg = qtm(shp = departementShapesFrance, text = "codgeo", 
                 text.size = map_text_size, style = "white", format = "World") + tm_compass(type="8star", position=c("left", "top")) 
      }
    
      if(input$eventMapShapesUi == "Commune") 
      {
        fg = qtm(shp = CommuneFrance, style = "white", format = "World") + tm_compass(type="8star", position=c("left", "bottom")) 
      }
      
      return(fg)
    })
  
    
  # event leafletmap
    # https://rstudio.github.io/leaflet/shiny.html 
    #https://github.com/atmajitg/bloodbanks  to get sample app
    # Reactive expression for the data subsetted to what the user selected
    filteredData <- reactive({
       req(credentials()$user_auth)
      eventData[eventData$n_ep >= input$range[1] & eventData$n_ep <= input$range[2],]
    })
    
    # This reactive expression represents the palette function,
    # which changes as the user makes selections in UI.
    colorpal <- reactive({
       req(credentials()$user_auth)
      colorNumeric(input$colors, eventData$n_ep)
    })
    
    output$map <- renderLeaflet({
       req(credentials()$user_auth)
      # Use leaflet() here, and only include aspects of the map that
      # won't need to change dynamically (at least, not unless the
      # entire map is being torn down and recreated).
      leaflet(eventData) %>% addTiles() %>%
        fitBounds(~min(long), ~min(lat), ~max(long), ~max(lat))

    })
    
    # Incremental changes to the map (in this case, replacing the
    # circles when a new color is chosen) should be performed in
    # an observer. Each independent set of things that can change
    # should be managed in its own observer.
    observe({
      pal <- colorpal()

      leafletProxy("map", data = filteredData()) %>%
        clearShapes() %>%
        addCircles(radius = ~10^n_ep/10, weight = 1, color = "#777777",
                   fillColor = ~pal(n_ep), fillOpacity = 0.7, popup = ~paste(n_ep)
        )
    })
    # Use a separate observer to recreate the legend as needed.
    observe({
      proxy <- leafletProxy("map", data = eventData)
      # Remove any existing legend, and only if the legend is
      # enabled, create a new one.
      proxy %>% clearControls()
      if (input$legend) {
        pal <- colorpal()
        proxy %>% addLegend(position = "bottomright",
                            pal = pal, values = ~n_ep
        )
      }
    })
    
# End of event analysis 
    
## SAMPLE DATA ANALYSIS -----
# Start of sample filter-----
# filtering district name from sample_table based of users selected region and parse it back to the frontend ----
output$pickerInputdistrictSampleUi <- renderUI({
  if(!is.null(input$regionSampleUi))
  {
    temp = sample_table %>%
      dplyr::filter(region_name %in% input$regionSampleUi)  %>%
      distinct(district_name) %>%
      .$district_name
  }else{
    temp = NULL
  }
  pickerInput(inputId = 'districtSampleUi', label = 'District of sample',
              choices = temp, 
              options = list(`actions-box` = TRUE, size = 12),
              selected = NULL,
              multiple = TRUE
  )
})
    
# filter sample_table by disease, region, and report date
selSampleRegionDiseaseTime = reactive({
  if(!is.null(input$regionSampleUi)){
    sample_table[((sample_table$region_name  %in% input$regionSampleUi) & (sample_table$disease_sample == input$diseaseSampleUi) & (sample_table$date_of_report >= (min(input$reportdateSampleUi) )  ) & (sample_table$date_of_report <= (max(input$reportdateSampleUi) ))),]
  } else{
    sample_table[((sample_table$disease_sample == input$diseaseSampleUi) & (sample_table$date_of_report >= (min(input$reportdateSampleUi))) & (sample_table$date_of_report <= (max(input$reportdateSampleUi)) )), ]
  }
})    
# add more filters here, one reactive function per filter condition
# fitter by district of sample
selSampleRegionDiseaseTimeDist = reactive({
  if(is.null(input$districtSampleUi))
  {temp = selSampleRegionDiseaseTime() } else{
    temp = selSampleRegionDiseaseTime() %>%
      dplyr::filter(district_name %in% input$districtSampleUi)
  }
  return(temp)
})

## add more filters here if needed, use one varible per filter
# filter 1
# filter 2, etc
# checking is user is authenticated before creating ui elements
sample_table_filtered = reactive({
  req(credentials()$user_auth)
  selSampleRegionDiseaseTimeDist()
}) 
# Adding control based on "apply changes" icon  ---
# Any output or computation that depends on sample_table_selected would run only when input$sampleDataAnalysisAction is clicked
sample_table_selected = eventReactive(input$sampleDataAnalysisAction,{ 
  sample_table_filtered()
}, ignoreNULL = FALSE) 

# End of sample filter 
# Start of sample analysis -----
#extracting user selected variable to plot bar chart 
sample_barVar = eventReactive(input$sampleDataAnalysisAction,{
  temp_data = sample_table_selected()
  if(input$bargraphSampleVariableUi != "samplematerial"){
  ret = as.character(temp_data[, colnames(temp_data) == input$bargraphSampleVariableUi])
  }else{
  ret=temp_data$samplematerial
  }
  },ignoreNULL = FALSE)
# plotting bar chart
output$barChartSampleUi <- renderPlotly({
  if(input$sampleIndicatorTypeUi == "Count"){ 
  fg = univariate_barplot(var = sample_barVar(), count=TRUE, x_verticalLayout = TRUE )
  }
  if(input$sampleIndicatorTypeUi == "Proportion"){ 
  fg = univariate_barplot(var = sample_barVar(), count=FALSE, x_verticalLayout = TRUE)
  }
  return(fg)
})

output$samples_table<-renderDT({
  sample_table_selected()
})
KPIValueBox_Server("samples",data = sample_table_selected,panel="samples")
# plotting pie chart
output$pieCdhartSampleUi <- renderPlotly({
fg = pieChartPlot(variable = sample_barVar())
})
# End of sample analysis -----
# UI output for "sample data analysis" tab ----
# The output object "sample_analysis_output" should be placed below the computation of elements needed in it
output$sample_analysis_output <- renderUI({
panels <- list(
tabPanel("Sample dashboard",
  tagList(                                                  
   KPIValueBox_UI("samples")
  )
 )
)
# check for configuration and add more panels 
if(sample_custom_indicators=="t"){
panels[[2]]=tabPanel("Custom indicators",
fluidRow(width=10,                                                         
column(6,                                                                
 wellPanel(
   h4(helpText("Dynamic pie chart")) ,
   div(plotlyOutput("pieCdhartSampleUi", width = "100%", height = "50vh" ), style = "font-size: 100%; width: 100%" ) 
 ) ),
column(6,                    
 wellPanel(
   h4(helpText("Dynamic bar graph")) ,
   div(plotlyOutput("barChartSampleUi", width = "100%", height = "50vh" ), style = "font-size: 100%; width: 100%" ) 
 )  )
) ## end of fluid row
)  }
if(sample_custom_indicators=="t"){ #change here to  sample customregion table
  panels[[3]]=tabPanel("Samples by region", value = 6,
         fluidRow(
           column(12, DT::dataTableOutput("samples_table"))    
         )
)
}
base::do.call(tabsetPanel, panels) 
})     

# Sample custom indicator tab filters should be added here -----
# This filters would show on ui only if "custom indicator tab" is activated
output$sample_cuctom_indicator_filter = renderUI({
if(sample_custom_indicators=="t"){
  pickerInput(
    inputId = "bargraphSampleVariableUi",
    label = 'Choose custom indicator variable',
    choices=sort(c("pathogentestresult","samplingreason","samplepurpose","shipped","received",
                               "specimencondition","samplesource","samplematerial")), 
    options = list(`actions-box` = TRUE, size = 12),
    selected = NULL,  multiple = FALSE)
} })
################## end of sample data analysis ###########################      
# Model specification----
    output$model_specificationUI <- renderUI(
      includeMarkdown(paste0("Documentation/model_specification_", input$language,".md"))
    )
} 
)
 









