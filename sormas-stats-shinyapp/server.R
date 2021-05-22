

#####  server function #######
shinyServer(
  function(input, output,session) {
   
     ### toggling sidebar panel
    observeEvent(input$toggleSidebar, {
      shinyjs::toggle(id = "Sidebar")
    })
    
    # ui element to filter trensmisson chenin by disdrict based of users delected region 
    output$pickerInputDistrict2 <- renderUI({
      # choicesRegionUI = sort(unique(contRegionDist$district_name[contRegionDist$region_name %in% input$regionUi]  ))
      if(!is.null(input$regionNetworkUi))
      {
        # choicesRegionNetworkUI = contRegionDist %>%
        #   dplyr::filter(region_name %in% input$regionNetworkUi)  %>%
        #   mutate(district_name )
        
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
    
  ###### Trasmission chain ######################################

  ########################## Nework option 2 code #############"""
# plotting network -----
  ## Filter elist by region, disease, and time time, etc
  selElistRegionUI = reactive({
    if(!is.null(input$regionNetworkUi))
    {
      #as.numeric(elist[((elist$disease == input$diseaseUi) & (elist$reportdatetime >= (min(input$reportdateUi))) & (elist$reportdatetime <= (max(input$reportdateUi)) )), colnames(elist) == "id" ])
      temp =  elist[((elist$region_name %in% input$regionNetworkUi) & (elist$disease == input$diseaseUi) & (elist$reportdatetime >= (min(input$reportdateUi) )  ) & (elist$reportdatetime <= (max(input$reportdateUi) ))),  colnames(elist)  %in% c("id","entityType", "district_name", "relationtocase","eventstatus", "risklevelEvent", "from_uuid_person", "to_uuid_person")]
    } else{
    temp = elist[((elist$disease == input$diseaseUi) & (elist$reportdatetime >= (min(input$reportdateUi))) & (elist$reportdatetime <= (max(input$reportdateUi)) )), colnames(elist) %in% c("id","entityType", "district_name", "relationtocase", "eventstatus", "risklevelEvent", "from_uuid_person", "to_uuid_person") ]
    }
    return(temp)
  })
  #
  selElistRegionDistUI = reactive({
    #temp = selElistRegionUI()
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
        dplyr::filter(risklevelEvent %in% input$risklevelUI)
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
      temp = temp[((temp$from_uuid_person == node_uuidUi) | (temp$to_uuid_person == node_uuidUi)),]
    } 
    return(temp)
    })

  # further filtering based on resultingCaseOnlyUi, activeEventsOnlyUi, visSingleChainUi
  elistSel2ResCaseSourseCase  = reactive({ 
  elistSel <- elist[elist$id %in% selElistRegionEntityTypeSettingEventstatusRisklevelUISingleNode()$id, ]
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
  
  if(input$visSingleChainUi != ""){
    contIdTemp = contIdsForSingleChain(elist =  elistSel2ResCaseArchived, uuid_node = stri_replace_all_fixed(toupper(as.character(input$visSingleChainUi)), " ", "") ) # retain list of contact ids
    ret = elistSel2ResCaseArchived[elistSel2ResCaseArchived$id %in% contIdTemp,]
  } else{
    ret <- elistSel2ResCaseArchived
  }
  return(ret)
  }) 
  
  # pltting network using selElist
  output$transChain <- renderVisNetwork({
    elistSel =  elistSel2ResCaseSourseCase()
    nodeLineListSelResCase <- nodeLineList[nodeLineList$id %in% unique(c(elistSel$from, elistSel$to)), ]
    plotNet(nodeLineList= nodeLineListSelResCase, elist = elistSel)
  })

  ## computation of network parameters esing transmission network data ----
  ## total number of contacts
  output$totalEdges <- renderInfoBox({
    infoBox(
      "∑ Contact", nrow(elistSel2ResCaseSourseCase() ),icon = icon("arrow-right"), 
      color = "black", fill = FALSE, subtitle = "Contact & Ep"
      )
  })  
  ## Total number of edges (contacts or event participants) resulting to cases
  output$totalReultingcasesEdges <- renderInfoBox({
    temp = elistSel2ResCaseSourseCase() %>%
      dplyr::filter(resultingcase_id != "NA")
    infoBox(
      title = "∑ Converted", value = nrow(temp),
      icon = icon("arrow-right"), color = colCase, fill = FALSE,
      subtitle = "Contact & Ep"
      
    )
  })   
  
  ## total number of nodes: ie person and events
  output$totalNodes <- renderInfoBox({
    infoBox(
      "∑ Person & Event", length(unique(c(elistSel2ResCaseSourseCase()$from_uuid_person, elistSel2ResCaseSourseCase()$to_uuid_person ))),
      #icon = icon("users")
      icon = icon("user-cog")
      , color = "green", fill = FALSE, subtitle = "Node sum"
      )
  }) 
  ## total number of event nodes
  output$totalEventNodes <- renderInfoBox({
   temp =  as.numeric(
     elistSel2ResCaseSourseCase() %>%
       dplyr::select(from_uuid_person, entityType ) %>%
       dplyr::filter(entityType == "event") %>%
       dplyr::distinct_at(. , vars(from_uuid_person))  %>%
       dplyr::summarise(n = n())
   )
    infoBox(
      title ="∑ Event", value = temp,
      icon = icon("cog"), color = colEvent, fill = FALSE,
      subtitle = "Sum"
      )
  }) 
  
  ## total number of person nodes
  output$totalPersonNodes <- renderInfoBox({
    id_vec = compute_person_node_uuid(elist = elistSel2ResCaseSourseCase())
    infoBox(
      title ="∑ Persons", value = length(id_vec),
      icon = icon("user"), color = colPerson, fill = FALSE,
      subtitle = "Case & Contact & Ep"
      )
  }) 
  ## Total number of nodes (persons) resulting to cases
  output$totalReultingcasesNodes <- renderInfoBox({
    temp = elistSel2ResCaseSourseCase() %>%
      dplyr::filter(resultingcase_id != "NA") %>%
      dplyr::distinct_at(., vars(to_uuid_person), .keep_all = TRUE) 
    infoBox(
      title = "∑ Converted", value = nrow(temp),
      icon = icon("user"), color = colCase, fill = FALSE,
      subtitle = "Contact & Ep Person"
      
    )
  })  
  ###
  
  ## Total number of ep and contact persons 
  output$totalContactEpPersonNodes <- renderInfoBox({
    temp = elistSel2ResCaseSourseCase()
    temp = as.numeric(prop_cont_ep_person_convertedToCase(elist = temp)$n_contact_ep_nodes)
    infoBox(
      title = "∑ Person", value = temp,
      icon = icon("user"), color = colCont, fill = FALSE,
      subtitle = "Contact & Ep"
      
    )
  })  

  ## Prpportion of ep and contact person converted to case
  output$propContactEpPersonConverted <- renderInfoBox({
    temp = elistSel2ResCaseSourseCase()
    temp = as.numeric(prop_cont_ep_person_convertedToCase(elist = temp)$prop_converted)
    infoBox(
      title = "% Converted", value = temp ,
      icon = icon("user"), color = colCase, fill = FALSE,
      subtitle = "Contact & Ep Person"
      
    )
  }) 
  
### Computing network parameters using igraph----
  graphObject <- reactive({
    elistSel = elistSel2ResCaseSourseCase()
    elistSel = elistSel %>%
      dplyr::distinct_at(., vars(from, to), .keep_all = FALSE)
    
    nodeLineListSelResCase = nodeLineList %>%
      dplyr::select(id, group) 
    nodeLineListSelResCase <- nodeLineListSelResCase[nodeLineListSelResCase$id %in% unique(c(elistSel$from, elistSel$to)), ]
    
    net <- graph_from_data_frame(d=elistSel, vertices = nodeLineListSelResCase, directed=T)
    return(net)
  })
 
  # computing summary of node degree 
  output$nodeDegreeSummary <- renderPrint({
    net = graphObject()
    deg <- degree(net, mode="all") # 
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
      title = "Edge density", value = edge_density ,
      icon = icon("arrow-right"), color = colEdge, fill = FALSE,
      subtitle = "Contact & Ep"
      
    )
  }) 
  
  # Diameter: longest geodesic distance ie longest undirected chain, thus extract the source case
  output$diameterDirected <- renderInfoBox({
    net = graphObject()
    temp = diameter(net, directed=TRUE) # # number of contact in longest chain of infector-infectee pairs
    infoBox(
      title = "Longest chain", value = temp,
      icon = icon("arrow-right"), color = colEdge, fill = FALSE,
      subtitle = "Directed edge"
      
    )
  }) 
  output$diameterUndirected <- renderInfoBox({
    net = graphObject()
    temp = diameter(net, directed=FALSE) # # number of contact in longest undirected chain
    infoBox(
      title = "Longest chain", value = temp,
      icon = icon("arrows-alt-h"), color = colEdge, fill = FALSE,
      subtitle = "Undirected edge"
      
    )
  }) 
  ## summary of source/ infector nodes for cases and evnet
  output$totInfectorCaseEventNodes <- renderInfoBox({
    temp = prop_missing_source_case_nodes(elist= elistSel2ResCaseSourseCase(), nodeLineList = nodeLineList)$sum_caseEvent_nodes
    infoBox(
      title = "∑ Infector", value = temp,
      icon = icon("user-cog"), color = colCase, fill = FALSE,
      subtitle = "Case & Event nodes"
    )
  }) 
  #
  output$totSourceInfectorCaseEventNodes <- renderInfoBox({
    temp = prop_missing_source_case_nodes(elist= elistSel2ResCaseSourseCase(), nodeLineList = nodeLineList)$sum_missing_source_case_nodes
    infoBox(
      title = "∑ Source Infectors", value = temp,
      icon = icon("user-cog"), color = colCase, fill = FALSE,
      subtitle = "Case & Event nodes"
    )
  }) 
  ##
  output$propInfectorCaseEventNodes <- renderInfoBox({
    temp = prop_missing_source_case_nodes(elist=elistSel2ResCaseSourseCase(), nodeLineList = nodeLineList)$prop_missing_source_case_nodes
    infoBox(
      title = "% Source Infector", value = temp,
      icon = icon("user-cog"), color = colCase, fill = FALSE,
      subtitle = "Case & Event nodes"
    )
  }) 
  
  # summary pronting uuid of app parant nodes
  output$sourceNodeUUID <- renderPrint({
    temp = prop_missing_source_case_nodes(elist=elistSel2ResCaseSourseCase(), nodeLineList = nodeLineList)$source_node_uuid
    cat(temp)
  })
  
  ##
  output$transitivityScore <- renderInfoBox({
    net = graphObject()
    temp = round(transitivity(net), 2)
    infoBox(
      title = "Transiticity Score", value = temp,
      icon = icon("project-diagram"), color = colPerson, fill = FALSE,
      subtitle = "Clustering coefficient"
    )
  }) 
  
  # computing case counts by case classification from graphObject 
  output$nodeClassificationCountTable <- DT::renderDataTable({
   
    net = graphObject()
    temp = as.data.frame(table(V(net)$group))
    temp = temp %>% rename("Node classification" = Var1, Total = Freq)
      res =  DT::datatable(
        temp,
        # extensions = c('Buttons', 'Scroller'),  # to hide top icon to download table
        options = list(
          # scrollY = 200,
          #              scrollX = 500,
          #              deferRender = TRUE,
          #              scroller = TRUE,
          #              # paging = TRUE,
                       # pageLength = 25,
                      buttons = list(c('excel','csv'),
                                     list(extend = 'colvis', targets = 0, visible = FALSE)),
                      dom = 'lBfrtip',
                       fixedColumns = TRUE,
                       autoWidth = TRUE,
                       columnDefs = list(list(className = 'dt-center', targets = "_all")),
                       searching = FALSE
        ), 
        rownames = FALSE)
      
   # }
    return(res)
  })
  
  
  
  output$networkParameter2Table <- DT::renderDataTable({
  
    temp =  tail(cars, 6) 
    #if(input$EventIndicatorTypeUi == "Count"){
      res =  DT::datatable(
        temp,
       # filter = 'top', 
       extensions = c('Buttons', 'Scroller'),
        options = list(
          # scrollY = 200,
          #              scrollX = 500,
          #              deferRender = TRUE,
          #              scroller = TRUE,
          #              # paging = TRUE,
          # pageLength = 25,
          buttons = list(c('excel','csv'),
                         list(extend = 'colvis', targets = 0, visible = FALSE)),
          dom = 'lBfrtip',
          fixedColumns = TRUE,
          autoWidth = TRUE,
          columnDefs = list(list(className = 'dt-center', targets = "_all")),
          searching = FALSE
        ), 
        rownames = FALSE)
      
   # }
    return(res)
  })
## end of transmission network analysis

## CONTACT DATA ANALYSIS-----
    #d render contact table based on region district time and disease
    d = reactive({
      if(is.null(input$regionContactUi))
      {
        contRegionDist[((contRegionDist$disease == input$diseaseContactUi) & (contRegionDist$reportdatetime >= (min(input$reportdateContactUi))) & (contRegionDist$reportdatetime <= (max(input$reportdateContactUi)) )), ]
      } else{
        contRegionDist[((contRegionDist$region_name %in% input$regionContactUi) & (contRegionDist$disease == input$diseaseContactUi) & (contRegionDist$reportdatetime >= (min(input$reportdateContactUi) )  ) & (contRegionDist$reportdatetime <= (max(input$reportdateContactUi) ))),]
      }

    })
    
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
    #Row 1,  All contacts, used d
    output$allCont <- renderInfoBox({
      infoBox(
        "", nrow(d( )), icon = icon("handshake"),
        color = colCont, fill = FALSE , subtitle = "All contacts"   )
    })

    # confrimed contacts
    output$contConfirmed = renderInfoBox({
      temp = d()
      temp = temp[temp$contactclassification == "CONFIRMED" ,]
      infoBox("", nrow(temp), icon = icon("handshake"), color = colCont, fill = FALSE, subtitle = "Confirmed"
      )
    })
    # Uncofirmed contacts
    output$contUnconfirmed = renderInfoBox({
      temp = d()[d()$contactclassification == "UNCONFIRMED" ,]
      infoBox("", nrow(temp), icon = icon("handshake"), color = colCont, fill = FALSE, subtitle = "Unconfirmed"
      )
    })
    # Not a contact contacts
    output$contNot = renderInfoBox({
      temp =  d()[d()$contactclassification == "NO_CONTACT" ,]
      infoBox("", nrow(temp), icon = icon("handshake"), color = colCont, fill = FALSE, subtitle = "Discarded"
      )
    })

    #### Row 2  contact status ####
    ## Active contacts
    output$activeCont = renderInfoBox({
      temp = d()[d()$contactstatus == "ACTIVE" ,]
      infoBox("", nrow(temp), icon = icon("handshake"), color = colCont, fill = FALSE, subtitle = "Active"
      )
      })
    ## converted to case
    output$convertedToCase = renderInfoBox({
      temp =  d()[d()$contactstatus == "CONVERTED" ,]
      infoBox( "",nrow(temp), icon = icon("handshake"), color = colCont,
              fill = FALSE,  subtitle = "Converted to case"
      )

    })
    ## dropped contacts
    output$dropped = renderInfoBox({
     temp =   d()[d()$contactstatus == "DROPPED" ,]
     infoBox("", nrow(temp), icon = icon("handshake"), color = colCont, fill = FALSE, subtitle = "Dropped"
     )
    })
    ## inactive
    output$inactiveCont = renderInfoBox({
      temp =  d()[d()$contactstatus == "INACTIVE",]
      infoBox("", nrow(temp), icon = icon("handshake"), color = colCont, fill = FALSE, subtitle = "Inactive"
      )
    })

    

    ### Row 4, summary of number of contacts per case
    p = reactive({
      data.frame(as.table(summary(as.factor(d()$caze_id), maxsum = 5000000)))
    })
    # Use p()$Freq
    ## min contact per case
    output$minConPerCase = renderInfoBox({
      infoBox("",   min(p()$Freq) , icon = icon("handshake"), color = colCont,
              fill = FALSE, subtitle = "Min cont-per-case"
      )
    })

    ## median contacte per case
    output$medianConPerCase = renderInfoBox({
      infoBox("", median(p()$Freq) , icon = icon("handshake"), color = colCont, fill = FALSE, subtitle = "Med cont-per-case"
      )
    })
   ## mean contact per case
    output$meanConPerCase = renderInfoBox({
      infoBox("",  round(mean(p()$Freq), digits = 2 ) , icon = icon("handshake"), color = colCont, fill = FALSE, subtitle = "Mean cont-per-case"
      )
    })
    ## max contacte per case
    output$maxConPerCase = renderInfoBox({

      infoBox("",  max(p()$Freq)   , icon = icon("handshake"), color = colCont, fill = FALSE, subtitle = "Max cont-per-case"
      )
    })

##########  end of KPI #################""
  
      ### Contacts per case ####
    minLim = reactive({
      c( min(p()$Freq), max(p()$Freq)+20)
    })

    output$plotContPerCase <- renderPlot({
      if(nrow(d( ) ) == 0)
      {
        plot(NULL, xlim=c(0,1), ylim=c(0,1), ylab="Number of contacts", xlab=" ",
             main = "No data exist based on your selection, please choose another selection for which data exist")
      }else{
        #summary(p$Freq)
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
      }
    )

    
    ## serial interval
    #selecting siDat baed on disease, time; regiion and district just as in d
    siD = reactive({
      if(is.null(input$regionContactUi) )
      {
        siDat[((siDat$disease == input$diseaseContactUi) & (siDat$reportdatetime >= (min(input$reportdateContactUi))) & (siDat$reportdatetime <= (max(input$reportdateContactUi)) )), ]
      } else{
        siDat[((siDat$region_name %in% input$regionContactUi) & (siDat$disease == input$diseaseContactUi) & (siDat$reportdatetime >= (min(input$reportdateContactUi) )  ) & (siDat$reportdatetime <= (max(input$reportdateContactUi) ))),]
      }
    })
  
    # Begin exportation of  data
    # cotactRegionDist export
    conRegionDistExp = reactive({
      data.frame(contactReportDate =as.character( d()$reportdatetime),  caseID= d()$caze_id, contactID =  d()$id, contactProximity = d()$contactproximity, contactClassification = d()$contactclassification,
                 caseClassification = d()$caseclassificationCase, disease = d()$disease, caseOutcome = d()$outcomeCase, contactRegion = d()$region_name,
                 contactDistrict = d()$district_name)
    })
    # output to download data
    output$conRegionDistExpCsv <- downloadHandler(
      filename = function() {
        paste("sormas_export_", Sys.Date(), ".csv", sep="")
      },
      content = function(file) {
        write.csv(conRegionDistExp(), file)
      }
    )
    output$conRegionDistExpTable <- renderPrint({
      orig <- options(width = 1000)
      # print(tail(cv_cases %>% select(c(country, date, cases, new_cases, deaths, new_deaths,
      #                                  recovered, new_recovered, active_cases,
      #                                  per100k, newper100k, activeper100k, deathsper100k, newdeathsper100k)), input$maxrows), row.names = FALSE)
      print(head(conRegionDistExp(), input$maxrows), row.names = FALSE)
      options(orig)
    })


    ### Number of contacts per case exprt
    # use p()
    conPerCaseExp = reactive({
      p() %>% dplyr::rename(Case_id = Var1, Nunber_of_contacts = Freq )
    })

    output$conPerCaseExpCsv <- downloadHandler(
      filename = function() {
        paste("sormas_contactPerCaseExp_", Sys.Date(), ".csv", sep="")
      },
      content = function(file) {
        write.csv(conPerCaseExp(), file)
      }
    )
    output$conPerCaseExpTable <- renderPrint({
      orig <- options(width = 1000)
      print(head(conPerCaseExp(), input$maxrows), row.names = FALSE)
      options(orig)
    })

    ## Serail interval
    # use siD()
    siExp = reactive({
      #p() %>% rename(Case_id = Var1, Nunber_of_contacts = Freq )
      siD() %>% dplyr::rename(Contact_report_date = reportdatetime, Disease = disease, Contact_region = region_name, Contact_district = district_name, Serial_interval = si)
    })

    output$siExpCsv <- downloadHandler(
      filename = function() {
        paste("sormas_serialIntervalExp_", Sys.Date(), ".csv", sep="")
      },
      content = function(file) {
        write.csv(siExp(), file)
      }
    )
    output$siExpTable <- renderPrint({
      orig <- options(width = 1000)
      print(head(siExp(), input$maxrows), row.names = FALSE)
      options(orig)
    })

    # use d()
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
      print(head(conPerGerionExp(), input$maxrows), row.names = FALSE)
      options(orig)
    })

  ## end exportation of data

    
  #### CASE DATA ANALYSIS  #################
    # ui element to filter casePersonRegionDist by disdrict based of users selected region 
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
    casePersonFilter = reactive({
      if(is.null(input$districtCaseUi))
      {
        temp = casePersonRegionFilter()
      } else{
        temp = casePersonRegionFilter() %>%
          dplyr::filter(district_name %in% input$districtCaseUi)
      }
      return(temp)
      
    })
      
    
    #### case KPI ################
    # case classification
    ## all cases
    output$allCase <- renderInfoBox({
      infoBox(
        "All cases", nrow(casePersonFilter() ), icon = icon("procedures"),  color = colCase, fill = FALSE    )
    })
    # unclassified cases
    output$UnclassifiedCases <- renderInfoBox({
      temp = casePersonFilter()
      temp = temp[temp$caseclassification == "NOT_CLASSIFIED" ,]
      infoBox(
        "Unclassified", nrow(temp), icon = icon("procedures"),  color = colCase, fill = FALSE )
    })
    # suspected cases
    output$suspectedCases <- renderInfoBox({
      temp = casePersonFilter()
      temp = temp[temp$caseclassification == "SUSPECT",]
      infoBox(
        "Suspect", nrow(temp), icon = icon("procedures"),  color = colCase, fill = FALSE )
    })
    # Probable cases
    output$probableCases <- renderInfoBox({
      temp = casePersonFilter()
      temp = temp[temp$caseclassification == "PROBABLE",]
      infoBox(
        "Probable", nrow(temp), icon = icon("procedures"),  color = colCase, fill = FALSE )
    })
    # Confirmed cases
    output$confirmedCases <- renderInfoBox({
      temp = casePersonFilter()
      temp = temp[temp$caseclassification %in% c("CONFIRMED", "CONFIRMED_NO_SYMPTOMS", "CONFIRMED_UNKNOWN_SYMPTOMS"),] # combination of all confirmed cases
      infoBox(
        "Confirmed", nrow(temp), icon = icon("procedures"),  color = colCase, fill = FALSE )
    })

    # case outcome
    # diseases cases
    output$diseasedCases <- renderInfoBox({
      temp = casePersonFilter()
      temp = temp[temp$outcome == "DECEASED",]
      infoBox(
        "Diseased", nrow(temp), icon = icon("procedures"),  color = colCase, fill = FALSE )
    })
    # recovered cases
    output$recoveredCases <- renderInfoBox({
      temp = casePersonFilter()
      temp = temp[temp$outcome == "RECOVERED",]
      infoBox(
        "Recovered", nrow(temp), icon = icon("procedures"),  color = colCase, fill = FALSE )
    })
    # unknownoutcome cases
    output$unknownOutcomeCases <- renderInfoBox({
      temp = casePersonFilter()
      temp = temp[temp$outcome == "UNKNOWN",]
      infoBox(
        "Unknown", nrow(temp), icon = icon("procedures"),  color = colCase, fill = FALSE )
    })
    # unknownoutcome cases
    output$noOutcomeCases <- renderInfoBox({
      temp = casePersonFilter()
      temp = temp[temp$outcome == "NO_OUTCOME",]
      infoBox(
        "No Outcome", nrow(temp), icon = icon("procedures"),  color = colCase, fill = FALSE )
    })

    ## case origin
    # incountry cases
    output$incountryCases <- renderInfoBox({
      temp = casePersonFilter()
      temp = temp[temp$caseorigin == "IN_COUNTRY",]
      infoBox(
        "In-country", nrow(temp), icon = icon("procedures"),  color = colCase, fill = FALSE )
    })
    # POE/ imported cases
    output$importedCases <- renderInfoBox({
      temp = casePersonFilter()
      temp = temp[temp$caseorigin == "POINT_OF_ENTRY",]
      infoBox(
        "Imported", nrow(temp), icon = icon("procedures"),  color = colCase, fill = FALSE )
    })

    ## case quarantine #####
    # institutional
    output$institutionalQCases <- renderInfoBox({
      temp = casePersonFilter()
      temp = temp[is.na(temp$quarantine) != T,]  # deletig NA before filtering
      temp = temp[temp$quarantine == "INSTITUTIONELL",]
      infoBox(
        "Institutional", nrow(temp), icon = icon("procedures"),  color = colCase, fill = FALSE )
    })
    # home
    output$homeQCases <- renderInfoBox({
      temp = casePersonFilter()
      temp = temp[is.na(temp$quarantine) != T,]  # deletig NA before filtering
      temp = temp[temp$quarantine == "HOME",]
      infoBox(
        "Home", nrow(temp), icon = icon("procedures"),  color = colCase, fill = FALSE )
    })
    # other
    output$otherQCases <- renderInfoBox({
      temp = casePersonFilter()
      temp = temp[is.na(temp$quarantine) != T,]  # deletig NA before filtering
      temp = temp[temp$quarantine == "OTHER",]
      infoBox(
        "Other", nrow(temp), icon = icon("procedures"),  color = colCase, fill = FALSE )
    })
    # None
    output$noneQCases <- renderInfoBox({
      temp = casePersonFilter()
      temp = temp[is.na(temp$quarantine) != T,]  # deletig NA before filtering
      temp = temp[temp$quarantine == "NONE",]
      infoBox(
        "None", nrow(temp), icon = icon("procedures"),  color = colCase, fill = FALSE )
    })
    # unknown
    output$unknownQCases <- renderInfoBox({
      temp = casePersonFilter()
      temp = temp[is.na(temp$quarantine) != T,]  # deletig NA before filtering
      temp = temp[temp$quarantine == "UNKNOWN",]
      infoBox(
        "Unknown", nrow(temp), icon = icon("procedures"),  color = colCase, fill = FALSE )
    })
    # Missing
    output$MissingQCases <- renderInfoBox({
      temp = casePersonFilter()
      temp = temp[is.na(temp$quarantine) == T,]
      infoBox(
        "Missing", nrow(temp), icon = icon("procedures"),  color = colCase, fill = FALSE )
    })


    ## case sex ####
    # male
    output$maleSexCases <- renderInfoBox({
      temp = casePersonFilter()
      temp = temp[is.na(temp$sex) != T,]  # deletig NA before filtering
      temp = temp[temp$sex == "MALE",]
      infoBox(
        "Male", nrow(temp), icon = icon("procedures"),  color = colCase, fill = FALSE )
    })
    #female
    output$femaleSexCases <- renderInfoBox({
      temp = casePersonFilter()
      temp = temp[is.na(temp$sex) != T,]  # deletig NA before filtering
      temp = temp[temp$sex == "FEMALE",]
      infoBox(
        "Female", nrow(temp), icon = icon("procedures"),  color = colCase, fill = FALSE )
    })
    #other
    output$otherSexCases <- renderInfoBox({
      temp = casePersonFilter()
      temp = temp[is.na(temp$sex) != T,]  # deletig NA before filtering
      temp = temp[temp$sex == "OTHER",]
      infoBox(
        "Other", nrow(temp), icon = icon("procedures"),  color = colCase, fill = FALSE )
    })
    #unknown
    output$unknownSexCases <- renderInfoBox({
      temp = casePersonFilter()
      temp = temp[is.na(temp$sex) != T,]  # deletig NA before filtering
      temp = temp[temp$sex == "UNKNOWN",]
      infoBox(
        "Unknown", nrow(temp), icon = icon("procedures"),  color = colCase, fill = FALSE )
    })
    #missing
    output$MissingSexCases <- renderInfoBox({
      temp = casePersonFilter()
      temp = temp[is.na(temp$sex) == T,]
      infoBox(
        "Missing", nrow(temp), icon = icon("procedures"),  color = colCase, fill = FALSE )
    })


    ## case occupation ####
    # health care worker
    output$healthWorkerOcupCases <- renderInfoBox({
      temp = casePersonFilter()
      temp = temp[is.na(temp$occupationtype) != T,]  # deletig NA before filtering
      temp = temp[temp$occupationtype == "HEALTHCARE_WORKER",]
      infoBox(
        "Health worker", nrow(temp), icon = icon("procedures"),  color = colCase, fill = FALSE )
    })
    # other occupations
    output$otherOcupCases <- renderInfoBox({
      temp = casePersonFilter()
      temp = temp[is.na(temp$occupationtype) != T,]  # deletig NA before filtering
      temp = temp[temp$occupationtype != "HEALTHCARE_WORKER",] #counting all occupations except health workers
      infoBox(
        "Other", nrow(temp), icon = icon("procedures"),  color = colCase, fill = FALSE )
    })
    # missing occupations
    output$MissingOcupCases <- renderInfoBox({
      temp = casePersonFilter()
      temp = temp[is.na(temp$occupationtype) == T,]
      infoBox(
        "Missing", nrow(temp), icon = icon("procedures"),  color = colCase, fill = FALSE )
    })


     ## case age ####
    # min age
    output$minAgeCases <- renderInfoBox({
      temp = casePersonFilter()
      temp = temp[is.na(temp$age) != T,]  # deletig NA before filtering
      infoBox(
        "Minimum", min(temp$age), icon = icon("procedures"),  color = colCase, fill = FALSE )
    })
    # median age
    output$medianAgeCases <- renderInfoBox({
      temp = casePersonFilter()
      temp = temp[is.na(temp$age) != T,]  # deletig NA before filtering
      infoBox(
        "Median", median(temp$age), icon = icon("procedures"),  color = colCase, fill = FALSE )
    })
    # mean age
    output$meanAgeCases <- renderInfoBox({
      temp = casePersonFilter()
      temp = temp[is.na(temp$age) != T,]  # deletig NA before filtering
      infoBox(
        "Mean", round(mean(temp$age), digits = 2), icon = icon("procedures"),  color = colCase, fill = FALSE )
    })
    # max age
    output$maxAgeCases <- renderInfoBox({
      temp = casePersonFilter()
      temp = temp[is.na(temp$age) != T,]  # deletig NA before filtering
      infoBox(
        "Maximum", max(temp$age), icon = icon("procedures"),  color = colCase, fill = FALSE )
    })
    # missing age
    output$missingAgeCases <- renderInfoBox({
      temp = casePersonFilter()
      temp = temp[is.na(temp$age) == T,]
      infoBox(
        "Missing", nrow(temp), icon = icon("procedures"),  color = colCase, fill = FALSE )
    })


    ############### Case tables  ###########
    # casePersonRegionDist export
    casebyRegionVar = reactive({
      casePersonFilter()
    })
    # output to download data
    output$caseRegionVarExpCsv <- downloadHandler(
      filename = function() {
        paste("sormas_export_case_region_", Sys.Date(), ".csv", sep="")
      },
      content = function(file) {
        write.csv(casePersonFilter(), file)
      }
    )
    output$casebyRegionVarTable <- renderPrint({
      orig <- options(width = 1000)
      print(head(casebyRegionVar(), input$maxrowsCase), row.names = FALSE)
      options(orig)
    })
    
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
      print(
        ggplotly(
          pyramidPlotFunction(data = temp)
        ))
    })

   #### Plotting time series plot for cases ###
    output$caseTimeSeriesPlot <- renderPlotly({
      temp = casePersonFilter()

      if (input$timeUnitUi == "Day")
      {
        if(input$byRegiontimeUnitUi == F)
        {
          dateSumCase = aggregate(total ~ reportdate, data = temp, sum, na.rm = F)
          fg=  timeSeriesPlotDay(data = dateSumCase, cum = input$cumUi )

        } else{
          dateSumCaseRegion = aggregate(total ~ reportdate + region_name, data = temp, sum, na.rm = F)
          #fg = timeSeriesPlotDayRegion(data = dateSumCaseRegion)
          fg = timeSeriesPlotDayRegion(data = dateSumCaseRegion, cum = input$cumRegionUi)

        }

      }
      if (input$timeUnitUi == "Epi-week")
      {
        weekSumCase = aggregate(total ~ reportweek+ reportyear, data = temp, sum, na.rm = F)
        fg=   timeSeriesPlotWeek(data = weekSumCase )
      }
      if (input$timeUnitUi == "Month")
      {
        monthSumCase = aggregate(total ~ reportmonth+ reportyear, data = temp, sum, na.rm = F)
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
        dateSumCaseClass = aggregate(total ~ reportdate + caseclassification, data = temp, sum, na.rm = F)
        fg=  epicurveDate(data = dateSumCaseClass)
      }
      if (input$timeUnitEpicurveUi == "Epi-week")
      {
        dateSumCaseClass = aggregate(total ~ reportdate + caseclassification, data = temp, sum, na.rm = F)
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
    # using casePersonFilter() and siDat 
     
    siDatRtDiseaseRegionFilter = reactive({
      if(is.null(input$regionCaseUi) )
      {
        siDat[((siDat$disease == input$diseaseCaseUi) & (siDat$reportdatetime >= (min(input$reportdateCaseUi))) & (siDat$reportdatetime <= (max(input$reportdateCaseUi)) )), ]
      } else{
        siDat[((siDat$region_name %in% input$regionCaseUi) & (siDat$disease == input$diseaseCaseUi) & (siDat$reportdatetime >= (min(input$reportdateCaseUi) )  ) & (siDat$reportdatetime <= (max(input$reportdateCaseUi) ))),]
      }
    })
    
    # fiter by district of case
    siDatRtDiseaseRegionDistFilter = reactive({
      if(is.null(input$districtCaseUi))
      {
        temp = siDatRtDiseaseRegionFilter()
      } else{
        temp = siDatRtDiseaseRegionFilter() %>%
          dplyr::filter(district_name %in% input$districtCaseUi)
      }
      return(temp)
      
    })  
    
    output$rtPlot <- renderPlot({
      dateSumCase = aggregate(total ~ reportdate, data = casePersonFilter(), sum, na.rm = F)
      # completting missing dates
      dateSumCase =  dateSumCase %>%
        dplyr::mutate(Date = as.Date(reportdate)) %>%
        tidyr::complete(Date = seq.Date(min(Date), max(Date), by="day"), fill = list(total = 0))
      dateSumCase = dateSumCase[,c(1,3)] # dropping old ulcompletted date
      colnames(dateSumCase) = c("dates","I")
      temp = siDatRtDiseaseRegionDistFilter()
      temp = temp[temp$si > 0,]  # deleting pairs with si = 0, this is needed for downtream analysis
      n = nrow(temp)
      si_data = data.frame(matrix(0,n,5))
      si_data[,2] = 1
      si_data[,3] = c(temp$si -1)
      si_data[,4] = temp$si
      colnames(si_data) =  c("EL", "ER", "SL", "SR", "type")
      si_data[,-5] = apply(si_data[,-5], 2, as.integer) # all columns except type should be integer

      if(input$rtMethodUi == "Parametric-Gamma"){
        fig = RtPlot(mean_si = 5.2, std_si = 2.3, method = "parametric_si",  burnin = 1000, dateSumCase = dateSumCase, si_data = si_data, rsi = input$rsiUi) # method = "parametric_si" or "si_from_data"; rsi = "all", "R", "SI"
      }
      if(input$rtMethodUi == "Transmission data" ){
        fig =  RtPlot(dateSumCase = dateSumCase, method = "si_from_data",  burnin = 1000,  si_data = si_data, rsi = input$rsiUi) # method = "parametric_si" or "si_from_data"; rsi = "all", "R", "SI"
      }
      return(fig)

    })
    
    ## superspreading analysis
    # Filtering by disease and region of infector case
    infectorInfecteeDataDiseaseRegionFilter = reactive({
      if(is.null(input$regionCaseUi))
      {
        infectorInfecteeData[((infectorInfecteeData$disease_infector == input$diseaseCaseUi) & (infectorInfecteeData$report_date_infector >= (min(input$reportdateCaseUi))) & (infectorInfecteeData$report_date_infector <= (max(input$reportdateCaseUi)) )), ]
      } else{
        infectorInfecteeData[((infectorInfecteeData$region_infector %in% input$regionCaseUi) & (infectorInfecteeData$disease_infector == input$diseaseCaseUi) & (infectorInfecteeData$report_date_infector >= (min(input$reportdateCaseUi) )  ) & (infectorInfecteeData$report_date_infector <= (max(input$reportdateCaseUi) ))),]
      }
      
    })
    
    # fiter by district of case
    infectorInfecteeDataDiseaseRegionDistFilter = reactive({
      if(is.null(input$districtCaseUi))
      {
        temp = infectorInfecteeDataDiseaseRegionFilter()
      } else{
        temp = infectorInfecteeDataDiseaseRegionFilter() %>%
          dplyr::filter(district_infector %in% input$districtCaseUi)
      }
      return(temp)
    })
    
 # fiting normal, weibull, gamma, lnorm distributions to serial intervals 
    siRet <- reactive({
      temp = infectorInfecteeDataDiseaseRegionDistFilter()
      siRet = serialIntervalPlot(infectorInfecteePair = temp,  distr = input$siDistMethodUi, minSi = input$serialIntervalRangeUi[1], maxSi = input$serialIntervalRangeUi[2] ) 
      return(siRet)
    })
    # plotting SI
    output$distribution_SI_plot <- renderPlot({
      temp = siRet()
      temp$siDistributionPlot
    })
    
    # exporting estimates of SI
    output$SI_estimate <- renderPrint({
      temp = siRet()
      temp$siEstmate
    })
    
    # Offspring distribution plot, dispersion parameter k
    kRet <- reactive({
      temp = infectorInfecteeDataDiseaseRegionDistFilter()
      kRet = offspringDistPlot(infectorInfecteePair = temp)
      return(kRet)
    })
    # plotting k
    output$distribution_k_plot <- renderPlot({
      temp = kRet()
      temp$offspringDistributionPlot
    })
    # exporting estimate of k
    output$k_estimate <- renderPrint({
      temp = kRet()
      temp$rkEstmate
    })
    
    ## end of case data analysis ##


     ##### EVETN DATA ANALYSIS  ##################
    eventLocRegDistFilter = reactive({
      if(input$regionUi == "All regions")
      {
        eventLocRegDist[((eventLocRegDist$diseaseEvent == input$diseaseUi) & (eventLocRegDist$reportdatetime >= (min(input$reportdateUi))) & (eventLocRegDist$reportdatetime <= (max(input$reportdateUi)) )), ]
      } else{
        eventLocRegDist[((eventLocRegDist$region_name == input$regionUi) & (eventLocRegDist$diseaseEvent == input$diseaseUi) & (eventLocRegDist$reportdatetime >= (min(input$reportdateUi) )  ) & (eventLocRegDist$reportdatetime <= (max(input$reportdateUi) ))),]
      }

    })

## EVENT DATA ANALYSIS: dashboard and tables -----
    # ui element to filter eventData by disdrict based of users selected region 
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

    selEventRegionUi = reactive({
      if(!is.null(input$regionEventUi)){
        eventData[((eventData$region_name  %in% input$regionEventUi) & (eventData$disease_event == input$diseaseEventUi) & (eventData$reportdatetime_event >= (min(input$reportdateEventUi) )  ) & (eventData$reportdatetime_event <= (max(input$reportdateEventUi) ))),]
      } else{
          eventData[((eventData$disease_event == input$diseaseEventUi) & (eventData$reportdatetime_event >= (min(input$reportdateEventUi))) & (eventData$reportdatetime_event <= (max(input$reportdateEventUi)) )), ]
          
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
   
   # renaming selected data to eventDataDiseaseRegionTimeFilter  
    eventDataDiseaseRegionTimeFilter = reactive({
      selEventRegionDistUi()
    })
    
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
    
    # Event dashboard indicators ----
    ## rotal events
    output$totalEvent <- renderInfoBox({
      infoBox(
        "Total Events", nrow(eventDataDiseaseRegionTimeFilter() ), icon = icon("users"),  color = colEvent, fill = FALSE)
    })  
    # totol event particopant
    output$totalEventParticipants <- renderInfoBox({
      infoBox(
        "Total Event Participants", nrow(eventDataDiseaseRegionTimeFilter() ), icon = icon("users"),  color = colEvent, fill = FALSE)
    })
    # total resulting cases from events
    output$totalEventResultingCases <- renderInfoBox({
      infoBox(
        "Total resulting cases", nrow(eventDataDiseaseRegionTimeFilter() ), icon = icon("procedures"),  color = colCase, fill = FALSE)
    })
    # total contacts  in events
    output$totalEventContacts <- renderInfoBox({
      infoBox(
        "Total resulting contacts", nrow(eventDataDiseaseRegionTimeFilter() ), icon = icon("handshake"),  color = colCont, fill = FALSE)
    })
    
    # total events by management status
    output$totalEventManagementPending <- renderInfoBox({
      infoBox(
        "Pending", nrow(eventDataDiseaseRegionTimeFilter() ), icon = icon("cog"),  color = colEvent, fill = FALSE) 
    })
    # 
    output$totalEventManagementOngoing <- renderInfoBox({
      infoBox(
        "Ongoing", nrow(eventDataDiseaseRegionTimeFilter() ), icon = icon("cog"),  color = colEvent, fill = FALSE)
    })
    # 
    output$totalEventManagementDone <- renderInfoBox({
      infoBox(
        "Done", nrow(eventDataDiseaseRegionTimeFilter() ), icon = icon("cog"),  color = colEvent, fill = FALSE)
    })
    # 
    output$totalEventManagementClosed <- renderInfoBox({
      infoBox(
        "Discarded", nrow(eventDataDiseaseRegionTimeFilter() ), icon = icon("cog"),  color = colEvent, fill = FALSE)
    })
    
    
    
    ## evnet status by type of place
    output$eventCuntbytyplaceTable <- DT::renderDataTable({
     
      # temp = twoByTwoTablefunction(data = eventDataDiseaseRegionTimeFilter() , Var1 = "eventstatus", Var2 = "typeofplace_event", spread = TRUE, Proportion = FALSE)
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
          # proportionByregion(data = eventCuntbytyplaceTable),
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
          # proportionByregion(data = eventCuntbytyplaceTable),
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
  
      #tempData = data.frame(eventDataDiseaseRegionTimeFilter())
      #temp = twoByTwoTablefunction(data = eventDataDiseaseRegionTimeFilter() , Var1 = "eventstatus", Var2 = "risklevel_event", spread = TRUE, Proportion = FALSE)
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
          # proportionByregion(data = eventCuntbytyplaceTable),
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
        temp = temp[, (colnames(temp) %in% c("eventinvestigationstatus","eventstatus")) ]
      }
        rowVar = colnames(temp)[1]
        colVar = colnames(temp)[2]
        
      # computing table values
      if(input$EventIndicatorTypeUi == "Count"){
        temp = twoByTwoTablefunction(data = temp , Var1 = rowVar, Var2 = colVar, spread = TRUE, Proportion = FALSE)
        res = DT::datatable(
          temp,
          filter = 'top', extensions = c('Buttons', 'Scroller'), # filter = 'none',
          options = list(scrollY = 400,
                         scrollX = 800,
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
        temp = twoByTwoTablefunction(data = temp, Var1 = rowVar, Var2 = colVar, spread = TRUE, Proportion = TRUE)
        res =  DT::datatable(
          # proportionByregion(data = eventCuntbytyplaceTable),
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
      #if piechartEventVaribleUi
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

## event count by jurisdiction -----
    output$eventCountbyJurisdictionTable <- DT::renderDataTable({
    
      if(is.null(input$regionEventUi) )
      {
        cuntbyRegionTableEvent = cuntbyRegionDistrictEvent(data = eventDataDiseaseRegionTimeFilter() , byRegion = TRUE )
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
      map_text_size = input$eventMapTextSizeUi
      if(input$eventMapShapesUi == "Region")
      {
        fg = qtm(shp = regionShapesFrance, text = "NAME_1", 
                 text.size = map_text_size, style = "white", format = "World") + tm_compass(type="8star", position=c("left", "bottom")) # fill.palette = "-Blues" to qhow in reverse order, type = "4star", "8star", "radar", "rose"
      }
      if(input$eventMapShapesUi == "Departement")
      {
        fg = qtm(shp = departementShapesFrance, text = "NAME_2", 
                 text.size = map_text_size, style = "white", format = "World") + tm_compass(type="8star", position=c("left", "bottom")) 
      }
      if(input$eventMapShapesUi == "Arrondissement")
      {
        fg = qtm(shp = ArrondissementFrance, text = "NAME_3", 
                 text.size = map_text_size, style = "white", format = "World") + tm_compass(type="8star", position=c("left", "bottom")) 
      }
      if(input$eventMapShapesUi == "Canton")
      {
        fg = qtm(shp = CantonFrance, text = "NAME_4", 
                 text.size = map_text_size, style = "white", format = "World") + tm_compass(type="8star", position=c("left", "bottom")) 
      }
      if(input$eventMapShapesUi == "Commune")
      {
        fg = qtm(shp = CommuneFrance, text = "NAME_5", 
                 text.size = map_text_size, style = "white", format = "World") + tm_compass(type="8star", position=c("left", "bottom")) 
      }
      
      return(fg)
    })
  
    
  # event leafletmap
    # https://rstudio.github.io/leaflet/shiny.html 
    #https://github.com/atmajitg/bloodbanks  to get sample app
    # Reactive expression for the data subsetted to what the user selected
    filteredData <- reactive({
      eventData[eventData$n_ep >= input$range[1] & eventData$n_ep <= input$range[2],]
    })
    
    # This reactive expression represents the palette function,
    # which changes as the user makes selections in UI.
    colorpal <- reactive({
      colorNumeric(input$colors, eventData$n_ep)
    })
    
    output$map <- renderLeaflet({
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
    
# Model specifocation----
    output$model_specificationUI <- renderUI(
      includeMarkdown(paste0("Documentation/model_specification_", input$language,".md"))
    )

} 
)










