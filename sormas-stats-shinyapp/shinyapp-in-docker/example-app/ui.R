
shinyUI(bootstrapPage(
  useShinyjs(),
  navbarPage(
             # theme = shinytheme("flatly"),
              theme = shinytheme("cerulean"),  # to change theme
             # theme = shinytheme("darkly"),
             collapsible = TRUE,
             inverse = FALSE,
             "SORMAS Stats",
             id="nav",
             tabPanel( "Transmission Network", 
                       icon = icon("project-diagram"), # icon("filter"),
                       sidebarLayout(
                         div( id ="Sidebar",
                         sidebarPanel(
                           # Filter specific to network diagram only
                           span(tags$i(h6("Filter options for network diagram. Please filter by region, district and click on Visualize network diagram to plot network diagram.")), style="color:#045a8d"),
                           pickerInput("diseaseUi", "Disease", 
                                       choices = c("CORONAVIRUS", "LASSA","MONKEYPOX", "LASSA", "CSM","EVD","NEW_INFLUENZA", "PLAGUE",
                                                   "UNDEFINED","UNSPECIFIED_VHF","MEASLES","OTHER"), 
                                       selected = c("CORONAVIRUS"),
                                       multiple = FALSE),
                           #br(),
                           dateRangeInput("reportdateUi","Report date (dd-mm-yyyy)" , start = Sys.Date()-30, end = Sys.Date(), min = NULL,
                                          max = NULL, format = "dd-mm-yyyy", startview = "month",
                                          weekstart = 0, language = "en", separator = " to ", width = NULL,
                                          autoclose = TRUE),

                          # filter by Region
                          pickerInput(
                            inputId = "regionNetworkUi",
                            label = 'Region of infection',
                            choices = sort(levels(as.factor(elist$region_name))),
                            options = list(
                              `actions-box` = TRUE, 
                              size = 12
                            ),
                            selected = NULL,
                            multiple = TRUE
                          ),
                          # filter by district
                          uiOutput('pickerInputDistrict2'),
                          # checkboc to plot notwork diagram
                          checkboxInput("visNetworkDiagramUi", "Visualize network diagram ?", FALSE),
                          
                          # filter by entity type
                          pickerInput(
                            inputId = "contactEntitiyTypeUi", 
                            label = 'Source infector entity type',
                            choices = sort(unique(elist$entityType)), 
                            options = list(
                              `actions-box` = TRUE, 
                              size = 12
                            ),
                            selected = NULL,
                            multiple = TRUE
                          ),
                          
                          # filter by relationship with infector
                          pickerInput(
                            inputId = "relationCaseUi", 
                            label = 'Contact settings',
                            choices = sort(levels(as.factor(elist$relationtocase))), 
                            options = list(
                              `actions-box` = TRUE, 
                              size = 12
                            ),
                            selected = NULL,
                            multiple = TRUE
                          ),
                          
                          # filter by event status
                          pickerInput(
                            inputId = "eventstatusUI", 
                            label = 'Event Status',
                            choices = sort(levels(as.factor(elist$eventstatus))), 
                            options = list(
                              `actions-box` = TRUE, 
                              size = 12
                            ),
                            selected = NULL,
                            multiple = TRUE
                          ),
                          
                          # filter by event risk level
                          pickerInput(
                            inputId = "risklevelUI", 
                            label = 'Event risk level',
                            choices = sort(levels(as.factor(elist$risklevelEvent))), 
                            options = list(
                              `actions-box` = TRUE, 
                              size = 12
                            ),
                            selected = NULL,
                            multiple = TRUE
                          ),
                          
                          checkboxInput("resultingCaseOnlyUi", "Only chains with resulting cases ?", TRUE),
                          checkboxInput("excludeHealthyEventPartUi", "Exclude healthy event participant ?", FALSE),
                          checkboxInput("IgraphLayoutUi", "Fast and fixed visualization ?", TRUE),
                          checkboxInput("activeEventsOnlyUi", "Only chains with active events ?", FALSE),
                         textInput("visSingleChainUi", label = h5("Only chain resulting from this ID"),
                                   value = "", placeholder = "Enter uuid of node ..." ),
                        
                        textInput("visSingleNodeUi", label = h5("Only contacts of this ID"),
                                  value = "", placeholder = "Enter uuid of node ..." ),
                          
                          br(),
                          #hr(),
                         
                          h6("Powered by:"),
                          tags$img(src = "HZI_Logo.jpg", height = 50, width = 200)
                           ,
                          width = 2)
                         ),
                         
                         mainPanel(
                           width = 10, 
                           actionButton("toggleSidebar", "Toggle sidebar"),
                                   fluidRow(
                                     visNetworkOutput("transChain", width = "100%", height = "100vh"), width=12 )
                           ,
                           hr(style = "border-color: #cbcbcb;"),
                           
                           fluidRow(
                             column(width = 5, align = "left",
                                    box(
                                      # title = "Summary of number of ontacts per node (Node degree)",
                                      h4(helpText("Source infector node ID. Use this ID to extract a single chain",  )),
                                      #footer = "Node degree",
                                      status ="primary", #  "success", # or "warning", 
                                      solidHeader = FALSE,
                                      collapsible = TRUE,
                                      collapsed = FALSE,
                                      width = 15,
                                      #height = 112, # 118, # 142,
                                      verbatimTextOutput("sourceNodeUUID")
                                    )
                             )
                             ,
                             column(width = 5, align = "left",
                                    p('* Zoom in the network diagram to see the contact categories and IDs for person and event nodes.', style = "font-size: 95%"),
                                    p('* Right click on the network diagram or legend to download or copy.', style = "font-size: 95%"),
                                    p("* These filters are based on complete data, thus entities with missing values (NULL or UNKNOWN) for the variable used for filtering are droped.", style = "font-size: 95%"),
                                    p('* All of the data used to generate indicators and figures in this app were obtained from', tags$a(href = "https://sormasorg.helmholtz-hzi.de/", 'SORMAS', target = '_blank'), '.', style = "font-size: 95%")
                               
                                   )
                           )
                         ) # end of main panel
                       
                       )
                       ,
                       # network parameters
                       wellPanel(style = "background: white", 
                                 h4(helpText("Network summary indicators")),
                                 fluidRow(
                                   column(2, infoBoxOutput("totalEdges", width = 12)),
                                   column(2, infoBoxOutput("totalNodes", width = 12)),
                                   column(2, infoBoxOutput("totalEventNodes", width = 12)),
                                   column(2, infoBoxOutput("totalReultingcasesEdges", width = 12)),
                                   column(2, infoBoxOutput("edgeDensity", width = 12)),
                                   column(2, infoBoxOutput("diameterDirected", width = 12))
                                 ),
                                 fluidRow(
                                   column(2, infoBoxOutput("totalPersonNodes", width = 12)),
                                   column(2, infoBoxOutput("totalContactEpPersonNodes", width = 12)),
                                   column(2, infoBoxOutput("tot3", width = 12)),
                                   column(2, infoBoxOutput("totalReultingcasesNodes", width = 12)),
                                   column(2, infoBoxOutput("propContactEpPersonConverted", width = 12)),
                                   column(2, infoBoxOutput("diameterUndirected", width = 12))
                                 ),
                                 
                                 fluidRow(
                                   column(2, infoBoxOutput("totInfectorCaseEventNodes", width = 12)),
                                   column(2, infoBoxOutput("totSourceInfectorCaseEventNodes", width = 12)),
                                   column(2, infoBoxOutput("propInfectorCaseEventNodes", width = 12)),
                                   column(2, infoBoxOutput("tedfff", width = 12)),
                                   column(2, infoBoxOutput("tedggg", width = 12)),
                                   column(2, infoBoxOutput("transitivityScore", width = 12))

                                 )
                       ),
                      
                       fluidRow( width = 12,
                                 column(2,
                                        wellPanel(
                                          h4(helpText("Total node counts by classification")),
                                          div( DT::dataTableOutput("nodeClassificationCountTable", width = "100%", height = "auto"), style = "font-size: 100%; width: 100%")
                                        )
                                 ),
                                 column(5,
                                        wellPanel(
                                          h4(helpText("Histogram of betweeness score")),
                                          #div( DT::dataTableOutput("networkParameter2Table")  , style = "font-size: 100%; width: 100%" )
                                          fluidRow(
                                            width = 12,
                                            plotOutput("nodeBetweenessHist", width = "100%", height = "40vh")
                                          )
                                          ,
                                          fluidRow(
                                            width = 12,
                                            box(
                                              # title = "Summary of number of ontacts per node (Node degree)",
                                              h4(helpText("Summary of node betweeness")),
                                              #footer = "Node degree",
                                              status ="primary", #  "success", # or "warning", 
                                              solidHeader = FALSE,
                                              collapsible = TRUE,
                                              collapsed = FALSE,
                                              width = 15,
                                              #height = 142, # 118, # 142,
                                              verbatimTextOutput("nodeBetweenessSummary")
                                            )
                                          )
                                        )
                                 )
                                 ,
                                 column(5,
                                        wellPanel(
                                          h4(helpText("Histogram of number of contacts per node (Node degree)")),
                                          fluidRow(
                                            width = 12,
                                            plotOutput("nodeDegreeHist", width = "100%", height = "40vh")
                                          ),
                                          fluidRow(
                                            width = 12,
                                            box(
                                             # title = "Summary of number of ontacts per node (Node degree)",
                                              h4(helpText("Summary of number of ontacts per node (Node degree)")),
                                              #footer = "Node degree",
                                              status ="primary", #  "success", # or "warning", 
                                              solidHeader = FALSE,
                                              collapsible = TRUE,
                                              collapsed = FALSE,
                                              width = 15,
                                             #height = 142, # 118, # 142,
                                              verbatimTextOutput("nodeDegreeSummary")
                                            )
                                          )
                                        )
                                 )
                                 
                       ) 
                       
             ),
## end of network diagram analysis              
             
##### contat data analysis -----
tabPanel("Contact data analysis", icon = icon("handshake"),
         sidebarLayout( position = "left",
                        sidebarPanel(
                          span(tags$i(h6("Visualization options.")), style="color:#045a8d"),
                          width = 2,
                          pickerInput("diseaseContactUi", "Disease", 
                                      choices = c("CORONAVIRUS", "LASSA","MONKEYPOX", "LASSA", "CSM","EVD","NEW_INFLUENZA", "PLAGUE",
                                                  "UNDEFINED","UNSPECIFIED_VHF","MEASLES","OTHER"), 
                                      selected = c("CORONAVIRUS"),
                                      multiple = FALSE),
                          #br(),
                          dateRangeInput("reportdateContactUi","Report date (dd-mm-yyyy)" , start = Sys.Date()-30, end = Sys.Date(), min = NULL,
                                         max = NULL, format = "dd-mm-yyyy", startview = "month",
                                         weekstart = 0, language = "en", separator = " to ", width = NULL,
                                         autoclose = TRUE),
                          
                          # Filter specific event region and district
                          pickerInput(
                            inputId = "regionContactUi",
                            label = 'Region of contact',
                            choices = sort(levels(as.factor(contRegionDist$region_name))),
                            options = list(
                              `actions-box` = TRUE,
                              size = 12
                            ),
                            selected = NULL,
                            multiple = TRUE
                          )
                        ),
                        
                      mainPanel(
                        tabsetPanel(
                          tabPanel("Contact dashboard",
                                   wellPanel(style = "background: white", 
                                            #h4('Contact classification'),
                                             fluidRow(width = 12,
                                               column(2, infoBoxOutput("allCont", width = 12)),
                                               column(2, infoBoxOutput("contConfirmed", width = 12)),
                                               column(2, infoBoxOutput("contUnconfirmed", width = 12)),
                                               column(2, infoBoxOutput("contNot", width = 12)),
                                               column(2,infoBoxOutput("activeCont", width = 12)),
                                               column(2,infoBoxOutput("convertedToCase", width = 12))
                                             )
                                             ,
                                             # h4('Contact status'),
                                             fluidRow(width=12,
                                                      # column(2,infoBoxOutput("activeCont", width = 12)),
                                                      # column(2,infoBoxOutput("convertedToCase", width = 12)),
                                                      column(2,infoBoxOutput("dropped", width = 12)),
                                                      column(2,infoBoxOutput("inactiveCont", width = 12)),
                                                      column(2, infoBoxOutput("minConPerCase", width = 12)),
                                                      column(2,infoBoxOutput("medianConPerCase", width = 12)),
                                                      column(2, infoBoxOutput("meanConPerCase", width = 12)),
                                                      column(2,infoBoxOutput("maxConPerCase", width = 12))
                                                      )
                                             
                                   ),
                                   
                                   plotOutput("plot", width = "100%", height = "90vh"),downloadButton("downloadBarplot", "Download this plot")
                                   , tags$br(),tags$br(),
                                   " Each bar in this plot represents a region or district and the height of the bar corresponds to the number of contacts 
                                      in the region or district.")
                          ,
                          tabPanel("Contacts per case", plotOutput("plotContPerCase", width = "100%", height = "90vh"),  downloadButton("downloadContPerCasePlot", "Download this plot"), tags$br(),tags$br(),
                                   " Contact per case."),
                          tabPanel("Detailed contact export",
                                   numericInput("maxrows", "Rows to show", 20),
                                   verbatimTextOutput("conRegionDistExpTable"),
                                   downloadButton("conRegionDistExpCsv", "Download as CSV"),tags$br(),tags$br(),
                                   "Each row in this data is a contact between a case person and a contact person. The data was obtained by merging contacts and their cases, thus the columns contains variables for contacts and cases"),
                          tabPanel("Contacts per case export",
                                   numericInput("maxrows", "Rows to show", 20),
                                   verbatimTextOutput("conPerCaseExpTable"),
                                   downloadButton("conPerCaseExpCsv", "Download as CSV"),tags$br(),tags$br(),
                                   "Each row in this data is a case. The data was obtained by summing the number of contacts for each case. Cases with no contact are not included in this table"),
                          tabPanel("Contact by region export",
                                   numericInput("maxrows", "Rows to show", 20),
                                   verbatimTextOutput("conPerGerionExpTable"),
                                   downloadButton("conPerGerionExpCsv", "Download as CSV"),tags$br(),tags$br(),
                                   "Each row in this data is a region with corresponding number of contacts.
                                         The data was obtained by summing the number of contacts in each region.
                                         The resgion of the source case was used in case the region of the contact was missing."

                          ),
                          
                          tabPanel("Serial Interval Export", icon = icon("table"),
                                   width = 10,
                                  mainPanel(width=12,
                                             dashboardPage(
                                               # dashboardHeader(title = "SI"),
                                               dashboardHeader( ),
                                               dashboardSidebar( disable = TRUE,
                                                                 pickerInput("conPerson", "Contact entity type", choices = c("Contact", "Contact person"), # option to view contact or contact person
                                                                             selected = c("Contact"),
                                                                             multiple = FALSE)
                                               )
                                               ,
                                               dashboardBody(
                                                 numericInput("maxrows", "Rows to show", 20),
                                                 verbatimTextOutput("siExpTable"),
                                                 downloadButton("siExpCsv", "Download as CSV"),tags$br(),tags$br(),
                                                 "Each row in this data is a contact between a case person and a contact person.
                                         The data was obtained by calculating the number of days between the symptom onset of the source case person and that of the secondary case (contact) person.
                                         Only contacts that resulted to a case were cosidered."
                                               )
                                             ))
                          )
                          )
                        , width = 10)
              )
         ),
             
##### Case data analysis ######## 
             tabPanel( "Case data analysis", icon = icon("procedures"),
                       sidebarLayout(
                         sidebarPanel( 
                           span(tags$i(h6("Visualization options.")), style="color:#045a8d"),
                           
                           conditionalPanel(condition = "input.tabs1==1",
                                            radioButtons("timeUnitEpicurveUi","Choose an option",  choices = c("Day","Epi-week", "Month"),selected = c("Epi-week"))
                                            ),
                           conditionalPanel(condition = "input.tabs1==2",
                                            radioButtons("timeUnitUi","Choose",  choices = c("Day","Epi-week", "Month"),selected = c("Epi-week")),
                                            checkboxInput(inputId = "cumUi", label = "Cummulative cases", value =F),
                                            checkboxInput(inputId = "byRegiontimeUnitUi", label = "Cases by region", value =F),
                                            checkboxInput(inputId = "cumRegionUi", label = "Cummulative cases by region", value =F)
                                            ),
                           conditionalPanel(condition = "input.tabs1==3", h4(" ")),
                           conditionalPanel(condition = "input.tabs1==4",
                                            radioButtons("caseMapshapesUi","Map shapes",  choices = c("By region","By district"),selected = c("By region")),
                                            radioButtons("caseleafletMapUi","Map type",  choices = c("Shapfiles","Leaflet"),selected = c("Shapfiles")),
                                            radioButtons("caseIndicatorTypeMapUi","Indicator type",  choices = c("Count","Proportion", "Incidence proportion / 100,000"),selected = c("Incidence proportion / 100,000"))
                                           ),
                           conditionalPanel(condition = "input.tabs1==5",
                                            radioButtons("rtMethodUi","SI estimation method ",  choices = c("Parametric-Gamma","Transmission data"),selected = c("Parametric-Gamma")),
                                            radioButtons("rsiUi","Ploting parameters",  choices = c("all","R","SI"),selected = c("R"))
                                            ),
                           conditionalPanel(condition = "input.tabs1==6",
                                            pickerInput("diseaseCaseUi", "Disease", 
                                                        choices = c("CORONAVIRUS", "LASSA","MONKEYPOX", "LASSA","CSM","EVD","NEW_INFLUENZA", "PLAGUE",
                                                                    "UNDEFINED","UNSPECIFIED_VHF","MEASLES","OTHER"), 
                                                        selected = c("CORONAVIRUS"),
                                                        multiple = FALSE),
                                            dateRangeInput("reportdateCaseUi","Report date (dd-mm-yyyy)" , start = Sys.Date()-30, end = Sys.Date(), min = NULL,
                                                           max = NULL, format = "dd-mm-yyyy", startview = "month",
                                                           weekstart = 0, language = "en", separator = " to ", width = NULL,
                                                           autoclose = TRUE),
                                            # filter by Region of event                 
                                            pickerInput(
                                              inputId = "regionCaseUi",
                                              label = 'Region of case',
                                              choices = sort(levels(as.factor(casePersonRegionDist$region_name))),
                                              options = list(
                                                `actions-box` = TRUE,
                                                size = 12
                                              ),
                                              selected = NULL,
                                              multiple = TRUE
                                            ),
                                            # # filter by district
                                            uiOutput('pickerInputdistrictCaseUi'),
                                            radioButtons("caseByRegionIndicatorTypeUi","Indicator type",  choices = c("Count","Proportion", "Incidence Proportion / 100,000"),selected = c("Count"))
                                            ),
                           conditionalPanel(condition = "input.tabs1==7",
                                            radioButtons("siDistMethodUi","Serial interval distribution fit ",  choices = c("Lognormal","Normal", "Weibull", "Gamma"),selected = c("Lognormal"))
                                            ,
                                            sliderInput("serialIntervalRangeUi", label = "Choose the serial interval range", min = -30, 
                                                        max = 50, step = 1, value = c(0, 50))
                           ),
                                            width = 2),
                         mainPanel( 
                           tabsetPanel(id="tabs1",
                                       tabPanel("Case dashboard", value = 6,
                                                wellPanel(style = "background: white",
                                                          h4('Case classification'),
                                                          fluidRow(width = 12,
                                                                   infoBoxOutput("allCase", width = 2),
                                                                   infoBoxOutput("UnclassifiedCases",width = 2),
                                                                   infoBoxOutput("suspectedCases", width = 2),
                                                                   infoBoxOutput("probableCases",width = 2),
                                                                   infoBoxOutput("confirmedCases", width = 2)
                                                          ),
                                                          h4('Case quarantine'),
                                                          fluidRow(width=12,
                                                                   infoBoxOutput("institutionalQCases",width = 2),
                                                                   infoBoxOutput("homeQCases", width = 2),
                                                                   infoBoxOutput("otherQCases", width = 2),
                                                                   infoBoxOutput("noneQCases", width = 2),
                                                                   infoBoxOutput("unknownQCases", width = 2),
                                                                   infoBoxOutput("MissingQCases", width = 2)
                                                          ),
                                                          h4('Case gender'),
                                                          fluidRow(width=12,
                                                                   infoBoxOutput("maleSexCases",width = 2),
                                                                   infoBoxOutput("femaleSexCases", width = 2),
                                                                   infoBoxOutput("otherSexCases", width = 2),
                                                                   infoBoxOutput("unknownSexCases", width = 2),
                                                                   infoBoxOutput("MissingSexCases", width = 2)
                                                          ),
                                                          h4('Case age'),
                                                          fluidRow(width=12,
                                                                   infoBoxOutput("minAgeCases",width = 2),
                                                                   infoBoxOutput("medianAgeCases", width = 2),
                                                                   infoBoxOutput("meanAgeCases", width = 2),
                                                                   infoBoxOutput("maxAgeCases", width = 2),
                                                                   infoBoxOutput("missingAgeCases", width = 2)
                                                          ),
                                                          h4('Case outcome'),
                                                          fluidRow(width=12,
                                                                   infoBoxOutput("diseasedCases",width = 2),
                                                                   infoBoxOutput("recoveredCases", width = 2),
                                                                   infoBoxOutput("unknownOutcomeCases", width = 2),
                                                                   infoBoxOutput("noOutcomeCases",width = 2)
                                                          ),
                                                          h4('Case occupation'),
                                                          fluidRow(width=12,
                                                                   infoBoxOutput("healthWorkerOcupCases",width = 2),
                                                                   infoBoxOutput("otherOcupCases", width = 2),
                                                                   infoBoxOutput("MissingOcupCases", width = 2)
                                                          ),
                                                          h4('Case Origin'),
                                                          fluidRow(width=12,
                                                                   infoBoxOutput("incountryCases",width = 2),
                                                                   infoBoxOutput("importedCases", width = 2)
                                                          )
                                                ),
                                              tags$br(),tags$br(),
                                                " Add some description text here")
                                       ,
                                       tabPanel("Cases by region", value = 6,
                                                fluidRow(
                                                  column(12, DT::dataTableOutput("caseCountbyRegionTable"))    
                                                ),
                                                # tags$br(),
                                                h4(strong("Meaning of column headers")),
                                                #tags$br(),
                                                h5(strong("Name:"), "Name of administrative unit,", strong("Total:"), "Total number of cases reported within the time interval specified in the filter,", strong("Total_last24hrs:"), "Total number of cases reported in the last 24 hrs,", 
                                                strong("Confirmed:"),  "Total number of confirmed cases,", strong("Probable:"), "Total number of probable cases,",  strong("Unclassified:"), "Total number of unclassified cases,",
                                                strong("Suspected:"), "Total number of suspected cases,", strong("Deseases:"), "Total number of death cases,", strong("No_Outcome:"), "Total number of cases without an outcome yet,",
                                                strong("Recovered:"), "Total number of recovered cases,", strong("Unk_Outcome:"), "Total number of cases with unknown outcome,", strong("In_Country:"), "Total number of cases resulting from local transmission,",
                                                strong("Imported:"), "Total number of imported cases,", strong("Home_Q:"), "Total number of cases quarantine at home,", strong("Institutional_Q:"), "Total number of cases quarantine in an institurion,",
                                                strong("No_Q:"), "Total number of cases that are not on quarantine,", strong("Other_Q:"), "Total number of cases quarantine at other places,", strong("Unk_Q:"), "Total number of cases with unknown quarantine"
                                                )
                                                )
                                       ,
                                       tabPanel("Epidemic curve", value = 1, plotlyOutput("caseEpicurvePlot", width = "100%", height = "80vh"))
                                       ,
                                       tabPanel("Time series plot", value = 2, plotlyOutput("caseTimeSeriesPlot", width = "100%", height = "80vh"))
                                       ,
                                       tabPanel("Case Pyramid",  value = 3,  plotlyOutput("casePyramidPlot", width = "80%", height = "80vh") )
                                       ,
                                       tabPanel("Administrative map", value = 4, plotOutput("regionMapCaseCount", width = "100%", height = "80vh"))
                                       ,
                                       tabPanel("Reproduction number (Rt)", value = 5, plotOutput("rtPlot", width = "80%", height = "80vh"))
                                       , 
                                       tabPanel("Superspreading and individual varaince (k)", value = 7,
                                                fluidRow( width = 10,
                                                          column(6,
                                                                 wellPanel(
                                                                   h4(helpText("Distributions of serial interval")),
                                                                   fluidRow(
                                                                     width = 12,
                                                                     plotOutput("distribution_SI_plot", width = "100%", height = "40vh")
                                                                   )
                                                                   ,
                                                                   fluidRow(
                                                                     width = 12,
                                                                     box(
                                                                       h4(helpText("Estimates of serial interval")),
                                                                       #footer = "Node degree",
                                                                       status ="primary", #  "success", # or "warning", 
                                                                       solidHeader = FALSE,
                                                                       collapsible = TRUE,
                                                                       collapsed = FALSE,
                                                                       width = 15,
                                                                      # height = 148, # 118, # 142,
                                                                       verbatimTextOutput("SI_estimate")
                                                                     )
                                                                   )
                                                                 )
                                                          )
                                                          ,
                                                          column(6,
                                                                 wellPanel(
                                                                   h4(helpText("Distributions of bumber of infectee per infector")),
                                                                   fluidRow(
                                                                     width = 12,
                                                                     plotOutput("distribution_k_plot", width = "100%", height = "40vh")
                                                                   ),
                                                                   fluidRow(
                                                                     width = 12,
                                                                     box(
                                                                       h4(helpText("Estimates of reproduction number and dispersion parameter using NB distribution")),
                                                                       #footer = "Node degree",
                                                                       status ="primary", #  "success", # or "warning", 
                                                                       solidHeader = FALSE,
                                                                       collapsible = TRUE,
                                                                       collapsed = FALSE,
                                                                       width = 15,
                                                                       #height = 158, # 118, # 142,
                                                                       verbatimTextOutput("k_estimate")
                                                                     )
                                                                   )
                                                                 )
                                                          )
                                                ) 
                                     )
                                     ,
                                     tabPanel("Basisc Export",
                                                       numericInput("maxrowsCase", "Rows to show", 20),
                                                       verbatimTextOutput("casebyRegionVarTable"),
                                                       downloadButton("caseRegionVarExpCsv", "Download as CSV"),tags$br(),tags$br(),
                                                       "Each row in this data a case. The data was obtained by merging case, person, region and district tables")
                            
                           ),
                           width = 10)                       
                       )
             ),


#### Event data analysis ----
tabPanel( "Event data analysis", icon = icon("procedures"),
          sidebarLayout( position = "left",
            sidebarPanel(
              span(tags$i(h6("Filter options for events.")), style="color:#045a8d"),
              width = 2,
              pickerInput("diseaseEventUi", "Disease", 
                          choices = c("CORONAVIRUS", "LASSA","MONKEYPOX", "LASSA", "CSM","EVD","NEW_INFLUENZA", "PLAGUE",
                                      "UNDEFINED","UNSPECIFIED_VHF","MEASLES","OTHER"), 
                          selected = c("CORONAVIRUS"),
                          multiple = FALSE),
              
              dateRangeInput("reportdateEventUi","Relevant date (dd-mm-yyyy)" , start = Sys.Date()-30, end = Sys.Date(), min = NULL,
                             max = NULL, format = "dd-mm-yyyy", startview = "month",
                             weekstart = 0, language = "en", separator = " to ", width = NULL,
                             autoclose = TRUE),
              span(tags$i(h6("Relevant date uses date of event and if missing, impute with report date.")), style="color:#045a8d"),
              
              # Filter specific event region and district
              # filter by Region of event
              pickerInput(
                inputId = "regionEventUi",
                label = 'Region of event',
                choices = sort(levels(as.factor(eventData$region_name))),
                options = list(
                  `actions-box` = TRUE, 
                  size = 12
                ),
                selected = NULL,
                multiple = TRUE
              ),
              # filter by district
              uiOutput('pickerInputdistrictEventUi'),
              
              radioButtons("EventIndicatorTypeUi","Indicator type",  choices = c("Count","Proportion", "Incidence Proportion / 100,000"),selected = c("Count"))
              ,
              pickerInput(
                inputId = "eventTableColumnVaribleUi", 
                label = 'Event by Jurisdictions table',
                choices = c(levels(as.factor(event_variable_data$event_variable)) ) ,
                selected = c("Name", "Total", "Event status"),
                multiple = TRUE,
                options = pickerOptions(
                  actionsBox = TRUE,
                  header = "Choose variables to use for table columns",
                )
              ),
              
              pickerInput(
                "piechartEventVaribleUi", 'Choose variable to plot Pie chart',
                choices = c("Event Status",levels(as.factor(colnames(eventData)))) ,
                selected = "Event Status",
                multiple = FALSE
              ),
              pickerInput(
                inputId = "twoByTwotableEventVariblesUi", 
                label = 'Choose variables to plot 2X2 table',
                choices = c(levels(as.factor(colnames(eventData)))),
                multiple = TRUE,
                options = pickerOptions(
                  actionsBox = TRUE,
                  title = "Please select 2 varibles",
                  header = "Only 2 variables can be selected",
                  maxOptions = 2
                )
              ),
              
              radioButtons("eventMapShapesUi","Choose map administrative area",  choices = c("Region","Departement", "Commune"),
                           selected = c("Region")),
              sliderInput("eventMapTextSizeUi", label = "Choose text size for map", min = 0, 
                          max = 2, step = 0.2, value = 1.2)
            ),
            mainPanel(width = 10,
              tabsetPanel(tabPanel("Event dashboard", value = 6,
                                   wellPanel(style = "background: white", 
                                             fluidRow(
                                               column(2, infoBoxOutput("totalEvent", width = 12)),
                                               column(2, infoBoxOutput("totalEventManagementPending", width = 12)),
                                               column(2, infoBoxOutput("totalEventManagementOngoing", width = 12)),
                                               column(2, infoBoxOutput("totalEventManagementDone", width = 12)),
                                               column(2, infoBoxOutput("totalEventManagementClosed", width = 12)),
                                               column(2, infoBoxOutput("totalEventManagementMissing", width = 12))
                                             )
                                             ,
                                             fluidRow(
                                               column(2, infoBoxOutput("totalEventParticipants", width = 12)),
                                               column(2, infoBoxOutput("totalEventResultingCases", width = 12)),
                                               column(2, infoBoxOutput("totalEventstatusSignal", width = 12)),
                                               column(2, infoBoxOutput("totalEventstatusEvent" , width = 12)),
                                               column(2, infoBoxOutput("totalEventstatusCluster" , width = 12)),
                                               column(2, infoBoxOutput("totalEventstatusScreening" , width = 12))
                                             )
                                             
                                   ),
                                   wellPanel(
                                     h4(helpText("Barplot for events by region/district grouped by event status")) , 
                                     #plotOutput("eventBarplotUi", width = "100%", height = "auto")
                                     plotlyOutput("eventBarplotUi", width = "100%", height = "70vh")
                                     ,
                                     style = "background: white"
                                   ),
                                   fluidRow(
                                     column(4,
                                            h4(helpText("Table 1: Event status by type of place")), div(DT::dataTableOutput("eventCuntbytyplaceTable") , style = "font-size: 85%; width: 95%" )  
                                     )
                                     ,
                                     column(4,
                                            h4(helpText("Table 2: Event status by management status")) ,  div( DT::dataTableOutput("eventStatusByMangemantStatusTable")  , style = "font-size: 85%; width: 95%" )   
                                            
                                     )
                                     ,
                                     column(4,
                                            h4(helpText("Table 3: Event status by risk level")) ,  div( DT::dataTableOutput("eventStatusByRisklevelTable")  , style = "font-size: 85%; width: 95%" )  
                                     )
                                   )
                                   ,
                                   fluidRow(
                                     column(4,
                                            h4(helpText("Fig 2: Event status")), div(plotlyOutput("pieCdhartEventStatusUi", width = "95%", height = "50vh") , style = "font-size: 95%; width: 95%" )
                                     )
                                     ,
                                     column(4,
                                            h4(helpText("Fig 3: Event source type")) ,  div(plotlyOutput("pitChartEventSourceTypeUi", width = "95%", height = "50vh") , style = "font-size: 95%; width: 95%" )
                                     )
                                     ,
                                     column(4,
                                            h4(helpText("Fig 4: Event district")) ,  div(plotlyOutput("pitChartEventDidtrictUi", width = "95%", height = "50vh") , style = "font-size: 95%; width: 95%" )
                                     )
                                   )
                                   
              )
              ,
              tabPanel("Event by Jurisdiction",
                       fluidRow(
                         column(12, DT::dataTableOutput("eventCountbyJurisdictionTable") )    
                       )
                       )
              ,
              tabPanel("Custom indicators",
                       wellPanel(
                         h4(helpText("Pie chart")) ,
                         div(plotlyOutput("pieCdhartEventUi", width = "95%", height = "50vh" ), style = "font-size: 100%; width: 95%" ) 
                       ),
                       wellPanel(
                         h4(helpText("2 X 2 table")) ,
                         DT::dataTableOutput("dynamic2x2TableEventUi")
                       )
                       )
              ,
              tabPanel("Event Time series" ),
              tabPanel("Shapfile map", plotOutput("eventMapUi", width = "100%", height = "95vh")),
              tabPanel("Leaflet map",     # leafletOutput("eventLeafletMapUi", width = "100%", height = 900)  
                       leafletOutput("map", width = "100%", height = 900),
                       #leafletOutput("eventLeafletMapUi", width = "100%", height = 900), 
                       absolutePanel(top = 10, right = 10,
                                     sliderInput("range", "Magnitudes", min(eventData$n_ep), max(eventData$n_ep),  # this can be used to represent number of ep per event node
                                                 value = range(eventData$n_ep), step = 1
                                     ),
                                     selectInput("colors", "Choose color Scheme to plot entities",
                                                 rownames(subset(brewer.pal.info, category %in% c("seq", "div")))
                                     ),
                                     checkboxInput("legend", "Show legend", TRUE)
                       )
                    ),
              tabPanel("Event management" )
            ),
          ) # clossing main panel                       
        )  # clossing sidbarlayout
),

### Backward tracing including cases, contacts and events
tabPanel("Backward tracing" ),

# Model specification ----
tabPanel("Model specification",
         icon = icon("book"),
         # uiOutput("methodUI")
         radioButtons(inputId = "language", 
                      label = "",
                      choiceValues = c("en", "fr"),
                      choiceNames = c("English", "Français"),
                      selected = "en",
                      inline = TRUE),
         
         uiOutput("model_specificationUI")
),

# About ------
tabPanel(title = "About", 

h4(strong("About SORMAS and SORMAS-Stats")),

h5("The the Surveillance, Outbreak Response Management and Analysis System (SORMAS) is an open-source mHealth (mobile health) system that organises and facilitates infectious
disease control and outbreak management procedures in addition to disease surveillance and epidemiologic analysis for all administrative
levels of a public health system (1, 2). SORMAS includes specific interfaces for 12 users (e.g., laboratorian,
contact tracing officer, epidemiologist), disease-specific process modules for 19 epidemic-prone diseases,
and a customizable process module for unforeseen emerging diseases and a COVID-19 module, which we
developed to support countries in the outbreak response. Users can operate SORMAS on web or mobile
app and bidirectionally synchronized with a central server via mobile telecommunication networks."),
         
h5("SORMAS-Stats contain functions to analyze and visualize surveillance data collected by the Surveillance Outbreak
  Response Management and Analysis System (SORMAS). SORMAS is an open source mobile eHealth System that processes
  disease control and outbreak management procedures in addition to surveillance and early detection of outbreaks through real-time digital
 surveillance including peripheral health care facilities and laboratories. "), 

h4(strong("SORMAS User and some of the diseases with modules for control measures")),

img(src = "process-flow-sormas.png", width = "800", height = "900"),  img(src = "sormas_diseases.png", width = "800", height = "900"),

h5(" Mote txt to add ...")
),

# Footer ------
hr(style = "border-color: #cbcbcb;"),
fluidRow(
  column(9,
         p('All of the data used to generate indicators and figures in this app were obtained from', tags$a(href = "https://sormasorg.helmholtz-hzi.de/", 'SORMAS', target = '_blank'), '.', style = "font-size: 85%"),
         p("App created by the ", tags$a(href = "https://github.com/hzi-braunschweig/SORMAS-Stats", 'SORMAS-Stats Team', target = '_blank'), HTML("&bull;"), style = "font-size: 85%"),
         p("To use this app and other related SORMAS apps, find all the source codes on Github:", tags$a(href = "https://github.com/hzi-braunschweig", tags$i(class = 'fa fa-github', style = 'color:#5000a5'), target = '_blank'), style = "font-size: 85%"),
         p("Want to contribute? Have a question? Identify a bug or want to make a request? Open a discussion on ", tags$a(href = "https://github.com/hzi-braunschweig/SORMAS-Stats/discussions", tags$i(class = 'fa fa-github', style = 'color:#5000a5'), target = '_blank'), style = "font-size: 85%"),
         p(tags$em("Last updated: May 05, 2021"), style = 'font-size:75%'))
  ,
  column(3, align = "right",
         p('Powered by:', tags$a(href = " ", target = '_blank'), '', style = "font-size: 85%"),
         p(tags$a(href = "https://www.helmholtz-hzi.de/en/", 'HZI', target = '_blank'), '', style = "font-size: 85%"),
         p(tags$a(href = "https://solidarites-sante.gouv.fr/systeme-de-sante-et-medico-social/e-sante/", 'E-santé', target = '_blank'), '', style = "font-size: 85%"),
         p(tags$a(href = "https://www.vitagroup.ag/de_DE/Ueber-uns/symeda", 'vitagroup', target = '_blank'), '', style = "font-size: 85%"),
         p(tags$a(href = "https://www.giz.de/en/html/index.html", 'GIZ', target = '_blank'), '', style = "font-size: 85%")
  )
  )
)
))

