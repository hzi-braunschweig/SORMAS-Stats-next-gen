
## configuration
### The codes in this file connnect to sormas db and import the required data into R

# Defining parameters to be used to sample data
delay = 365 # number of days to count backward from today,  default from date = 90 days in the past
event_delay = 1000
fromDate = as.character(Sys.Date() - delay - 1) #  you can directly define fromDate as: fromDate = as.character("yyyy-mm-dd")
toDate = as.character(Sys.Date() + 1) # or toDate = as.character("yyyy-mm-dd"), +1 is added because between sql commant does not consider end of intervals
uniquePersonPersonContact = TRUE # or FALSE to keep only one contact between the same 2 persons ( case person and contact person)

event_fromDate = as.character(Sys.Date() - event_delay - 1) #  you can directly define fromDate as: fromDate = as.character("yyyy-mm-dd")
event_toDate = as.character(Sys.Date() + 1) # or toDate = as.character("yyyy-mm-dd"), +1 is added because between sql commant does not consider end of intervals

# Defining colours to be used by dashboard icons for entities
colCont = "green"  # contact colour
colCase="red"   # case colour
colEvent = "blue"   # event colour
colPerson = "black"   # contact person or event participant person colour 
colEdge = "black" 

# Defining connection to db
DB_USER = "sormas_user"
DB_PASS = "password"
DB_HOST = "127.0.0.1"
DB_PORT = "5432"
DB_NAME = "sormas"
## end of configuratiion

# load shiny packages
#source("loading_packages.R")
source(file.path(".", "loading_packages.R"))

library(shiny)
library(shinyjs)
# library(shinythemes)
# library(shinydashboard)
# library(shinyWidgets)
# 
# # load app-specific packages
# library(visNetwork)
# library(ggplot2)
# library(dplyr)
# library(plotly)
# library(extrafont)
# library(rpart)
# library(RColorBrewer)
# library(webshot) # for plot export
# library(ggthemes)
# library(lubridate)
# library(Hmisc)
# library(gdata)
# library(scales)
# library(EpiEstim)
# library(incidence)
# library(tidyr)
# library(rgdal)
# library(maps)
# library(maptools)
# library(mapdata)
# library(foreign)    
# library(sp)
# library(broom)
# library(ggmap)
# library(rgeos)
# library(tmap)
# library(sf)
# library(lattice)
# library(fontawesome)

# load binary files for functions
source(file.path(".", "loading_functions.R"))

# # functions for contact data analysis
# load(file.path("./utils", "dateTimeToDate.R"))
# load(file.path("./utils", "import.multiple.csv.files.R"))
# load(file.path("./utils", "plotNet.R"))
# load(file.path("./utils","importingData.R"))
# load(file.path("./utils","mergingData.R"))
# load(file.path("./utils","contIdsForSingleChain.R"))
# 
# ## Functions for case data analysis
# load(file.path("./utils","pyramidPlotFunction.R"))
# load(file.path("./utils","timeSeriesPlotDay.R"))
# load(file.path("./utils","timeSeriesPlotWeek.R"))
# load(file.path("./utils","timeSeriesPlotMonth.R"))
# load(file.path("./utils","timeSeriesPlotDayRegion.R"))
# load(file.path("./utils","epicurveDate.R"))
# load(file.path("./utils","epicurveMonth.R"))
# load(file.path("./utils","regionMapPlot.R"))
# load(file.path("./utils","districtMapPlot.R"))
# load(file.path("./utils","RtPlot.R"))
# ##

# Load data

source(file.path(".", "loading_data.R")) # replace this woth demo data in case not connected to a sormas server

# 
# # - use tryCatch lateron
# # - Try (1) PSQL connection, (2) Load from mounted "last-data", (3) fallback to "demo-data"
# load(file.path("./demo-data", "contRegionDist.RData")) 
# load(file.path("./demo-data", "nodeLineList.RData"))
# load(file.path("./demo-data", "elist.RData"))
# load(file.path("./demo-data", "siDat.RData"))
# load(file.path("./demo-data", "casePersonRegionDist.RData"))
# 
# ####
# ##loading and cleaning shap files. This cleaning will depend on the country and the source of teh shap file, thus codes nedd to be adjusted in this section.
# # lnd <- rgdal::readOGR(dsn = "Shapefiles", layer = "State_Aug28")
# districtShapes <- rgdal::readOGR( dsn = file.path("./demo-data/Shapefiles"), layer = "LGAs_Aug28")
# regionShapes <- rgdal::readOGR( dsn = file.path("./demo-data/Shapefiles"), layer = "State_Aug28")   
# 
# # renaming region names to match shapfiles, do this for all names that are not the same
# regionShapes@data$StateName = as.character(regionShapes@data$StateName)
# regionShapes@data$StateName[regionShapes@data$StateName == "Fct, Abuja"] = "FCT"
# regionShapes@data$StateName[regionShapes@data$StateName == "Akwa Ibom"] = "Akwa-Ibom"

######################################""""""

# Define UI for dataset viewer app ----
ui <- shinyUI(bootstrapPage(
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
                                 sliderInput("eventMapTextSizeUi", label = "Coose text size for map", min = 0, 
                                             max = 3, step = 0.1, value = 1.2)
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
                                         tabPanel("Event management" ),
                                         tabPanel("Backward tracing" )
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
               
               h5(" Mote txt to add ..."),
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

### end of ui







# Define server logic to summarize and view selected dataset ----
server <- shinyServer(
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
        temp =  elist[((elist$region_name %in% input$regionNetworkUi) & (elist$disease == input$diseaseUi) & (elist$reportdatetime >= (min(input$reportdateUi) )  ) & (elist$reportdatetime <= (max(input$reportdateUi) ))),  colnames(elist)  %in% c("id","entityType", "district_name", "relationtocase","eventstatus", "risklevelEvent", "from_uuid_person", "to_uuid_person", "resultingcase_id")]
      } else{
        temp = elist[((elist$disease == input$diseaseUi) & (elist$reportdatetime >= (min(input$reportdateUi))) & (elist$reportdatetime <= (max(input$reportdateUi)) )), colnames(elist) %in% c("id","entityType", "district_name", "relationtocase", "eventstatus", "risklevelEvent", "from_uuid_person", "to_uuid_person", "resultingcase_id") ]
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
    
    # filter to exclude helthy event participants
    selElistRegionEntityTypeSettingEventstatusRisklevelUISingleNodeEvenPartHealthy = reactive({
      if(input$excludeHealthyEventPartUi == TRUE){
        temp = selElistRegionEntityTypeSettingEventstatusRisklevelUISingleNode() %>%
          dplyr::filter(is.na(resultingcase_id) & (entityType == "Event")  )  # selecting health ep 
        
        ret = selElistRegionEntityTypeSettingEventstatusRisklevelUISingleNode() %>%
          dplyr::filter( !(id %in% temp$id)) # dropping dropping edges of healthy ep
      } else{
        ret = selElistRegionEntityTypeSettingEventstatusRisklevelUISingleNode()
      }
      return(ret)
    })
    
    # further filtering based on resultingCaseOnlyUi, activeEventsOnlyUi, visSingleChainUi
    elistSel2ResCaseSourseCase  = reactive({ 
      elistSel <- elist[elist$id %in% selElistRegionEntityTypeSettingEventstatusRisklevelUISingleNodeEvenPartHealthy()$id, ]
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
      if(input$visNetworkDiagramUi == TRUE){
        elistSel =  elistSel2ResCaseSourseCase()
        nodeLineListSelResCase <- nodeLineList[nodeLineList$id %in% unique(c(elistSel$from, elistSel$to)), ]
        # ordering colums, this is needed to be in a specific order
        nodeToPlot = nodeLineListSelResCase %>%
          dplyr::relocate(id, group, label,  value, shape,  code,  title)
        elistPlot = elistSel %>%
          dplyr::relocate(from, to)
        plotNet(nodeLineList= nodeToPlot, elist = elistPlot, IgraphLayout= input$IgraphLayoutUi)
      } 
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
          dplyr::filter(entityType == "Event") %>%
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
      
      # ordering colums, this is needed to be in a specific order for igraph
      nodeToPlot = nodeLineListSelResCase %>%
        dplyr::relocate(id, group)
      elistPlot = elistSel %>%
        dplyr::relocate(from, to)
      net <- graph_from_data_frame(d=elistPlot, vertices = nodeToPlot, directed=T)
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
    ## Total events
    output$totalEvent <- renderInfoBox({
      infoBox(
        "", nrow(eventDataDiseaseRegionTimeFilter() ), icon = icon("cog"),  color = colEvent,
        fill = FALSE, subtitle =  "Total Events")
    })  
    # totol event particopant
    output$totalEventParticipants <- renderInfoBox({
      infoBox(
        "", sum(eventDataDiseaseRegionTimeFilter()$eventPart_sum ), icon = icon("users"),
        color = colEvent, fill = FALSE, subtitle = "Event Participants")
    })
    # total resulting cases from events
    output$totalEventResultingCases <- renderInfoBox({
      infoBox(
        "", sum(eventDataDiseaseRegionTimeFilter()$resulting_case_sum ), icon = icon("procedures"),
        color = colCase, fill = FALSE, subtitle = "Resulting Cases")
    })
    
    # total events by management status
    output$totalEventManagementPending <- renderInfoBox({
      temp = eventDataDiseaseRegionTimeFilter() %>%
        dplyr::filter(eventmanagementstatus == "PENDING") 
      infoBox(
        "", nrow(temp),
        icon = icon("cog"), color = colEvent, fill = FALSE, subtitle = "Management: Pending" ) 
    })
    # 
    output$totalEventManagementOngoing <- renderInfoBox({
      temp = eventDataDiseaseRegionTimeFilter() %>%
        dplyr::filter(eventmanagementstatus == "ONGOING")
      infoBox(
        "", nrow(temp), icon = icon("cog"), color = colEvent, fill = FALSE, subtitle = "Management: Ongoing")
    })
    #
    output$totalEventManagementDone <- renderInfoBox({
      temp = eventDataDiseaseRegionTimeFilter() %>%
        dplyr::filter(eventmanagementstatus == "DONE")
      infoBox(
        "", nrow(temp), icon = icon("cog"), color = colEvent, fill = FALSE, subtitle = "Management: Done" )
    })
    # 
    output$totalEventManagementClosed <- renderInfoBox({
      temp = eventDataDiseaseRegionTimeFilter() %>%
        dplyr::filter(eventmanagementstatus == "CLOSED")
      infoBox(
        "", nrow(temp), icon = icon("cog"),  
        color = colEvent, fill = FALSE, subtitle = "Management: Closed" )
    })
    #
    output$totalEventManagementMissing <- renderInfoBox({
      temp = eventDataDiseaseRegionTimeFilter() %>%
        dplyr::filter(is.na(eventmanagementstatus))
      infoBox(
        "", nrow(temp), icon = icon("cog"),  
        color = colEvent, fill = FALSE, subtitle = "Management: Missing " )
    })
    
    ### total events by event status
    output$totalEventstatusSignal <- renderInfoBox({
      temp = eventDataDiseaseRegionTimeFilter() %>%
        dplyr::filter(eventstatus == "SIGNAL" )
      infoBox(
        "", nrow(temp), icon = icon("cog"),  
        color = colEvent, fill = FALSE, subtitle = "Signal" ) 
    })
    # 
    output$totalEventstatusEvent <- renderInfoBox({
      temp = eventDataDiseaseRegionTimeFilter() %>%
        dplyr::filter(eventstatus == "EVENT" )
      infoBox(
        "", nrow(temp), icon = icon("cog"),  
        color = colEvent, fill = FALSE, subtitle = "Event")
    })
    # 
    output$totalEventstatusCluster <- renderInfoBox({
      temp = eventDataDiseaseRegionTimeFilter() %>%
        dplyr::filter(eventstatus == "CLUSTER" )
      infoBox(
        "", nrow(temp), icon = icon("cog"),  
        color = colEvent, fill = FALSE, subtitle = "Cluster" ) 
    })
    # 
    output$totalEventstatusScreening <- renderInfoBox({
      temp = eventDataDiseaseRegionTimeFilter() %>%
        dplyr::filter(eventstatus == "SCREENING" )
      infoBox(
        "", nrow(temp), icon = icon("cog"),  
        color = colEvent, fill = FALSE, subtitle = "Screening" )
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
# Create Shiny app ----
shinyApp(ui, server)
