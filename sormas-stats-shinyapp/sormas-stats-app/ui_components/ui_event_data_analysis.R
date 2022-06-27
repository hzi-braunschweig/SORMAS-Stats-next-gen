###### Event data analysis ##########
# This sub section of the ui.r file renders the event data analysis tab
# All front-end methods related to this tab should be added in this file
tabPanel("Event data analysis", icon = icon("procedures"),
sidebarLayout(position = "left",
sidebarPanel(width = 2,
  span(tags$i(h5("Please select filter options and click on `Apply changes` to run analysis.")), style="color:#045a8d"),
  span(tags$i(h5("The relevant date filter uses  the date of event and if missing, impute with report date.")), style="color:#045a8d"),
  actionButton(inputId = "eventDataAnalysisAction", label = "Apply changes", icon =  icon("running"),
               class = "btn-primary", width = '55%'),
  hr(),
  conditionalPanel(condition =  "input.tabEvent == 1",
                   pickerInput(
                     inputId = "eventTableColumnVaribleUi", 
                     label = 'Choose table columns',
                     choices = c(levels(as.factor(event_variable_data$event_variable)) ) ,
                     selected = c("Name", "Total", "Event status"),
                     multiple = TRUE,
                     options = pickerOptions(
                       actionsBox = TRUE,
                       header = "Choose variables to use for table columns",
                     ))
  ),
  conditionalPanel(condition =  "input.tabEvent == 2",
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
                   checkboxInput("transpose2x1TableEventUi", "Transpose 2 X 2 table ?", TRUE),
                   pickerInput(
                     "piechartEventVaribleUi", 'Choose variable to plot Pie chart',
                     choices = c("Event Status",levels(as.factor(colnames(eventData)))) ,
                     selected = "Event Status",
                     multiple = FALSE
                   ),
                   pickerInput(
                     "barplotEventVaribleUi", 'Choose variable to plot bar graph',
                     choices = c("Location category",levels(as.factor(colnames(eventData)))) ,
                     selected = "Location category",
                     multiple = FALSE
                   )
  ),
  conditionalPanel(condition =  "input.tabEvent == 3",
                   radioButtons("eventMapShapesUi","Choose map administrative area",  choices = c("Region","Departement", "Commune"),
                                selected = c("Region")),
                   sliderInput("eventMapTextSizeUi", label = "Choose text size for map", min = 0, 
                               max = 2, step = 0.2, value = 1.2)
  )               
 ), 
mainPanel(width = 10,
 fluidRow(
   # filter by disease of event 
   column(2, 
          pickerInput("diseaseEventUi", "Disease", choices = sort(levels(as.factor(eventData$disease_event))), 
                      selected = c("CORONAVIRUS"),multiple = FALSE)
   ),
   # filter by date of event 
   column(2, 
          dateRangeInput("reportdateEventUi","Relevant date (dd-mm-yyyy)" , start = Sys.Date() - delay_default_UI, end = Sys.Date(), min = NULL,
                         max = NULL, format = "dd-mm-yyyy", startview = "month",
                         weekstart = 0, language = "en", separator = " to ", width = NULL,
                         autoclose = TRUE)
   ),
   # filter by Region of event
   column(2,
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
          )
   ),
   # filter by district of event
   column(2, 
          uiOutput('pickerInputdistrictEventUi')
   ),
   # filter by event identification source
   column(2,
          pickerInput(
            inputId = "eventIdentificationSourceUi",
            label = 'Identification source',
            choices = sort(levels(as.factor(eventData$event_identification_source))),
            #choices = sort(levels(as.factor( c("forward_tracing", "backward_tracing", "unknown", "NA")  ))),
            options = list(
              `actions-box` = TRUE, 
              size = 12
            ),
            selected = NULL,
            multiple = TRUE
          ) 
   ),
   #filter by indicator type
   column(2, 
          radioButtons("EventIndicatorTypeUi","Indicator type",  choices = c("Count","Proportion"),selected = c("Count"), inline = TRUE)
   ) 		   
 ),  
tabsetPanel(id= "tabEvent",
 tabPanel("Event dashboard", value = 0,
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
 tabPanel("Event by Jurisdiction", value = 1,
          fluidRow(
            column(12, DT::dataTableOutput("eventCountbyJurisdictionTable") )    
          )
 )
 ,
 tabPanel("Custom indicators",  value = 2,
          wellPanel(
            h4(helpText("2 X 2 table. Click on 'Choose variables to plot 2X2 table' to generate the same table for other attributes.")) ,
            DT::dataTableOutput("dynamic2x2TableEventUi")
          ),
          fluidRow( width = 10,                                                         
                    column(6,                                                                
                           wellPanel(
                             h4(helpText("Dynamic pie chart: Click on 'Choose variable to plot Pie chart' to plot for another varaible.")) ,
                             div(plotlyOutput("pieCdhartEventUi", width = "100%", height = "50vh" ), style = "font-size: 100%; width: 100%" ) 
                           )
                    ),
                    column(6,                    
                           wellPanel(
                             h4(helpText("Dynamic bar graph: Click on 'Choose variable to plot bar graph' to plot for another varaible.")) ,
                             div(plotlyOutput("barChartEventUi", width = "100%", height = "50vh" ), style = "font-size: 100%; width: 100%" ) 
                           )              
                    )
          ) 
 ),
 tabPanel("Shapfile map", value = 3,
          plotOutput("eventMapUi", width = "100%", height = "95vh")),
 tabPanel("Leaflet map",  #  value = 10, 
          leafletOutput("map", width = "100%", height = 900),
          absolutePanel(top = 10, right = 10,
            sliderInput("range", "Magnitudes", min(eventData$n_ep), max(eventData$n_ep),  # this can be used to represent number of ep per event node
                        value = range(eventData$n_ep), step = 1
            ),
            selectInput("colors", "Choose color Scheme to plot entities",
                        rownames(subset(brewer.pal.info, category %in% c("seq", "div")))
            ),
            checkboxInput("legend", "Show legend", TRUE)                       
          )
 )
 ) 
 ) # closing main panel                       
)  # closing sidbarlayout
)
