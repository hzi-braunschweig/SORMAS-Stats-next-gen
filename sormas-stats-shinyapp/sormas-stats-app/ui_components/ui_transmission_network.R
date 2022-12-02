###### Transmission chain analysis ##########
# This sub section of the ui.r file renders the transmission network diagram tab
# All front-end methods related to this tab should be added in this file
tabPanel("Transmission Network", 
          icon = icon("project-diagram"), # icon("filter"),
          sidebarLayout(
            sidebarPanel(
              # language settings that applies to the whole app, not just this tab
              shiny.i18n::usei18n(i18n),
              pickerInput('selected_language',
                  label=i18n$t('Change language'),
                  choices = i18n$get_languages(),
                  selected = i18n$get_key_translation()
                ),
              # Filter specific to network diagram only
              span(tags$i(h5("Please select filter options and click on the `Apply changes` icon below. Click on `Visualize network diagram` to plot the network diagram.")), style="color:#045a8d"),
              pickerInput("diseaseUi", i18n$t("Disease"), 
                          choices = c("CORONAVIRUS", "LASSA","MONKEYPOX", "LASSA", "CSM","EVD","NEW_INFLUENZA", "PLAGUE",
                                      "UNDEFINED","UNSPECIFIED_VHF","MEASLES","OTHER"), 
                          selected = c("CORONAVIRUS"),
                          multiple = FALSE),
              #br(),
              dateRangeInput("reportdateUi", i18n$t("Report date (dd-mm-yyyy)") , start = Sys.Date() - delay_default_UI, end = Sys.Date(), min = NULL,
                             max = NULL, format = "dd-mm-yyyy", startview = "month",
                             weekstart = 0, language = "en", separator = " to ", width = NULL,
                             autoclose = TRUE),  # replace start = Sys.Date()-30 in case you need to show default statistics for last 30 days only
              
              # filter by Region
              pickerInput(
                inputId = "regionNetworkUi",
                label = i18n$t('Region of infection'),
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
              # filter by entity type
              pickerInput(
                inputId = "contactEntitiyTypeUi", 
                label = i18n$t('Source infector entity type'),
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
                label = i18n$t('Contact settings'),
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
                label = i18n$t('Event status'),
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
                label = i18n$t('Event risk level'),
                choices = sort(levels(as.factor(elist$risklevel_event))), 
                options = list(
                  `actions-box` = TRUE, 
                  size = 12
                ),
                selected = NULL,
                multiple = TRUE
              ),
              checkboxInput("resultingCaseOnlyUi", i18n$t("Only chains with resulting cases ?"), TRUE),
              checkboxInput("excludeHealthyEventPartUi", i18n$t("Exclude healthy event participant ?"), FALSE),
              checkboxInput("activeEventsOnlyUi", i18n$t("Only chains with active events ?"), FALSE),
              checkboxInput("IgraphLayoutUi", i18n$t("Fast and fixed visualization ?"), TRUE),
              checkboxInput("visNetworkDiagramUi", i18n$t("Visualize network diagram ?"), TRUE),
              selectizeInput(
                inputId = "visSelectedChainsUi"
                , label = h5(i18n$t("Source infector node IDs (comma delimited)"))
                , choices = NULL
                , multiple = TRUE
                , options = list(create = TRUE)
              ),
              numericInput("nodeDegreeMinUi", h5(i18n$t("Minimum source infector node contact")), value = 1, min = 1),
              textInput("visSingleNodeUi", label = h5(i18n$t("Only contacts of this ID")),
                        value = "", placeholder = "Enter uuid of node ..."),
              
              # adding action button to apply filters
              actionButton(inputId = "transChainAction", label = i18n$t("Apply changes"), icon =  icon("running"),
                           class = "btn-primary", width = '65%'), #  class = "btn-primary" for normal size icon, ref: https://www.jquery-az.com/boots/demo.php?ex=12.0_1
              #span(tags$i(h6("Click this button to update the output displayed on this dashboard each time you modify the filters.")), style="color:#045a8d"),

              # add logout button to UI
              #shinyauthr::logoutUI(id = "logout"),
              #br(),
              #hr(),
              #h6("Powered by:"),
              # tags$img(src = "HZI_Logo.jpg", height = 50, width = 200) #addtiing HZI logo to sidebar panel
              width = 2),
            #), #end of div to activate option to dis sidebar panel 
            
            mainPanel(
              width = 10,
              # add login panel UI function
              #shinyauthr::loginUI(id = "login", title = "Please authenticate to begin analysis. Default credential is
              #                                 username: ars password: sormas-stats"),
              # add logout button to UI
              # div(class = "pull-right", shinyauthr::logoutUI(id = "logout")), # can also be "pull-left" or "pull-middle"
              
              #actionButton("toggleSidebar", "Toggle sidebar"), # removed from app
              fluidRow(
                visNetworkOutput("transChain", width = "100%", height = "100vh"), width=12 )
              ,
              hr(style = "border-color: #cbcbcb;"),
              
              fluidRow(
                column(width = 6, align = "left",
                       box(
                         h4(helpText(i18n$t("Source infector node ID. Use this ID to extract a single chain"),  )),
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
                column(width = 6, align = "left",
                       p(i18n$t('* Zoom in the network diagram to see the contact categories and IDs for person and event nodes'), style = "font-size: 95%"),
                       p(i18n$t('* High risk means Type of contact is Face_to_face_long or Medical_unsave or Touched_fluid or Touched_cloth or Physical_contact else Low risk'), style = "font-size: 95%"),
                       p(i18n$t('* Right click on the network diagram or legend to download or copy.'), style = "font-size: 95%"),
                       p(i18n$t('* The filters are based on complete data. Entities with missing values for the variable used for filtering are not considered'), style = "font-size: 95%"),
                       p(i18n$t('* The data used to generate indicators and figures in this app were obtained from'), tags$a(href = "https://demo.sormas.org/sormas-ui/", 'SORMAS', target = '_blank'), '.', style = "font-size: 95%")
                )
              )
            ) # end of main panel
            
          ) 
          ,
          # network parameters
          wellPanel(style = "background: white", 
                    h4(helpText(i18n$t("Summary indicators"))),
                    fluidRow(
                      column(2, infoBoxOutput("totalEdges", width = 12)),
                      column(2, infoBoxOutput("totalNodes", width = 12)),
                      column(2, infoBoxOutput("totalEventNodes", width = 12)),
                      column(2, infoBoxOutput("totalReultingcasesEdges", width = 12)),
                      column(2, infoBoxOutput("transChainSumUI", width = 12)),
                      column(2, infoBoxOutput("diameterDirected", width = 12))
                    ),
                    fluidRow(
                      column(2, infoBoxOutput("totalPersonNodes", width = 12)),
                      column(2, infoBoxOutput("totalContactEpPersonNodes", width = 12)),
                      column(2, infoBoxOutput("totalInfectorInfecteePair", width = 12)),
                      column(2, infoBoxOutput("totalReultingcasesNodes", width = 12)),
                      column(2, infoBoxOutput("propContactEpPersonConverted", width = 12)),
                      column(2, infoBoxOutput("diameterUndirected", width = 12))
                    ),
                    
                    fluidRow(
                      column(2, infoBoxOutput("totInfectorCaseEventNodes", width = 12)),
                      column(2, infoBoxOutput("totSourceInfectorCaseEventNodes", width = 12)),
                      column(2, infoBoxOutput("propInfectorCaseEventNodes", width = 12)),
                      column(2, infoBoxOutput("nodeVMR", width = 12)),
                      column(2, infoBoxOutput("edgeDensity", width = 12)),
                      column(2, infoBoxOutput("transitivityScore", width = 12))
                      
                    )
          ),
          
          fluidRow( width = 12,
                    column(2,
                           wellPanel(
                             h4(helpText(i18n$t("Total node counts by classification"))),
                             div( DT::dataTableOutput("nodeClassificationCountTable", width = "100%", height = "auto"), style = "font-size: 100%; width: 100%")
                           )
                    ),
                    column(5,
                           wellPanel(
                             h4(helpText(i18n$t("Histogram of betweeness score"))),
                             #div( DT::dataTableOutput("networkParameter2Table")  , style = "font-size: 100%; width: 100%" )
                             fluidRow(
                               width = 12,
                               plotOutput("nodeBetweenessHist", width = "100%", height = "40vh")
                             )
                             ,
                             fluidRow(
                               width = 12,
                               box(
                                 h4(helpText(i18n$t("Summary of node betweeness"))),
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
                             h4(helpText(i18n$t("Histogram of number of contacts per node (Node degree)"))),
                             fluidRow(
                               width = 12,
                               plotOutput("nodeDegreeHist", width = "100%", height = "40vh")
                             ),
                             fluidRow(
                               width = 12,
                               box(
                                 h4(helpText(i18n$t("Summary of number of contacts per node (Node degree)"))),
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
          
)
