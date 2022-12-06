###### Case data analysis ##########
# This sub section of the ui.r file renders the case data analysis tab
# All front-end methods related to this tab should be added in this file
tabPanel(i18n$t("Case data analysis"), icon = icon("procedures"),
sidebarLayout(
 sidebarPanel( 
   span(tags$i(h5(i18n$t("Please select filter options and click on `Apply changes` to run analyses."))), style="color:#045a8d"),
   actionButton(inputId = "caseDataAnalysisAction", label = i18n$t("Apply changes"), icon =  icon("running"),
                class = "btn-primary", width = '55%'),
   hr(),
   conditionalPanel(condition = "input.tabs1==0",
                    radioButtons("caseDashboardIndicatorTypeUi",i18n$t("Indicator type"),  
                                 choices = c("Count"), #,"Proportion"),
                                 selected = c("Count")) 
   ),
   conditionalPanel(condition = "input.tabs1==1",
                    radioButtons("timeUnitEpicurveUi",i18n$t("Choose"),  choices = c("Day","Epi-week", "Month"),selected = c("Epi-week"))
   ),
   conditionalPanel(condition = "input.tabs1==2",
                    radioButtons("timeUnitUi", i18n$t("Choose"),  choices = c("Day","Epi-week", "Month"),selected = c("Epi-week")),
                    checkboxInput(inputId = "cumUi", label = i18n$t("Cumulative cases"), value =F),
                    checkboxInput(inputId = "byRegiontimeUnitUi", label = i18n$t("Cases by region"), value =F),
                    checkboxInput(inputId = "cumRegionUi", label = i18n$t("Cumulative cases by region"), value =F)
   ),
   conditionalPanel(condition = "input.tabs1==3", h4(" "),
                    radioButtons("sexCategoryUi", i18n$t("Choose sex category"),  choices = c("Male X Female","Male X Others", "Female X Others"),selected = c("Male X Female"))
   ),
   conditionalPanel(condition = "input.tabs1==4",
                    radioButtons("caseMapshapesUi", "Map shapes",  choices = c("By region","By district"),selected = c("By region")),
                    radioButtons("caseIndicatorTypeMapUi","Indicator type",  choices = c("Count", "Incidence proportion / 100,000"),selected = c("Count")), 
                    #  the corresponding back-end method for "Incidence proportion / 100,000" has not been implemented, this is just a place holder
                    # A possible option is to add indicator type as a parameter to the regionMapPlot and districtMapPlot functions + option to include population data
                    # filter map by Region of case
                    # Only shapes of the selected regions would be plotted
                    pickerInput(
                      inputId = "regionCaseMapUi",
                      label = i18n$t('Choose region to plot shapes'),
                      choices = sort(levels(as.factor(casePersonRegionDist$region_name))),
                      options = list(`actions-box` = TRUE, size = 12),
                      selected = NULL,multiple = TRUE)
   ),
   conditionalPanel(condition = "input.tabs1==5",
                    # serial interval distribution to use in estimating Rt
                    pickerInput(
                      inputId = "si_rt_UI",
                      label = h5(i18n$t('Choose distribution for SI')),
                      choices = c( "Gamma", "Lognormal", "Weibull" ), # dist = "G" (Gamma), "W" (Weibull), "L" (Lognormal) 
                      options = list(
                        `actions-box` = TRUE,
                        title = i18n$t("Select distribution for SI"),
                        header = i18n$t("Only select if SI estimation method is parametric"),
                        maxOptions = 1,
                        size = 12
                      ),
                      selected = "Gamma",
                      multiple = FALSE
                    ),
                    radioButtons("rtMethodUi", h5(i18n$t("SI estimation method")),  choices = c("Parametric distribution","Transmission data"),
                                 selected = c("Parametric distribution")),
                    # Only show mean and sd when method == parametric
                    # ref: https://mran.microsoft.com/snapshot/2015-06-24/web/packages/shinyjs/README.html
                    shinyjs::hidden(
                      div(id = "showMeanSdSIUI",
                          numericInput("mean_siUI", label = h5(i18n$t("Specify SI mean")), value = 5.2, min = 1), # Mean SI must be > 1 for parametric distribution
                          numericInput("std_siUI", label = h5(i18n$t("Specify SI Std Dev")), value = 2.3, min =0.01 )
                      )
                    ), 
                    sliderInput("siUi", label = h5(i18n$t("Choose maximum value for SI")), min = 1, 
                                max = 30, step = 1, value = 14),
                    radioButtons("rsiUi", h5(i18n$t("Ploting parameters")),  choices = c("all","R","SI"), selected = c("R"), inline = TRUE),
                    checkboxInput("rtLegandUi", label= h5(i18n$t("Show legend of estimated Rt plot?")), value = FALSE),
                    span(tags$i(h5(i18n$t("This section estimates the time dependent reproduction number Rt, t = week. The data used are the case-based incidence data and transmission network data."))), style="color:#045a8d"),
                    span(tags$i(h5(i18n$t("We begin by estimating the serial interval (SI) distribution and use it to estimate Rt. The SI distribution can be estimated parametrically by specifying the mean and std for SI OR by using the transmission network data."))), style="color:#045a8d"),
                    span(tags$i(h5(i18n$t("For `SI estimation method` using transmission data, the analysis is based on 1500 MCMC iterations and may take some time, repenting on your data size."))), 
                         style="color:#045a8d")
                    
   ),
   conditionalPanel(condition = "input.tabs1==6",
                    radioButtons("caseByRegionIndicatorTypeUi",i18n$t("Indicator type"),  
                                 choices = c("Count","Proportion"),
                                 selected = c("Count")) 
   ),
   conditionalPanel(condition = "input.tabs1==7",
                    sliderInput("serialIntervalRangeUi", label = h5(i18n$t("Choose the serial interval range")), min = -30, 
                                max = 50, step = 1, value = c(1, 30)),
                    pickerInput(
                      inputId = "siDistMethodUi", 
                      label = h5(i18n$t('Choose distribution with best fit')),
                      choices = c("Lognormal","Normal", "Weibull", "Gamma"),
                      selected = "Lognormal",
                      multiple = FALSE,
                      options = pickerOptions(
                        actionsBox = TRUE,
                        header = i18n$t("Distribution with best fit has smallest AIC or BIC"),
                        #title = "Only one distribution can be selected",
                        maxOptions = 2)
                    ),
                    numericInput("niter_SI_UI", label = h5(i18n$t("Specify number of bootstrap iteration")), value = 100),
                    checkboxInput("showModelDiagnosticsPanel_SI",  i18n$t("Show model diagnostics plots"), FALSE),
                    br(),
                    span(tags$i(h5(i18n$t("This section estimates the serial interval (SI). The data used here is the case-based data of infector-infertee pairs extracted from the transmission network data."))), style="color:#045a8d"),
                    span(tags$i(h5(i18n$t("Different distributions are fitted to the observed SI. After choosing the distribution with best fit based on smallest AIC, the app then estimate the mean SI and 95% CI using bootstrap."))), style="color:#045a8d")
   ), 
   conditionalPanel(condition = "input.tabs1==8",
                    numericInput("polyDegree_RtK_UI", label = h5(i18n$t("Specify degree of the polynomial curve")),  min = 1, max=9, value = NA),
                    br(),
                    span(tags$i(h5(i18n$t("This section estimates the superspreading parameter (k) and effective reproduction number (R). The data used here is the case-based infector-infertee pairs extracted from the transmission network data."))), style="color:#045a8d"),
                    span(tags$i(h5(i18n$t("A negative binomial distribution (NB) is fitted to the offspring distribution (number of infector per infectee). The mean of the NB estimates the effective reproductive number (R) while the dispersion parameter estimate the superspreading parameter (k). The 95% confidence interval is estimated using `confint` function of the R `stats` package."))), style="color:#045a8d")
   ),
   width = 2),
 mainPanel(
   fluidRow(
     column(2, 
            pickerInput("diseaseCaseUi", i18n$t("Disease"),
                        choices = sort(levels(as.factor(casePersonRegionDist$disease))),
                        selected = c("CORONAVIRUS"),
                        multiple = FALSE)
     ),
     column(2,
            dateRangeInput("reportdateCaseUi",i18n$t("Report date (dd-mm-yyyy)"), start = Sys.Date() - delay_default_UI, end = Sys.Date(), min = NULL,
                           max = NULL, format = "dd-mm-yyyy", startview = "month",
                           weekstart = 0, language = "en", separator = " to ", width = NULL,
                           autoclose = TRUE)
     ),
     column(2, 
            pickerInput(
              inputId = "regionCaseUi",
              label = i18n$t('Region of case'),
              choices = sort(levels(as.factor(casePersonRegionDist$region_name))),
              options = list(
                `actions-box` = TRUE,
                size = 12
              ),
              selected = NULL,  multiple = TRUE )
     ),
     column(2, uiOutput('pickerInputdistrictCaseUi') # ui element for district
     ),
     # column(2, pickerInput(
     #   inputId = "classificationCaseUi",
     #   label = i18n$t('Classification of case'),
     #   choices = sort(levels(as.factor(casePersonRegionDist$caseclassification))),
     #   options = list(
     #     `actions-box` = TRUE,
     #     size = 12
     #   ),
     #   selected = NULL, multiple = TRUE
     # )),
     
     # column(2, pickerInput(
     #   inputId = "facilityCaseUi",
     #   label = i18n$t('Facility id of case'),
     #   choices = sort(levels(as.factor(casePersonRegionDist$healthfacility_id))),
     #   options = list(
     #     `actions-box` = TRUE,
     #     size = 12
     #   ),
     #   selected = NULL, multiple = TRUE
     # ))
   )
   ,
  tabsetPanel(id="tabs1",
  tabPanel(i18n$t("Case dashboard"),  value = 0,
           wellPanel(style = "background: white",
                     h4(i18n$t('Case classification')),
                     fluidRow(width = 12,
                              infoBoxOutput("allCase", width = 2),
                              infoBoxOutput("UnclassifiedCases",width = 2),
                              infoBoxOutput("suspectedCases", width = 2),
                              infoBoxOutput("probableCases",width = 2),
                              infoBoxOutput("confirmedCases", width = 2)
                     ),
                     h4(i18n$t('Case quarantine')),
                     fluidRow(width=12,
                              infoBoxOutput("institutionalQCases",width = 2),
                              infoBoxOutput("homeQCases", width = 2),
                              infoBoxOutput("otherQCases", width = 2),
                              infoBoxOutput("noneQCases", width = 2),
                              infoBoxOutput("unknownQCases", width = 2),
                              infoBoxOutput("MissingQCases", width = 2)
                     ),
                     h4(i18n$t('Case gender')),
                     fluidRow(width=12,
                              infoBoxOutput("maleSexCases",width = 2),
                              infoBoxOutput("femaleSexCases", width = 2),
                              infoBoxOutput("otherSexCases", width = 2),
                              infoBoxOutput("unknownSexCases", width = 2),
                              infoBoxOutput("MissingSexCases", width = 2)
                     ),
                     h4(i18n$t('Case age')),
                     fluidRow(width=12,
                              infoBoxOutput("minAgeCases",width = 2),
                              infoBoxOutput("medianAgeCases", width = 2),
                              infoBoxOutput("meanAgeCases", width = 2),
                              infoBoxOutput("maxAgeCases", width = 2),
                              infoBoxOutput("missingAgeCases", width = 2)
                     ),
                     h4(i18n$t('Case outcome')),
                     fluidRow(width=12,
                              infoBoxOutput("diseasedCases",width = 2),
                              infoBoxOutput("recoveredCases", width = 2),
                              infoBoxOutput("unknownOutcomeCases", width = 2),
                              infoBoxOutput("noOutcomeCases",width = 2)
                     ),
                     h4(i18n$t('Case occupation')),
                     fluidRow(width=12,
                              infoBoxOutput("healthWorkerOcupCases",width = 2),
                              infoBoxOutput("otherOcupCases", width = 2),
                              infoBoxOutput("MissingOcupCases", width = 2)
                     ),
                     h4(i18n$t('Case Origin')),
                     fluidRow(width=12,
                              infoBoxOutput("incountryCases",width = 2),
                              infoBoxOutput("importedCases", width = 2)
                     )
           ),
           tags$br(), #tags$br(),
           i18n$t("You can request more indicators you want to see here"))
  ,
  tabPanel(i18n$t("Cases by region"), value = 6,
           fluidRow(
             column(12, DT::dataTableOutput("caseCountbyRegionTable"))    
           ),
           h4(strong(i18n$t("Meaning of column headers"))),
           h5(strong(i18n$t("Name:")), i18n$t("Name of administrative unit"), strong("Total:"), i18n$t("Total number of cases reported within the time interval specified in the filter"), strong("Total_last24hrs:"), i18n$t("Total number of cases reported in the last 24 hrs"), 
              strong("Confirmed:"),  i18n$t("Total number of confirmed cases"), strong("Probable:"), i18n$t("Total number of probable cases"),  strong("Unclassified:"), i18n$t("Total number of unclassified cases"),
              strong("Suspected:"), i18n$t("Total number of suspected cases"), strong("Deseases:"), i18n$t("Total number of death cases"), strong("No_Outcome:"), i18n$t("Total number of cases  with no outcome"),
              strong("Recovered:"), i18n$t("Total number of recovered cases"), strong("Unk_Outcome:"), i18n$t("Total number of cases with unknown outcome"), strong("In_Country:"), i18n$t("Total number of cases resulting from local transmission"),
              strong("Imported:"), i18n$t("Total number of imported cases"), strong("Home_Q:"), i18n$t("Total number of cases quarantine at home"), strong("Institutional_Q:"), i18n$t("Total number of cases quarantine in an institution"),
              strong("No_Q:"), i18n$t("Total number of cases that are not on quarantine"), strong("Other_Q:"), i18n$t("Total number of cases quarantine at other places"), strong("Unk_Q:"), i18n$t("Total number of cases with unknown quarantine")
           )
  )
  ,
  tabPanel(i18n$t("Epidemic curve"), value = 1, plotlyOutput("caseEpicurvePlot", width = "100%", height = "80vh"))
  ,
  tabPanel(i18n$t("Time series plot"), value = 2, plotlyOutput("caseTimeSeriesPlot", width = "100%", height = "80vh"))
  ,
  tabPanel(i18n$t("Case Pyramid"),  value = 3,
           wellPanel(
             h4(helpText(i18n$t("Case pyramid by Sex and Age"))), 
             plotlyOutput("casePyramidPlot", width = "80%", height = "75vh"), # height = "auto"
             style = "background: white"
           ) 
  )
  #,
 # tabPanel("Administrative map", value = 4, plotOutput("regionMapCaseCount", width = "100%", height = "80vh")) # commented because the development of this tab is not complete
  ,
  tabPanel(i18n$t("Serial interval analysis"), value = 7,
           fluidRow(width = 10,
                    column(12,
                            wellPanel(
                              h4(helpText(i18n$t("Model selection: Goodness-of-fit criteria and statistics. Model with smallest values correspond to distribution with best fit." ))),
                              div( DT::dataTableOutput("si_model_fitTable", width = "100%", height = "auto"), 
                                   style = "font-size: 100%; width: 100%")
                            )
                     )
           ),
           fluidRow(width = 10,                                                         
                     column(6,
                            conditionalPanel(condition = 'input.showModelDiagnosticsPanel_SI',
                                             wellPanel(
                                               h4(helpText(i18n$t("Empirical and theoretical CDFs"))),
                                               fluidRow(
                                                 width = 12,
                                                 plotOutput("SI_model_cdf_plot", width = "100%", height = "60vh")
                                               )
                                             )
                            )
                     ),
                     column(6,
                            conditionalPanel(condition = 'input.showModelDiagnosticsPanel_SI',
                                             wellPanel(
                                               h4(helpText(i18n$t("Q-Q plot"))),
                                               fluidRow(
                                                 width = 12,
                                                 plotOutput("SI_model_qq_plot", width = "100%", height = "60vh")
                                               )
                                             )
                            )
                     )
           ), 
           fluidRow( width = 10,
                     column(6,
                            wellPanel(
                              h4(helpText(i18n$t("Empirical and theoretical density"))),
                              fluidRow(
                                width = 12,
                                plotOutput("SI_hist_model_plot", width = "100%", height = "60vh")
                              )
                            )
                     ),
                     column(6,
                            wellPanel(
                              fluidRow(
                                width = 12,
                                box(
                                  h4(helpText(i18n$t("Summary statistics for observed serial interval"))),
                                  status ="primary", 
                                  solidHeader = FALSE,
                                  collapsible = TRUE,
                                  collapsed = FALSE,
                                  width = 15,
                                  div(DT::dataTableOutput("si_summaryTable", width = "100%", height = "auto"), style = "font-size: 100%; width: 100%"),
                                  h5(helpText(i18n$t("n_value <= 0: number of records with negative or zero serial interval. This corresponds to asymptomatic transmission.")))
                                )
                              ),
                              fluidRow(
                                width = 12,
                                box(
                                  h4(helpText(i18n$t("Parameter estimate and 95% CI of distribtion with best fit to the data"))),
                                  status ="primary", 
                                  solidHeader = FALSE,
                                  collapsible = TRUE,
                                  collapsed = FALSE,
                                  width = 15,
                                  div( DT::dataTableOutput("SI_estimate_table", width = "100%", height = "auto"), style = "font-size: 100%; width: 100%"),
                                  h5(helpText("These estimates were estimated using boostrap based on the specified number of iteration specified in the filter.
                                                           For efficient estimates, set the specified number of iterations to be > 1000. This may takes some time for bootsrap
                                                          to complete."))
                                )
                              ),
                              fluidRow(
                                width = 12,
                                box(
                                  h4(helpText(i18n$t("Estimate of mean serial interval"))),
                                  status ="primary", 
                                  solidHeader = FALSE,
                                  collapsible = TRUE,
                                  collapsed = FALSE,
                                  width = 15,
                                  div( DT::dataTableOutput("si_mean_CI_table", width = "100%", height = "auto"), style = "font-size: 100%; width: 100%")
                                )
                              )
                            )
                     )
           ), 
           fluidRow( width = 10,
                     column(12,
                            wellPanel(
                              h4(helpText(i18n$t("Serial interval and fitted distribution for model with best fit to the data"))),
                              fluidRow(
                                width = 12,
                                plotOutput("distribution_SI_plot", width = "100%", height = "60vh")
                              )
                            )
                     )
           )
  ),
  tabPanel(i18n$t("Dispersion analysis"), value = 8,  
           fluidRow(width = 10,                                                        
                     column(6,
                            wellPanel(
                              h4(helpText(i18n$t("Distributions of number of infectee per infector (offspring distribution)"))),
                              fluidRow(
                                width = 12,
                                plotOutput("distribution_k_plot", width = "100%", height = "60vh")
                              )
                            )
                     ),
                     column(6,
                            wellPanel(
                              fluidRow(
                                width = 12,
                                box(
                                  h4(helpText(i18n$t("Summary statistics for observed offspring distribution"))),
                                  status ="primary",  
                                  solidHeader = FALSE,
                                  collapsible = TRUE,
                                  collapsed = FALSE,
                                  width = 15,
                                  div( DT::dataTableOutput("nodedegree_summaryTable", width = "100%", height = "auto"), style = "font-size: 100%; width: 100%"),
                                  h5(helpText(i18n$t("n_value <= 0: number of terminal infectee nodes. This corresponds to cases that were infected but did not infect further cases.")))
                                )
                              ),
                              fluidRow(
                                width = 12,
                                box(
                                  h4(helpText(i18n$t("Estimates of reproduction number (R) and dispersion parameter (k) using NB distribution"))),
                                  status ="primary", 
                                  solidHeader = FALSE,
                                  collapsible = TRUE,
                                  collapsed = FALSE,
                                  width = 15,
                                  div( DT::dataTableOutput("k_estimate_table", width = "100%", height = "auto"), style = "font-size: 100%; width: 100%"),
                                  h5(helpText(i18n$t("SD: standard deviation, CI: confidence interval, a lower dispersion parameter k indicates  higher transmission heterogeneity, i.e. more transmissions resulted from a small number of people.")))
                                )
                              )
                            )
                     )
           ) 
  ),
  tabPanel(i18n$t("Reproduction number (Rt)"), value = 5, 
           wellPanel(
             h4(helpText(i18n$t("Estimate of time dependent reproduction number Rt"))) ,
             fluidRow(
               width = 12,
               plotOutput("rtPlot", width = "90%", height = "80vh"),
               style = "background: white"
             ),
             fluidRow(
               width = 12,
               box(
                 h4(helpText(i18n$t("Summary of estimated mean time dependent reproduction number"))),
                 status ="primary", 
                 solidHeader = FALSE,
                 collapsible = TRUE,
                 collapsed = FALSE,
                 width = 12,
                 div( DT::dataTableOutput("rtSummary_table", width = "50%", height = "auto"), style = "font-size: 100%; width: 100%"),
               )
             )
           )
  )
   ),
   width = 10)                       
)
)

