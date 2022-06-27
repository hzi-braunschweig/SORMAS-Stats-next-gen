###### Sample data analysis ##########
# This sub section of the ui.r file renders the sample data analysis tab
# All front-end methods related to this tab should be added in this file
tabPanel("Sample data analysis",
icon = icon("vial"),
sidebarLayout(position = "left",
sidebarPanel(width = 2,
             span(tags$i(h5("Please select filter options and click on `Apply changes` to run analysis.")), style="color:#045a8d"),
             span(tags$i(h5("The date filter uses the report date of sample, not the report date of the essociated entity (case, contact, event participant, ...)")), style="color:#045a8d"),
             actionButton(inputId = "sampleDataAnalysisAction", label = "Apply changes", icon =  icon("running"),
                          class = "btn-primary", width = '55%'),
             hr(),
             pickerInput(
               inputId = "samplepurposeUi",
               label = 'Choose sample pursope',
               choices = sort(levels(as.factor(sample_table$samplepurpose))),
               options = list(`actions-box` = TRUE, size = 12),
               selected = NULL,multiple = TRUE),
             uiOutput("sample_cuctom_indicator_filter")
), 
mainPanel(width = 10,
          # The top filter that appears on the top of all tabs
          fluidRow(
            # filter by disease ----
            column(2,
                   pickerInput("diseaseSampleUi", "Disease", choices = sort(levels(as.factor(sample_table$disease_sample))),
                               selected = c("CORONAVIRUS"), multiple = FALSE)
            ),
            # filter by date of sample report ----
            column(2, 
                   dateRangeInput("reportdateSampleUi","Report date (dd-mm-yyyy)" , start = Sys.Date() - delay_default_UI, end = Sys.Date(), min = NULL,
                                  max = NULL, format = "dd-mm-yyyy", startview = "month",
                                  weekstart = 0, language = "en", separator = " to ", width = NULL,
                                  autoclose = TRUE)
            ),
            # filter by region of sample ----
            column(2,
                   pickerInput(
                     inputId = "regionSampleUi",
                     label = 'Region of sample',
                     choices = sort(levels(as.factor(sample_table$region_name))),
                     options = list(
                       `actions-box` = TRUE, 
                       size = 12
                     ),
                     selected = NULL,
                     multiple = TRUE
                   )
            ),
            # filter by district of sample ----
            column(2, 
                   uiOutput('pickerInputdistrictSampleUi')
            ),
            # filter by indicator type of sample ----
            column(2,
                   radioButtons("sampleIndicatorTypeUi","Indicator type",  choices = c("Count","Proportion"),selected = c("Count"), inline = TRUE)
            )
          ),
          # outputting the whole sample ui element built from the server side ------
          uiOutput("sample_analysis_output")
) # closing main panel                       
)  # closing sidbarlayout 
)
