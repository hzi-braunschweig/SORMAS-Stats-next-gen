###### Contact data analysis ##########
# This sub section of the ui.r file renders the contact data analysis tab
# All front-end methods related to this tab should be added in this file
tabPanel("Contact data analysis", icon = icon("handshake"),
sidebarLayout(position = "left",
    sidebarPanel( width = 2,
    span(tags$i(h5("Please select filter options and click on `Apply changes` to run analyses.")), style="color:#045a8d"),
    
    actionButton(inputId = "contactDataAnalysisAction", label = "Apply changes", icon =  icon("running"),
                 class = "btn-primary", width = '55%'),
    hr(),
    pickerInput("diseaseContactUi", "Disease", 
                choices = c("CORONAVIRUS", "LASSA","MONKEYPOX", "LASSA", "CSM","EVD","NEW_INFLUENZA", "PLAGUE",
                            "UNDEFINED","UNSPECIFIED_VHF","MEASLES","OTHER"), 
                selected = c("CORONAVIRUS"),
                multiple = FALSE),
    dateRangeInput("reportdateContactUi","Report date (dd-mm-yyyy)" , start = Sys.Date() - delay_default_UI, end = Sys.Date(), min = NULL,
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
    mainPanel(width = 10,
              uiOutput("contact_analysis_output")
    )
)
)