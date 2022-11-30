shinyUI(bootstrapPage(
useShinyjs(),
tags$style("@import url(https://use.fontawesome.com/releases/v5.7.2/css/all.css);"),
navbarPage(
theme = shinytheme("cerulean"),  # to change the theme, other options are: darkly, flatly
collapsible = TRUE,
inverse = FALSE,
title = "SORMAS-Stats", 
id="nav",
##### Transmission network ######## 
base::source(file.path("./ui_components","ui_transmission_network.R"), local = TRUE)$value,
     
##### Case data analysis ######## 
base::source(file.path("./ui_components","ui_case_data_analysis.R"), local = TRUE)$value,

#### Event data analysis ########
base::source(file.path("./ui_components","ui_event_data_analysis.R"), local = TRUE)$value,

##### Contact data analysis ########
base::source(file.path("./ui_components","ui_contact_data_analysis.R"), local = TRUE)$value,

# Sample data analysis ########
base::source(file.path("./ui_components","ui_sample_data_analysis.R"), local = TRUE)$value,

# Model specification ----
tabPanel("Model specification",
         icon = icon("book"),
         radioButtons(inputId = "language", 
                      label = "",
                      choiceValues = c("en", "fr", "de"),
                      choiceNames = c("English", "Français", "Deutsch"),
                      selected = "en",
                      inline = TRUE),
uiOutput("model_specificationUI")
),

# About ------
tabPanel(title = "About", 
fluidRow( 
dashboardPage( # the use of shiny dashboard is to make sure that all icons are fine, do not remove or deactivate this tab.
  dashboardHeader( ),
  dashboardSidebar(disable = TRUE),
   dashboardBody(
h4(strong("About SORMAS and SORMAS-Stats")),
h5("The Surveillance, Outbreak Response Management and Analysis System (SORMAS) is an open-source mHealth (mobile health) system that organises and facilitates infectious
disease control and outbreak management procedures in addition to disease surveillance and epidemiologic analysis for all administrative
levels of a public health system. SORMAS includes specific interfaces for 12 users (e.g., laboratorian, contact tracing officer, epidemiologist), 
disease-specific process modules for 21 epidemic-prone diseases, and a customizable process module for unforeseen emerging diseases and a COVID-19 module, 
which we developed to support countries in the outbreak response. Users can operate SORMAS on the web or mobile app and bidirectionally synchronized with a central server via mobile telecommunication networks."),
     
 h5("SORMAS-Stats contain functions to analyze and visualize surveillance data collected by SORMAS."), 
     h4(strong("Users, diseases and control measures in SORMAS")),
     img(src = "process-flow-sormas.png", width = "800", height = "900"),  img(src = "sormas_diseases.png", width = "800", height = "900"),
#   h5(" Add more txt  and gifures here...")

 )
)
)
),
# Footer ------
tags$footer(
hr(style = "border-color: #cbcbcb;"),
fluidRow(
  column(9,
         p(i18n$t('The data used in this app were obtained from'), tags$a(href = "https://demo.sormas.org/sormas-ui/", 'SORMAS', target = '_blank'), i18n$t('. Messages in red of the form Error: ... means there is no available data to analyse. Please change your filter option or inform your server admin'), style = "font-size: 85%"),
         p(i18n$t("App created by the "), tags$a(href = "https://github.com/hzi-braunschweig/SORMAS-Stats-next-gen", 'SORMAS-Stats Team', target = '_blank'), style = "font-size: 85%"),
         p(i18n$t("Find all the source codes on Github:"), tags$a(href = "https://github.com/hzi-braunschweig/SORMAS-Stats-next-gen", tags$i(class = 'fa fa-github', style = 'color:#5000a5'), target = '_blank'), style = "font-size: 85%"),
         p(i18n$t("Want to contribute? Have a question? Identify a bug or want to make a request? Open a discussion on "), tags$a(href = "https://github.com/hzi-braunschweig/SORMAS-Stats-next-gen/discussions", tags$i(class = 'fa fa-github', style = 'color:#5000a5'), target = '_blank'), style = "font-size: 85%"),
         p(i18n$t("This app contains data reported in SORMAS between:"), fromDate, '--', Sys.Date(),  style = 'font-size:85%')  ) #  tags$em(toDate), "Last updated: June 27, 2022", toDate
  ,
  column(3, align = "right",
         p(i18n$t('Powered by:'), tags$a(href = " ", target = '_blank'), '', style = "font-size: 85%"),
         p(tags$a(href = "https://www.helmholtz-hzi.de/en/research/research-topics/bacterial-and-viral-pathogens/epidemiology/our-research/", 'HZI', target = '_blank'), '', style = "font-size: 85%"),
         p(tags$a(href = "https://www.bourgogne-franche-comte.ars.sante.fr/", 'ARS-BFC', target = '_blank'), '', style = "font-size: 85%"),
         p(tags$a(href = "https://www.vitagroup.ag/de_DE/Ueber-uns/symeda", 'vitagroup', target = '_blank'), '', style = "font-size: 85%"),
         p(tags$a(href = "https://www.giz.de/en/html/index.html", 'GIZ', target = '_blank'), '', style = "font-size: 85%")
  )
  )
)
)
))