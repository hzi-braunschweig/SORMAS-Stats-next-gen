# Minimal webapp to compute and visualize epidemic risks based on
#
# df = mtcars
# names(df)
# library(tidyverse)
#
# df %>%
#   group_by(cyl, gear)%>%
#   summarise(AverageMPG = mean(mpg))





source("data-raw/configuration.R")
source("R/data-preparation.R")
source("R/risk-computation.R")
source("R/display-output.R")

################################ read me #######################################


# replaced epi_data_raw with reactive_epi_data_raw in the server
## this was done to apply location filters on the raw data

# replaced epi_indicators with reactive_epi_indicators in the server

################################################################################


## package added
library(tidyverse)
library(shinydashboard)
library(shinyWidgets)
library(ggeasy)
library(knitr)
## moved data above so we can get list of countries
## then apply location filters on it
epi_data_raw <- tibble::as_tibble(
  read.csv2(
    paste0(epi_data_path, "/", epi_data_file_name),
    skip = 1,
    stringsAsFactors=FALSE,
    fileEncoding="latin1"

  )
)

## gets locations data from the raw data
LocationData = epi_data_raw %>%
  select(person.address.country,responsibleRegion,responsibleDistrict,person.address.city)%>%
  distinct()

## changing city name to district cause thats how its been used in the previous code
names(LocationData)[4] = "district"

Flag = 1

epi_data_raw$responsibleRegion = paste(epi_data_raw$responsibleRegion)

ui <- shinydashboard::dashboardPage(

  # Header

  shinydashboard::dashboardHeader(
    title = "IDIS: Risk Assessment Module",
    disable = F,
    titleWidth = 450,
    tags$li(
      class = 'dropdown',
      conditionalPanel(
        condition = '$("html").hasClass("shiny-busy")',
        'loading...'
      )
    )
  ),

  # Sidebar with controllers

  shinydashboard::dashboardSidebar(
    tags$head(
      tags$style(
        HTML(".treeview-menu {padding-left: 0px;}")
      )
    ),

    ## collapsible location filters
    menuItem(
      #("Location"),
      # selectInput(
      #   "country",
      #   "Country",
      #   choices = unique(epi_data_raw$person.address.country),
      #   selected = unique(epi_data_raw$person.address.country)[1]
     #  ),

      ## button to apply changes

      pickerInput(
        inputId = "country",
        label = "Country",
        choices = unique(epi_data_raw$person.address.country),
        #selected = unique(epi_data_raw$person.address.country),
        options = list(`actions-box` = T),
        multiple = T
      ),


      pickerInput(
        inputId = "region",
        label = "Region",
        choices = unique(epi_data_raw$responsibleRegion),
        #selected = unique(epi_data_raw$responsibleRegion)[1],
        #selected = unique(epi_data_raw$responsibleRegion),
        options = list(`actions-box` = T),
        multiple = T
      ),


      pickerInput(
        inputId = "district",
        label = "District",
        choices = unique(epi_data_raw$responsibleDistrict),
        #selected = unique(epi_data_raw$responsibleDistrict),
        options = list(`actions-box` = T),
        multiple = T
      ),
      ## is updated based on country selected
      #uiOutput("selectRegion"),
      ## is updated based on country and region selected
      #uiOutput("selectDistrict"),
      ## is updated based on country region and district selected
      # uiOutput("selectCity"),
      #actionButton("ApplyChanges", "Apply", width = "90%", style = "background-color:#22A7FF; color: white;"),

      startExpanded = TRUE
    ),


    # selectInput(
    #   "disease",
    #   "Disease",
    #   choices = diseases,
    #   selected = display_filter_default[["disease"]]
    # ),


    ## group by options
#    pickerInput("selectGroupBy", "Group By:",
#                 c("Country","Region","District","City"),
 #                "City"
 #   ),

    pickerInput(
      inputId = "disease",
      label = "Disease",
      choices = diseases,
      selected = display_filter_default[["disease"]]
    ),

    numericInput(
      "n_reference_weeks",
      "Number of reference weeks",
      value = n_reference_weeks_default,
      min = 1
    ),

    numericInput(
      "n_observation_weeks",
      "Number of observation weeks",
      value = n_observation_weeks_default,
      min = 1
    ),

    pickerInput(
      "indicator_type",
      "Indicator type",
      choices = indicator_types,
      selected = display_filter_default[["indicator_type"]]
    ),

    pickerInput(
      "risk_type",
      "Risk type",
      choices = risk_types,
      selected = display_filter_default[["risk_type"]]
    ),

  #  pickerInput("selectGroupBy", "Group By:",
   #             c("Country","Region","District","City"),
  #              "City"

  #  ),



    ## is updated based risk_types
    uiOutput("AbsoluteAnomaly"),
    pickerInput("selectGroupBy", "Group By:",
                c("Country","Region","District","City"),
                "City"

    ),
      actionButton("ApplyChanges", "Apply", width = "90%", style = "background-color:#22A7FF; color: white;"),





    width = sidebar_width
  ),

  # Main panel with outputs in different tabs

  shinydashboard::dashboardBody(
    fluidPage(
      tabsetPanel(


        type = "pills",

        tabPanel(
          "Heatmap",
          shinycssloaders::withSpinner(
            uiOutput("plot_heatmap_ui"),
            color='Green'
          )
        ),

        tabPanel(
          "Time series",
          shinycssloaders::withSpinner(
            uiOutput("plot_timeseries_ui"),
            color='Green'
          )
        ),

        tabPanel(
          "Data",
          shinycssloaders::withSpinner(
            DT::dataTableOutput("epi_risks"),
            color='Green'
          )
        ),
        ## added tab for info
        tabPanel(
          "Info",
          ##change info.rmd to get content here
          shinycssloaders::withSpinner(uiOutput("info"))


        ),

      )
    )
  )
)

server <- function(input, output, session) {

  ## ui for region
  output$selectRegion = renderUI({
    req(input$country)
    temp = epi_data_raw %>%
      filter(person.address.country %in% input$country)

    # selectInput(
    #   "region",
    #   "Region",
    #   choices = unique(temp$responsibleRegion),
    #   selected = unique(temp$responsibleRegion)[1]
    # )
    pickerInput(
      inputId = "region",
      label = "Region",
      choices = unique(temp$responsibleRegion),
      # selected = unique(temp$responsibleRegion)[1],
      selected = unique(temp$responsibleRegion),
      options = list(`actions-box` = T),
      multiple = T
    )
  })

  ## ui for district
  output$selectDistrict = renderUI({
    req(input$country)
    req(input$region)

    temp = epi_data_raw %>%
      filter(person.address.country %in% input$country)%>%
      filter(responsibleRegion %in% input$region)

    # selectInput(
    #   "district",
    #   "District",
    #   choices = unique(temp$responsibleDistrict),
    #   selected = unique(temp$responsibleDistrict)[1]
    # )
    pickerInput(
      inputId = "district",
      label = "District",
      choices = unique(temp$responsibleDistrict),
      selected = unique(temp$responsibleDistrict),
      options = list(`actions-box` = T),
      multiple = T
    )
  })

  # ## ui for city
  # output$selectCity = renderUI({
  #   req(input$country)
  #   req(input$region)
  #   req(input$district)
  #
  #   temp = epi_data_raw %>%
  #     filter(person.address.country %in% input$country)%>%
  #     filter(responsibleRegion %in% input$region)%>%
  #     filter(responsibleDistrict %in% input$district)
  #
  #   selectInput(
  #     "city",
  #     "City",
  #     choices = unique(temp$person.address.city),
  #     selected = unique(temp$person.address.city)[1]
  #   )
  # })



  # conditional filters for risk_types

  output$AbsoluteAnomaly = renderUI({
    req(input$risk_type)
    UI = list()
    if(input$risk_type == "absolute"){
      UI = append(UI,
                  list(
                    numericInput(
                      "low_mid_absolute",
                      "Absolute low-moderate threshold",
                      value = risk_thresholds_default$low_mid[
                        risk_thresholds_default$risk_type == "absolute"
                      ],
                      min = 0
                    )
                  )
      )
      UI = append(UI,
                  list(
                    numericInput(
                      "mid_high_absolute",
                      "Absolute moderate-high threshold",
                      value = risk_thresholds_default$mid_high[
                        risk_thresholds_default$risk_type == "absolute"
                      ],
                      min = 0
                    )
                  )
      )

    }
    else if(input$risk_type == "anomaly"){
      UI = append(UI,
                  list(
                    sliderInput(
                      "low_mid_anomaly",
                      "Anomaly low-moderate threshold",
                      value = risk_thresholds_default$low_mid[
                        risk_thresholds_default$risk_type == "anomaly"
                      ],
                      min = 0,
                      max = 1
                    )
                  )
      )
      UI = append(UI,
                  list(
                    sliderInput(
                      "mid_high_anomaly",
                      "Anomaly moderate-high threshold",
                      value = risk_thresholds_default$mid_high[
                        risk_thresholds_default$risk_type == "anomaly"
                      ],
                      min = 0,
                      max = 1
                    )
                  )
      )
    }

    UI

  })

  # Parameters

  display_filter <- reactive({
    list(
      select_country = input$person.address.country,
      disease = input$disease,
      indicator_type = input$indicator_type,
      risk_type = input$risk_type
    )
  })

  risk_thresholds <- reactive({

    tibble::tibble(
      disease = input$disease,
      indicator_type = input$indicator_type,
      risk_type = risk_types,
      low_mid = c(input$low_mid_absolute, input$low_mid_anomaly),
      mid_high = c(input$mid_high_absolute, input$mid_high_anomaly)
    )

  })

  # Data

  # epi_data_raw <- tibble::as_tibble(
  #   read.csv2(
  #     paste0(epi_data_path, "/", epi_data_file_name),
  #     skip = 1
  #   )
  # )
  #
  #   epi_indicators <- prepare_epi_data(epi_data_raw, indicator_types, week_types,
  #                                      case_definitions)
  #

  ## epi_data_raw to reactive_epi_data_raw so we can apply location filters

  observeEvent(input$ApplyChanges,{

    reactive_epi_data_raw = reactive({
      if(Flag == 0){
        temp = epi_data_raw %>%
          filter(person.address.country %in% isolate(input$country))%>%
          filter(responsibleRegion %in% isolate(input$region))%>%
          filter(responsibleDistrict %in% isolate(input$district))
      }
    })

  })




  reactive_epi_data_raw = eventReactive(input$ApplyChanges,{
    # if(Flag == 1){
    # req(input$country)
    # req(input$region)
    # req(input$district)
    # req(input$city)
    # Flag = 0

    temp = epi_data_raw %>%
      filter(person.address.country %in% isolate(input$country))%>%
      filter(responsibleRegion %in% isolate(input$region))%>%
      filter(responsibleDistrict %in% isolate(input$district))
    # %>%
    #   filter(person.address.city %in% input$city)
    # }
  })


  ## epi_indicators to reactive_epi_indicators so we can use reactive_epi_data_raw
  reactive_epi_indicators <- reactive({
    req(reactive_epi_data_raw())
    reactive_epi_data_raw1 <<- reactive_epi_data_raw()

    prepare_epi_data(reactive_epi_data_raw(), indicator_types, week_types,
                     case_definitions)

  })

  names(reactive_epi_indicators1)



  # calculateRiskScore = function(reactive_epi_indicators,case_weight,death_weight,hosp_weight, groupBy){
  #
  #   names(reactive_epi_indicators1)
  #
  #   Temp = merge(reactive_epi_indicators1,LocationData, by = "district")%>% distinct()
  #
  #   if(groupBy == "City"){
  #     RiskScore = Temp %>%
  #       group_by(district,week,indicator_type) %>%
  #       summarise(RS = mean(indicator_value, na.rm =T))
  #   }
  #
  #   if(groupBy == "Country"){
  #     RiskScore = Temp %>%
  #       group_by(person.address.country,week,indicator_type) %>%
  #       summarise(RS = mean(indicator_value, na.rm =T))
  #   }
  #
  #   if(groupBy == "Region"){
  #
  #     RiskScore = Temp %>%
  #       group_by(responsibleRegion,week,indicator_type) %>%
  #       summarise(RS = mean(indicator_value, na.rm =T))
  #
  #
  #   }
  #
  #   if(groupBy== "District"){
  #     RiskScore = Temp %>%
  #       group_by(responsibleDistrict,week,indicator_type) %>%
  #       summarise(RS = mean(indicator_value, na.rm =T))
  #   }
  #
  #   # unique(RiskScore$indicator_type)
  #   RiskScore$RS[RiskScore$indicator_type == "case count"] = RiskScore$RS[RiskScore$indicator_type == "case count"]* case_weight
  #   RiskScore$RS[RiskScore$indicator_type == "death count"] = RiskScore$RS[RiskScore$indicator_type == "death count"]* death_weight
  #   RiskScore$RS[RiskScore$indicator_type == "hospitalization count"] =  RiskScore$RS[RiskScore$indicator_type == "hospitalization count"] * hosp_weight
  #
  #   names(RiskScore)[1] = "district"
  #
  #   RiskScore = RiskScore %>%
  #     group_by(district,week) %>%
  #     summarise(RS = sum(RS, na.rm = T)%>% round(3))
  #
  # }
  #
  # risk_score <- reactive({
  #   req(reactive_epi_indicators())
  #
  #   case_weight <- 0.5
  #   death_weight <- 0.3
  #   hosp_weight <- 0.2
  #
  #   calculateRiskScore(reactive_epi_indicators(),case_weight,death_weight,hosp_weight, input$selectGroupBy)
  #  # Rs11 <<- RS
  #
  #  # RS
  # })

  # observe({
  #
  #   RS  <- risk_score()
  #   RS22 <<- RS
  #   # RS
  # })
  #
  # # Calculate the risk score as a weighted average of the rates
  # risk_score <- case_weight * avg_case_rate +
  #   death_weight * avg_death_rate +
  #   hosp_weight * avg_hospitalization_rate
  #
  # # Return the risk score
  # return(risk_score)


  ## changed data here reactive_epi_indicators()
  # epi_risks <- eventReactive(input$ApplyChanges,{
  epi_risks <- reactive({
    req(reactive_epi_indicators()) ## req() added cause this make sures the data we are going to use is below is updated after filters and waits until it has arrived
    req(input$selectGroupBy)

    reactive_epi_indicators1 <<- reactive_epi_indicators()

    if(input$selectGroupBy == "City"){
      Temp =   reactive_epi_indicators()
    }
    if(input$selectGroupBy == "Country"){
      Temp =   reactive_epi_indicators()

      Temp = merge(Temp,LocationData, by = "district")%>% distinct()
      Temp = Temp %>%
        group_by(disease,indicator_type,week,person.address.country) %>%
        summarise(indicator_value = sum(indicator_value, na.rm = T))%>%
        mutate(district = person.address.country) %>%
        select("disease","district","week","indicator_value", "indicator_type")

    }
    if(input$selectGroupBy == "Region"){
      Temp =   reactive_epi_indicators()

      Temp = merge(Temp,LocationData, by = "district")%>% distinct()
      Temp = Temp %>%
        group_by(disease,indicator_type,week,responsibleRegion) %>%
        summarise(indicator_value = sum(indicator_value, na.rm = T))%>%
        mutate(district = responsibleRegion) %>%
        select("disease","district","week","indicator_value", "indicator_type")

    }
    if(input$selectGroupBy == "District"){
      Temp =   reactive_epi_indicators()

      Temp = merge(Temp,LocationData, by = "district")%>% distinct()
      Temp = Temp %>%
        group_by(disease,indicator_type,week,responsibleDistrict) %>%
        summarise(indicator_value = sum(indicator_value, na.rm = T))%>%
        mutate(district = responsibleDistrict) %>%
        select("disease","district","week","indicator_value", "indicator_type")

    }
    # "responsibleRegion"      "responsibleDistrict"

    add_epi_risks(Temp, risk_types, risk_thresholds(),
                  input$n_reference_weeks, input$n_observation_weeks)
  })





  # Plots and tables

  epi_risks_display <- reactive({
    req(epi_risks())
    epi_risks1 <<- epi_risks()
    prepare_display(epi_risks(), display_filter(),
                    input$n_reference_weeks, input$n_observation_weeks)
  })



  ## Heatmap
  heatmap_height <- reactive({
    req(epi_risks_display())
    Mutiplier = 1
    if(length(unique(epi_risks_display()$district)) < 50){
      Mutiplier = 2
    }
    if(length(unique(epi_risks_display()$district)) < 30){
      Mutiplier = 2.5
    }
    if(length(unique(epi_risks_display()$district)) < 20){
      Mutiplier = 3
    }
    if(length(unique(epi_risks_display()$district)) < 15){
      Mutiplier = 3.5
    }
    if(length(unique(epi_risks_display()$district)) < 9){
      Mutiplier = 5
    }
    if(length(unique(epi_risks_display()$district)) < 6){
      Mutiplier = 8
    }
    if(length(unique(epi_risks_display()$district)) < 3){
      Mutiplier = 10
    }
    if(length(unique(epi_risks_display()$district)) == 1){
      Mutiplier = 20
    }

    print(length(unique(epi_risks_display()$district)))
    round(relative_height_heatmap*Mutiplier *
            length(unique(epi_risks_display()$district)))
  })

  output$plot_heatmap <- renderPlot({
    req(epi_risks_display())
    epi_risks_display1 <<- epi_risks_display()

    plot_heatmap_risk(epi_risks_display(), risk_colors,
                      input$n_observation_weeks, y_lab = input$selectGroupBy)
  })

  ## not using height anymore
  output$plot_heatmap_ui <- renderUI({
    plotOutput("plot_heatmap", height = heatmap_height())
    # plotOutput("plot_heatmap")
  })

  ## Time series
  timeseries_height <- reactive({
    req(epi_risks_display())
    if(length(unique(epi_risks_display()$district)) /
       ncol_timeseries < 4){
      round(
        relative_height_timeseries *
          length(unique(epi_risks_display()$district)) /
          ncol_timeseries
      )*5
    }else{

      round(
        relative_height_timeseries *
          length(unique(epi_risks_display()$district)) /
          ncol_timeseries
      )
    }
  })
  output$plot_timeseries <- renderPlot({
    req(epi_risks_display())
    plot_timeseries_risk(epi_risks_display(), risk_colors, ncol_timeseries)
  })

  ## not using height anymore
  output$plot_timeseries_ui <- renderUI({
    plotOutput("plot_timeseries", height = timeseries_height())
    # plotOutput("plot_timeseries")
  })

  ## Table
  output$epi_risks <- DT::renderDataTable({
    # req(epi_risks())
    req(epi_risks_display())

    Temp <- epi_risks_display()
    # RS <- risk_score()



    # Temp = merge(Temp,RS, by = c("district","week"))



    # Temp1 <<- epi_risks_display()
    # Temp1111 <<- Temp

    # Temp1111$RS = calculate_risk_score(Temp1111$indicator_value, input$low_mid_absolute, input$mid_high_absolute)
    # Temp$RS = calculate_risk_score(Temp1111$indicator_value, input$low_mid_absolute, input$mid_high_absolute)
    Temp$RS = NA
    if(input$risk_type == "absolute"){

      for (i in 1:nrow(Temp)) {
        Temp$RS[i] =    calculate_risk_score(Temp$indicator_value[i], input$low_mid_absolute, input$mid_high_absolute)
      }

    }
    else if(input$risk_type == "anomaly"){

      for (i in 1:nrow(Temp)) {
        Temp$RS[i] =    calculate_risk_score(Temp$indicator_value[i], input$low_mid_anomaly, input$mid_high_anomaly)
      }

    }

    Temp = distinct(Temp)
    # Temp$RS =  lapply(x = Temp1111$indicator_value,calculate_risk_score(x, input$low_mid_absolute, input$mid_high_absolute))

    # names(Temp)[2] =
    names(Temp)[names(Temp) == "district"] = input$selectGroupBy
    names(Temp)[names(Temp) == "indicator_value"] = "Indicator Value"
    names(Temp)[names(Temp) == "week"] = "Week"
    names(Temp)[names(Temp) == "risk_level"] = "Risk Level"
    names(Temp)[names(Temp) == "RS"] = "Risk Score"

    # Temp1 <<- Temp
    # Temp = Temp %>%
    #   select(input$selectGroupBy,Week, `Indicator Value`,
    #          `Risk Level`)
    # Temp = Temp[,c(2,3,4,9)]
    Temp = Temp[,c(input$selectGroupBy,"Week","Indicator Value","Risk Level","Risk Score")]
    # Temp2 <<- Temp


    DT::datatable(
      # epi_risks(),
      Temp,
      style = "bootstrap",
      extensions = "Scroller",
      options = list(dom = "Bftlrip", scrollX = TRUE,
                     buttons = c("csv", "excel"), rownames = FALSE, deferRender = TRUE,
                     scrollY = 400, scroller = TRUE)
    ) |>
      DT::formatStyle(columns = 1:ncol(Temp),`font-size` = "12px")
  })




  ## r markdown for info tab
  output$info <- renderUI({
    withMathJax(HTML(readLines(rmarkdown::render(input = "info.rmd",
                                                 output_format = rmarkdown::html_fragment(),
                                                 quiet = TRUE
    ))))
  })


}

shiny::shinyApp(ui, server)
