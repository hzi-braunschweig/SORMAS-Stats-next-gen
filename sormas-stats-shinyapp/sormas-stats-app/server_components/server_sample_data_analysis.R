## SAMPLE DATA ANALYSIS -----
# This sub section of the server.r file renders the sample data analysis tab
# All back-end methods related to this tab should be added in this file
# filtering district name from sample_table based of users selected region and parse it back to the frontend ----
output$pickerInputdistrictSampleUi <- renderUI({
  if(!is.null(input$regionSampleUi))
  {
    temp = sample_table %>%
      dplyr::filter(region_name %in% input$regionSampleUi)  %>%
      distinct(district_name) %>%
      .$district_name
  }else{
    temp = NULL
  }
  pickerInput(inputId = 'districtSampleUi', label = 'District of sample',
              choices = temp, 
              options = list(`actions-box` = TRUE, size = 12),
              selected = NULL,
              multiple = TRUE
  )
})
# filter sample_table by disease, region, and report date ----
selSampleRegionDiseaseTime = reactive({
  if(!is.null(input$regionSampleUi)){
    sample_table[((sample_table$region_name  %in% input$regionSampleUi) & (sample_table$disease_sample == input$diseaseSampleUi) & (sample_table$date_of_report >= (min(input$reportdateSampleUi) )  ) & (sample_table$date_of_report <= (max(input$reportdateSampleUi) ))),]
  } else{
    sample_table[((sample_table$disease_sample == input$diseaseSampleUi) & (sample_table$date_of_report >= (min(input$reportdateSampleUi))) & (sample_table$date_of_report <= (max(input$reportdateSampleUi)) )), ]
  }
})    
# add more filters here, one reactive function per filter condition
# fitter by district of sample
selSampleRegionDiseaseTimeDist = reactive({
  if(is.null(input$districtSampleUi))
  {temp = selSampleRegionDiseaseTime() } else{
    temp = selSampleRegionDiseaseTime() %>%
      dplyr::filter(district_name %in% input$districtSampleUi)
  }
  return(temp)
})
## add more filters here if needed, use one variable per filter
# filter 1
# filter 2, etc.
# checking if user is authenticated before creating ui elements
sample_table_filtered = reactive({
  req(credentials()$user_auth)
  selSampleRegionDiseaseTimeDist()
}) 
# Adding control based on "apply changes" icon  ---
# Any output or computation that depends on sample_table_selected would run only when input$sampleDataAnalysisAction is clicked
sample_table_selected = eventReactive(input$sampleDataAnalysisAction,{ 
  sample_table_filtered()
}, ignoreNULL = FALSE) 
# plotting bar chart -----
#extracting user selected variable to plot bar chart 
sample_barVar = eventReactive(input$sampleDataAnalysisAction,{
  temp_data = sample_table_selected()
  if(input$bargraphSampleVariableUi != "samplematerial"){
    ret = as.character(temp_data[, colnames(temp_data) == input$bargraphSampleVariableUi])
  }else{
    ret=temp_data$samplematerial
  }
},ignoreNULL = FALSE)
# plotting bar chart
output$barChartSampleUi <- renderPlotly({
  if(input$sampleIndicatorTypeUi == "Count"){ 
    fg = univariate_barplot(var = sample_barVar(), count=TRUE, x_verticalLayout = TRUE )
  }
  if(input$sampleIndicatorTypeUi == "Proportion"){ 
    fg = univariate_barplot(var = sample_barVar(), count=FALSE, x_verticalLayout = TRUE)
  }
  return(fg)
})
# plotting pie chart
output$pieCdhartSampleUi <- renderPlotly({
  fg = pieChartPlot(variable = sample_barVar())
})

# UI output for "sample data analysis" tab ----
# The output object "sample_analysis_output" should be placed below the computation of elements needed in it
output$sample_analysis_output <- renderUI({
  panels <- list(
    tabPanel("Sample dashboard",
             fluidRow(width=10,                                                         
                      column(6,                                                                
                             wellPanel(
                               h4(helpText("To add some figures or info boxes here.")) 
                             )
                      ),
                      column(6,                    
                             wellPanel(
                               h4(helpText("To add some info boxes here")) 
                             ) )
             ) ## end of fluid row
    )
  )
  # check for configuration and add more panels 
if(sample_custom_indicators=="t"){
panels[[2]]=tabPanel("Custom indicators",
fluidRow(width=10,                                                         
column(6,                                                                
 wellPanel(
   h4(helpText("Dynamic pie chart")) ,
   div(plotlyOutput("pieCdhartSampleUi", width = "100%", height = "50vh" ), style = "font-size: 100%; width: 100%" ) 
 ) ),
column(6,                    
 wellPanel(
   h4(helpText("Dynamic bar graph")) ,
   div(plotlyOutput("barChartSampleUi", width = "100%", height = "50vh" ), style = "font-size: 100%; width: 100%" ) 
 )  )
) ## end of fluid row
    )}
  base::do.call(tabsetPanel, panels) 
})
# Sample custom indicator tab filters should be added here 
# This filters would show on ui only if "custom indicator tab" is activated
output$sample_cuctom_indicator_filter = renderUI({
  if(sample_custom_indicators=="t"){
    pickerInput(
      inputId = "bargraphSampleVariableUi",
      label = 'Choose custom indicator variable',
      choices=sort(c("pathogentestresult","samplingreason","samplepurpose","shipped","received",
                     "specimencondition","samplesource","samplematerial")), 
      options = list(`actions-box` = TRUE, size = 12),
      selected = NULL,  multiple = FALSE)
  } })