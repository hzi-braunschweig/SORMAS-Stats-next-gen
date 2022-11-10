## CASE DATA ANALYSIS-----
# This sub section of the server.r file renders the case data analysis tab
# All back-end methods related to this tab should be added in this file
# ui element to filter casePersonRegionDist by district based on user selected region 
output$pickerInputdistrictCaseUi <- renderUI({
  if(!is.null(input$regionCaseUi))
  {temp = casePersonRegionDist %>%
   dplyr::filter(region_name %in% input$regionCaseUi)  %>%
   distinct(district_name) %>%
   .$district_name
  }else{temp = NULL}
  pickerInput(inputId = 'districtCaseUi', label = 'District of case',
              choices = temp, options = list(`actions-box` = TRUE, size = 12),
              selected = NULL, multiple = TRUE)
  })
# Filtering casepersonRegion
casePersonRegionFilter = reactive({
if(is.null(input$regionCaseUi))
{ casePersonRegionDist[((casePersonRegionDist$disease == input$diseaseCaseUi) & (casePersonRegionDist$reportdate >= (min(input$reportdateCaseUi))) & (casePersonRegionDist$reportdate <= (max(input$reportdateCaseUi)) )), ]
} else{
 casePersonRegionDist[((casePersonRegionDist$region_name %in% input$regionCaseUi) & (casePersonRegionDist$disease == input$diseaseCaseUi) & (casePersonRegionDist$reportdate >= (min(input$reportdateCaseUi) )  ) & (casePersonRegionDist$reportdate <= (max(input$reportdateCaseUi) ))),]
}
})
# filter by district of case
casePersonRegionDistFilter = reactive({
  
  if(is.null(input$districtCaseUi))
  {
    temp = casePersonRegionFilter()
  } else{
    temp = casePersonRegionFilter() %>%
      dplyr::filter(district_name %in% input$districtCaseUi)
  }
  return(temp)
  
})
# Adding control based on apply changes icon on front ui
# Any output or computation that depend on casePersonFilter wouuld run only when caseDataAnalysisAction is clicked
casePersonFilter = eventReactive(input$caseDataAnalysisAction, { 
  casePersonRegionDistFilter() 
}, ignoreNULL = FALSE)

#### case KPI ################
# case classification
## all cases
output$allCase <- renderInfoBox({
  infoBox("All cases", nrow(casePersonFilter() ), icon = icon("procedures"),  color = colCase, fill = FALSE)
})
# unclassified cases
output$UnclassifiedCases <- renderInfoBox({
  temp = casePersonFilter()
  temp = temp[temp$caseclassification == "NOT_CLASSIFIED" ,]
  infoBox("Unclassified", nrow(temp), icon = icon("procedures"),  color = colCase, fill = FALSE )
})
# suspected cases
output$suspectedCases <- renderInfoBox({
  temp = casePersonFilter()
  temp = temp[temp$caseclassification == "SUSPECT",]
  infoBox("Suspect", nrow(temp), icon = icon("procedures"),  color = colCase, fill = FALSE )
})
# Probable cases
output$probableCases <- renderInfoBox({
  temp = casePersonFilter()
  temp = temp[temp$caseclassification == "PROBABLE",]
  infoBox("Probable", nrow(temp), icon = icon("procedures"),  color = colCase, fill = FALSE )
})
# Confirmed cases
output$confirmedCases <- renderInfoBox({
  temp = casePersonFilter()
  temp = temp[temp$caseclassification %in% c("CONFIRMED", "CONFIRMED_NO_SYMPTOMS", "CONFIRMED_UNKNOWN_SYMPTOMS"),] # combination of all confirmed cases
  infoBox("Confirmed", nrow(temp), icon = icon("procedures"),  color = colCase, fill = FALSE )
})

# case outcome
# diseases cases
output$diseasedCases <- renderInfoBox({
  temp = casePersonFilter()
  temp = temp[temp$outcome == "DECEASED",]
  infoBox("Diseased", nrow(temp), icon = icon("procedures"),  color = colCase, fill = FALSE )
})
# recovered cases
output$recoveredCases <- renderInfoBox({
  temp = casePersonFilter()
  temp = temp[temp$outcome == "RECOVERED",]
  infoBox("Recovered", nrow(temp), icon = icon("procedures"),  color = colCase, fill = FALSE )
})
# unknownoutcome cases
output$unknownOutcomeCases <- renderInfoBox({
  temp = casePersonFilter()
  temp = temp[temp$outcome == "UNKNOWN",]
  infoBox("Unknown", nrow(temp), icon = icon("procedures"),  color = colCase, fill = FALSE )
})
# unknownoutcome cases
output$noOutcomeCases <- renderInfoBox({
  temp = casePersonFilter()
  temp = temp[temp$outcome == "NO_OUTCOME",]
  infoBox("No Outcome", nrow(temp), icon = icon("procedures"),  color = colCase, fill = FALSE )
})
## case origin
# incountry cases
output$incountryCases <- renderInfoBox({
  temp = casePersonFilter()
  temp = temp[temp$caseorigin == "IN_COUNTRY",]
  infoBox("In-country", nrow(temp), icon = icon("procedures"),  color = colCase, fill = FALSE )
})
# POE/ imported cases
output$importedCases <- renderInfoBox({
  temp = casePersonFilter()
  temp = temp[temp$caseorigin == "POINT_OF_ENTRY",]
  infoBox("Imported", nrow(temp), icon = icon("procedures"),  color = colCase, fill = FALSE )
})
## case quarantine #####
# institutional
output$institutionalQCases <- renderInfoBox({
  temp = casePersonFilter()
  temp = temp[is.na(temp$quarantine) != T,]  # deleting NA before filtering
  temp = temp[temp$quarantine == "INSTITUTIONELL",]
  infoBox("Institutional", nrow(temp), icon = icon("procedures"),  color = colCase, fill = FALSE )
})
# home
output$homeQCases <- renderInfoBox({
  temp = casePersonFilter()
  temp = temp[is.na(temp$quarantine) != T,]  # deleting NA before filtering
  temp = temp[temp$quarantine == "HOME",]
  infoBox("Home", nrow(temp), icon = icon("procedures"),  color = colCase, fill = FALSE )
})
# other
output$otherQCases <- renderInfoBox({
  temp = casePersonFilter()
  temp = temp[is.na(temp$quarantine) != T,]  # deleting NA before filtering
  temp = temp[temp$quarantine == "OTHER",]
  infoBox("Other", nrow(temp), icon = icon("procedures"),  color = colCase, fill = FALSE )
})
# None
output$noneQCases <- renderInfoBox({
  temp = casePersonFilter()
  temp = temp[is.na(temp$quarantine) != T,]  # deleting NA before filtering
  temp = temp[temp$quarantine == "NONE",]
  infoBox("None", nrow(temp), icon = icon("procedures"),  color = colCase, fill = FALSE )
})
# unknown
output$unknownQCases <- renderInfoBox({
  temp = casePersonFilter()
  temp = temp[is.na(temp$quarantine) != T,]  # deleting NA before filtering
  temp = temp[temp$quarantine == "UNKNOWN",]
  infoBox("Unknown", nrow(temp), icon = icon("procedures"),  color = colCase, fill = FALSE )
})
# Missing
output$MissingQCases <- renderInfoBox({
  temp = casePersonFilter()
  temp = temp[is.na(temp$quarantine) == T,]
  infoBox("Missing", nrow(temp), icon = icon("procedures"),  color = colCase, fill = FALSE )
})
## case sex ####
# male
output$maleSexCases <- renderInfoBox({
  temp = casePersonFilter()
  temp = temp[is.na(temp$sex) != T,]  # deleting NA before filtering
  temp = temp[temp$sex == "MALE",]
  infoBox("Male", nrow(temp), icon = icon("procedures"),  color = colCase, fill = FALSE )
})
#female
output$femaleSexCases <- renderInfoBox({
  temp = casePersonFilter()
  temp = temp[is.na(temp$sex) != T,]  # deleting NA before filtering
  temp = temp[temp$sex == "FEMALE",]
  infoBox("Female", nrow(temp), icon = icon("procedures"),  color = colCase, fill = FALSE )
})
#other
output$otherSexCases <- renderInfoBox({
  temp = casePersonFilter()
  temp = temp[is.na(temp$sex) != T,]  # deleting NA before filtering
  temp = temp[temp$sex == "OTHER",]
  infoBox("Other", nrow(temp), icon = icon("procedures"),  color = colCase, fill = FALSE )
})
#unknown
output$unknownSexCases <- renderInfoBox({
  temp = casePersonFilter()
  temp = temp[is.na(temp$sex) != T,]  # deleting NA before filtering
  temp = temp[temp$sex == "UNKNOWN",]
  infoBox("Unknown", nrow(temp), icon = icon("procedures"),  color = colCase, fill = FALSE )
})
#missing
output$MissingSexCases <- renderInfoBox({
  temp = casePersonFilter()
  temp = temp[is.na(temp$sex) == T,]
  infoBox("Missing", nrow(temp), icon = icon("procedures"),  color = colCase, fill = FALSE )
})


## case occupation ####
# health care worker
output$healthWorkerOcupCases <- renderInfoBox({
  temp = casePersonFilter()
  temp = temp[is.na(temp$occupationtype) != T,]  # deleting NA before filtering
  temp = temp[temp$occupationtype == "HEALTHCARE_WORKER",]
  infoBox("Health worker", nrow(temp), icon = icon("procedures"),  color = colCase, fill = FALSE )
})
# other occupations
output$otherOcupCases <- renderInfoBox({
  temp = casePersonFilter()
  temp = temp[is.na(temp$occupationtype) != T,]  # deleting NA before filtering
  temp = temp[temp$occupationtype != "HEALTHCARE_WORKER",] #counting all occupations except health workers
  infoBox("Other", nrow(temp), icon = icon("procedures"),  color = colCase, fill = FALSE )
})
# missing occupations
output$MissingOcupCases <- renderInfoBox({
  temp = casePersonFilter()
  temp = temp[is.na(temp$occupationtype) == T,]
  infoBox("Missing", nrow(temp), icon = icon("procedures"),  color = colCase, fill = FALSE )
})
## case age ####
# min age
output$minAgeCases <- renderInfoBox({
  temp = casePersonFilter()
  temp = temp[is.na(temp$age) != T,]  # deleting NA before filtering
  infoBox("Minimum", min(temp$age), icon = icon("procedures"),  color = colCase, fill = FALSE )
})
# median age
output$medianAgeCases <- renderInfoBox({
  temp = casePersonFilter()
  temp = temp[is.na(temp$age) != T,]  # deleting NA before filtering
  infoBox("Median", median(temp$age), icon = icon("procedures"),  color = colCase, fill = FALSE )
})
# mean age
output$meanAgeCases <- renderInfoBox({
  temp = casePersonFilter()
  temp = temp[is.na(temp$age) != T,]  # deleting NA before filtering
  infoBox("Mean", round(mean(temp$age), digits = 2), icon = icon("procedures"),  color = colCase, fill = FALSE )
})
# max age
output$maxAgeCases <- renderInfoBox({
  temp = casePersonFilter()
  temp = temp[is.na(temp$age) != T,]  # deleting NA before filtering
  infoBox("Maximum", max(temp$age), icon = icon("procedures"),  color = colCase, fill = FALSE )
})
# missing age
output$missingAgeCases <- renderInfoBox({
  temp = casePersonFilter()
  temp = temp[is.na(temp$age) == T,]
  infoBox("Missing", nrow(temp), icon = icon("procedures"),  color = colCase, fill = FALSE )
})
## Case sum by region and other variables ######
# tabulate cases by region
output$caseCountbyRegionTable <- DT::renderDataTable({
if(is.null(input$regionCaseUi) ){
cuntbyRegionTable = cuntbyRegionDistrictCase(data = casePersonFilter(), byRegion = TRUE )
if(input$caseByRegionIndicatorTypeUi == "Count"){
res =  DT::datatable(
  cuntbyRegionTable,
  filter = 'top', extensions = c('Buttons', 'Scroller'),
  options = list(scrollY = 550,
  scrollX = 500,
  deferRender = TRUE,
  scroller = TRUE,
  # paging = TRUE,  # pageLength = 25,
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
    proportionByregion(data = cuntbyRegionTable),
    filter = 'top', extensions = c('Buttons', 'Scroller'),
    options = list(scrollY = 550,
      scrollX = 500,
      deferRender = TRUE,
      scroller = TRUE,
      # paging = TRUE, # pageLength = 25,
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
        filter = 'top', extensions = c('Buttons', 'Scroller'),
        options = list(scrollY = 550,
                       scrollX = 500,
                       deferRender = TRUE,
                       scroller = TRUE,
                       # paging = TRUE,  # pageLength = 25,
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
        proportionByregion(data = cuntbyRegionTable),
        filter = 'top', extensions = c('Buttons', 'Scroller'),
        options = list(scrollY = 550,
                       scrollX = 500,
                       deferRender = TRUE,
                       scroller = TRUE,
                       # paging = TRUE, # pageLength = 25,
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
  if(!(dataframe_is_empty(temp))){ 
  if(input$sexCategoryUi == "Male X Female")
  {
    fg =  pyramidPlotFunction(data = temp, sexCat = "MaleFemale")
  }
  if(input$sexCategoryUi == "Male X Others")
  {
    fg =  pyramidPlotFunction(data = temp, sexCat = "MaleOther")
  }
  if(input$sexCategoryUi == "Female X Others")
  {
    fg = pyramidPlotFunction(data = temp, sexCat = "FemaleOther")
  }
  }else{
    fg = empty_dataframe_plotly()
  }
  return(fg)
})
## Plotting time series plot for cases ##
output$caseTimeSeriesPlot <- renderPlotly({
  temp = casePersonFilter()
  #check if data frame is not empty and plot  else plot an empty plot
  if(!(dataframe_is_empty(temp))){ 
  if (input$timeUnitUi == "Day")
  {
    if(input$byRegiontimeUnitUi == F)
    {
      dateSumCase = stats::aggregate(total ~ reportdate, data = temp, sum, na.rm = F)
      fg=  timeSeriesPlotDay(data = dateSumCase, cum = input$cumUi )
    } else{
      dateSumCaseRegion = stats::aggregate(total ~ reportdate + region_name, data = temp, sum, na.rm = F)
      fg = timeSeriesPlotDayRegion(data = dateSumCaseRegion, cum = input$cumRegionUi)
    }
  }
  if (input$timeUnitUi == "Epi-week")
  {
    weekSumCase = stats::aggregate(total ~ reportweek+ reportyear, data = temp, sum, na.rm = F)
    fg=   timeSeriesPlotWeek(data = weekSumCase )
  }
  if (input$timeUnitUi == "Month")
  {
    monthSumCase = stats::aggregate(total ~ reportmonth+ reportyear, data = temp, sum, na.rm = F)
    fg=  timeSeriesPlotMonth(data = monthSumCase )
  }
  }else{
    fg = empty_dataframe_plotly()
  }
  return(fg)
})
## Plotting epicure
output$caseEpicurvePlot <- renderPlotly({
  temp = casePersonFilter()
  if(!(dataframe_is_empty(temp))){ 
  if (input$timeUnitEpicurveUi == "Day")
  {
    dateSumCaseClass = stats::aggregate(total ~ reportdate + caseclassification, data = temp, sum, na.rm = F)
    fg=  epicurveDate(data = dateSumCaseClass)
  }
  if (input$timeUnitEpicurveUi == "Epi-week")
  {
    dateSumCaseClass = stats::aggregate(total ~ reportdate + caseclassification, data = temp, sum, na.rm = F)
    fg=  epicurveDate(data = dateSumCaseClass)
  }
  if (input$timeUnitEpicurveUi == "Month")
  {
    fg =   epicurveMonth(data = temp)
  }
  } else{
    fg = empty_dataframe_plotly()
  }
  return(fg)
})

## Map for cases ####
## map plot
output$regionMapCaseCount <- renderPlot({
  temp = casePersonFilter()
  if(!(dataframe_is_empty(temp))){
  if(input$caseMapshapesUi == "By region")
  {
    if(input$caseIndicatorTypeMapUi == "Count"){
      fg = regionMapPlot(data = temp, lnd = regionShapes)
    }
    if(input$caseIndicatorTypeMapUi == "Incidence proportion / 100,000"){
      fg = empty_dataframe_plotly()
    }
  }
  if(input$caseMapshapesUi == "By district")
  {
    if(input$caseIndicatorTypeMapUi == "Count"){
    fg = districtMapPlot(data = temp, districtShapes =districtShapes)
    }
    if(input$caseIndicatorTypeMapUi == "Incidence proportion / 100,000"){
      fg = empty_dataframe_plotly()
    }
  }
    }else{
    fg = empty_dataframe_plotly()
  }
  return(fg)
})
## Rt analysis and plotting-----
# using casePersonFilter() and infectorInfecteeData
# Filtering by disease, time, region of infector and  deduplication (unique infector-infectee persons)
infectorInfecteeDataDiseaseRegionFilter = reactive({
  if(is.null(input$regionCaseUi))
  {
    ret = infectorInfecteeData %>% 
      dplyr::filter(disease_infector == input$diseaseCaseUi) %>% 
      dplyr::filter(report_date_infector >= input$reportdateCaseUi[1] & report_date_infector <= input$reportdateCaseUi[2] )
  } else{
    ret = infectorInfecteeData %>% 
      dplyr::filter( disease_infector == input$diseaseCaseUi & region_infector %in% input$regionCaseUi) %>% 
      dplyr::filter((report_date_infector >= input$reportdateCaseUi[1]) & (report_date_infector <= input$reportdateCaseUi[2]) )
  }
  # de-duplication,  keeping only unique infector-infectee persons
  ret = ret %>% 
    dplyr::distinct_at(. , vars(person_id_case_infector, person_id_case_infectee), .keep_all = TRUE) 
  return(ret)
})  
# filter by district of infector_case  
infectorInfecteeDataDiseaseRegionDist = reactive({
  if(is.null(input$districtCaseUi))
  {
    ret = infectorInfecteeDataDiseaseRegionFilter() 
  } else{
    ret = infectorInfecteeDataDiseaseRegionFilter() %>%
      dplyr::filter(district_infector %in% input$districtCaseUi)
  }
  return(ret)
})
#Rendering and showing ui element for mean and sd  based on user defined choice of "SI estimation method": parametric or si_from_data
# Only show mean and sd when method == parametric
observe({
  if(input$rtMethodUi == "Parametric distribution"){
    shinyjs::show(id = "showMeanSdSIUI", anim = TRUE)
  } 
  else {
    shinyjs::hide(id = "showMeanSdSIUI", anim = TRUE)
  } 
})
# filter by serial interval range 
# Adding control based on apply changes icon on front ui
infectorInfecteeDataDiseaseRegionDistSerialIntFilter  =  eventReactive(input$caseDataAnalysisAction, {
  temp = infectorInfecteeDataDiseaseRegionDist() %>%
    tidyr::drop_na(serial_interval) %>%    # Dropping rows with NA fo SI
    dplyr::filter(., serial_interval %in% c(input$serialIntervalRangeUi[1] : input$serialIntervalRangeUi[2]))  # filter by SI range
  return(temp)
}, ignoreNULL = FALSE)

#Preparing data and estimating rt
rt_data = eventReactive(input$caseDataAnalysisAction, {
  # This is a list of all the data needed to estimate and plot Rt
  # The two data sets used are the complete case line listing casePersonFilter and SI data 
  # preparing si_data data
  rt_analysis_data = casePersonFilter()  
  rt_analysis_data$total = 1
  dateSumCase = stats::aggregate(total ~ reportdate, data = rt_analysis_data, FUN = sum, na.rm = T)  
  # completing missing dates
  dateSumCase =  dateSumCase %>%
    dplyr::mutate(Date = as.Date(reportdate)) %>%
    tidyr::complete(Date = seq.Date(min(Date), max(Date), by="day"), fill = list(total = 0))
  dateSumCase = dateSumCase[,c(1,3)] # dropping old uncompleted date
  colnames(dateSumCase) = c("dates","I")
  temp = infectorInfecteeDataDiseaseRegionDistSerialIntFilter()   # data having SI as column
  # extracting SI values that are not NA, negative and fall in the range specifird by user
  temp =  temp %>%
    dplyr::filter((serial_interval > 0) & (serial_interval <= input$siUi))
  n = nrow(temp)
  si_data = data.frame(matrix(0,n,5))
  si_data[,2] = 1
  si_data[,3] = c(temp$serial_interval -1)
  si_data[,4] = temp$serial_interval
  colnames(si_data) =  c("EL", "ER", "SL", "SR", "type")
  si_data[,-5] = apply(si_data[,-5], 2, as.integer) # all columns except type should be integer
  #estimating and computing summary of mean rt
  if(input$rtMethodUi == "Parametric distribution"){
    distVec = c(Gamma = "G", Weibull = "W", Lognormal = "L") 
    distUI = distVec[names(distVec) == input$si_rt_UI] # getting short form of distribution names to be used in modelling
    ret = RtPlot(mean_si = input$mean_siUI, std_si = input$std_siUI, method = "parametric_si",  burnin = 1000, dateSumCase = dateSumCase,
                 si_data = si_data, rsi = input$rsiUi, dist = distUI, rt_legend = input$rtLegandUi) # method = "parametric_si" or "si_from_data"; rsi = "all", "R", "SI"
  }
  if(input$rtMethodUi == "Transmission data" ){
    ret =  RtPlot(dateSumCase = dateSumCase, method = "si_from_data",  burnin = 1000,  si_data = si_data, rsi = input$rsiUi, rt_legend = input$rtLegandUi) # method = "parametric_si" or "si_from_data"; rsi = "all", "R", "SI"
  }
  return(ret) # retuned vector of estimated rt and plot of estimated rt
}, ignoreNULL = FALSE) 
#plotting Rt
output$rtPlot <- renderPlot({
  fig = rt_data()$rt_fig # extracting figure component in rt_data
  return(fig)
})
# Computing summary statistics for rt
output$rtSummary_table <- DT::renderDataTable({
  temp = round(summary(rt_data()$rt_mean), 2) # computing summary of extracted bootstrap means
  #converting summary output to data frame
  temp2 = data.frame(x=t(matrix(temp)))
  names(temp2) = names(temp)
  rownames(temp2) = c("")
  res = DT::datatable(temp2,
   options = list(
     dom = 't',
     fixedColumns = TRUE,
     #autoWidth = TRUE,
     columnDefs = list(list(className = 'dt-center', targets = "_all")),
     searching = FALSE
   ), 
   rownames = FALSE )
  return(res)
})
## SI analysis
# model selection for SI  
# fiting normal, weibull, gamma, lnorm distributions to serial intervals 
output$si_model_fitTable <- DT::renderDataTable({
  temp = fit_distribution(serial_interval = infectorInfecteeDataDiseaseRegionDistSerialIntFilter()$serial_interval)
  res = DT::datatable(temp,
                      options = list(
                        dom = 't',
                        fixedColumns = TRUE,
                        #autoWidth = TRUE,
                        columnDefs = list(list(className = 'dt-center', targets = "_all")),
                        searching = FALSE
                      ), 
                      rownames = FALSE )
  return(res)
}) 
# summary statistics for SI
output$si_summaryTable <- DT::renderDataTable({
  temp = summary_statistics(x = infectorInfecteeDataDiseaseRegionDistSerialIntFilter()$serial_interval)
  res = DT::datatable(
    data = temp,
    options = list(
      dom = 't',
      fixedColumns = TRUE,  #autoWidth = TRUE,
      columnDefs = list(list(className = 'dt-center', targets = "_all")),
      searching = FALSE
    ), 
    rownames = FALSE)
  return(res)
})

# plotting distributions for model selection
# plotting distribution of data
output$SI_hist_model_plot <- renderPlot({
  temp = fitdist_plot(x = infectorInfecteeDataDiseaseRegionDistSerialIntFilter()$serial_interval)
  temp$density
})
#qq plot for si
output$SI_model_qq_plot <- renderPlot({
  temp = fitdist_plot(x = infectorInfecteeDataDiseaseRegionDistSerialIntFilter()$serial_interval)
  temp$qq
})
#cdf plot for si
output$SI_model_cdf_plot <- renderPlot({
  temp = fitdist_plot(x = infectorInfecteeDataDiseaseRegionDistSerialIntFilter()$serial_interval)
  temp$cdf
})
# computing mean CI based on user specified distribution
# Delay reactivity to compute mean estimate and 95% CI for serial interval based on distribution with best fit to the data
# bases on serial interval range, distribution, etc
si_mean_CI_table_data = eventReactive(input$caseDataAnalysisAction, { 
  ret_mean_CI = serial_interval_mean_CI(infectorInfecteePair = infectorInfecteeDataDiseaseRegionDistSerialIntFilter(),
  distr = input$siDistMethodUi, input$serialIntervalRangeUi[1], maxSi = input$serialIntervalRangeUi[2])   
}, ignoreNULL = FALSE)
output$si_mean_CI_table <- DT::renderDataTable({
  res = DT::datatable(
    data = si_mean_CI_table_data() ,
    options = list(
      dom = 't',
      fixedColumns = TRUE,#autoWidth = TRUE,
      columnDefs = list(list(className = 'dt-center', targets = "_all")),
      searching = FALSE
    ), 
    rownames = FALSE)
  return(res)
})

# fitting user chosen distribution to SI
# Any output or computation that depend on siRet (ie data and parameters used to generate siRet) would run only when 
# caseDataAnalysisAction is clicked

siRet = eventReactive(input$caseDataAnalysisAction, { 
  temp = infectorInfecteeDataDiseaseRegionDistSerialIntFilter()
  siRet = serialIntervalPlot(infectorInfecteePair = temp,  distr = input$siDistMethodUi, niter = input$niter_SI_UI,
                             minSi = input$serialIntervalRangeUi[1], maxSi = input$serialIntervalRangeUi[2] ) 
  return(siRet)
}, ignoreNULL = FALSE)

# plotting SI
output$distribution_SI_plot <- renderPlot({
  temp = siRet()
  temp$siDistributionPlot
})
# exporting estimates of SI
output$SI_estimate_table <- DT::renderDataTable({
  temp = siRet()
  res = DT::datatable(
    data =  temp$siEstmate,
    options = list(
      dom = 't',
      fixedColumns = TRUE,#autoWidth = TRUE,
      columnDefs = list(list(className = 'dt-center', targets = "_all")),
      searching = FALSE
    ), 
    rownames = TRUE)
  return(res)
})

# Offspring distribution and estimation of dispersion parameter k
# conditioning all estimates from kRet to deleay response based on the caseDataAnalysisAction icon
# Since infector-infectee pairs with NA for serial interval can be included in offspring analysis
# we do not need to use infectorInfecteeDataDiseaseRegionDistSerialIntFilter() but to use infectorInfecteeDataDiseaseRegionDist()

# summary statistics for offspring distribtion
nodedegree_summaryTab = eventReactive(input$caseDataAnalysisAction, { 
  # compute node degree
  temp = offspringDistPlot(infectorInfecteePair = infectorInfecteeDataDiseaseRegionDist(), polyDegree = input$polyDegree_RtK_UI)
  # compute summary statistics table
  ret = summary_statistics(x = temp$offspringDegree )
  return(ret)
}, ignoreNULL = FALSE)
# exporting nodedegree_summaryTab table to ui
output$nodedegree_summaryTable <- DT::renderDataTable({
  res = DT::datatable(
    data = nodedegree_summaryTab() ,
    options = list(
      dom = 't',
      fixedColumns = TRUE,#autoWidth = TRUE,
      columnDefs = list(list(className = 'dt-center', targets = "_all")),
      searching = FALSE
    ), 
    rownames = FALSE)
  return(res)
})

# Estimation of dispersion parameter k and R
kRet <- eventReactive(input$caseDataAnalysisAction, {
  temp =  infectorInfecteeDataDiseaseRegionDist() # infectorInfecteeDataDiseaseRegionDistSerialIntFilter()
  kRet = offspringDistPlot(infectorInfecteePair = temp, polyDegree = input$polyDegree_RtK_UI)
  return(kRet)
}, ignoreNULL = FALSE) 

# plotting k
output$distribution_k_plot <- renderPlot({
  temp = kRet()
  plot(temp$offspringDistributionPlot)
})
# exporting estimate of k
output$k_estimate_table <- DT::renderDataTable({
  temp = kRet()
  res = DT::datatable(
    data =  temp$rkEstmate,
    options = list(
      dom = 't',
      fixedColumns = TRUE,
      #autoWidth = TRUE,
      columnDefs = list(list(className = 'dt-center', targets = "_all")),
      searching = FALSE
    ), 
    rownames = TRUE)
  return(res)
})
