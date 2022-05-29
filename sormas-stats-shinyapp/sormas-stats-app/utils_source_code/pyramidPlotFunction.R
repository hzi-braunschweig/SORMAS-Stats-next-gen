pyramidPlotFunction = function(data, sexCat = "MaleFemale"){
  # default sexCat is MaleFemale, possible values are: MaleOther, FemaleOther
  # sexCat defined the categories of sex to use
  data$sex = as.character(data$sex)
  data$sex[data$sex %in% c("","UNKNOWN")] = NA # assignning "UNKNOWN"and "" as missing
  pyramidData = data %>% tidyr::drop_na(sex,age)%>%  # dropping missing values of age and sex
    dplyr::filter(age<=130)  # dropping wring records for age using 130 yrs as max possible age of a person
  pyramidData = pyramidData[pyramidData$sex %in% c("MALE", "FEMALE", "OTHER"),] # retain mele and female only fro now
  ## cut the age variable into age groups with 5-year intervals
  pyramidData$ageCat = cut(pyramidData$age, breaks = seq(0, 130, 5), right = FALSE) 
  pyramidData$count = 1
  ## aggregate the data by gender and age group
  pyramidData <- stats::aggregate(count ~ sex + ageCat, data = pyramidData, FUN = sum)
  ## sort data by first by gender, then by age groups
  pyramidData <- with(pyramidData, pyramidData[order(sex,ageCat),])
  if(sexCat == "MaleFemale"){
    pyramidData = dplyr::filter(pyramidData, sex %in% c("MALE", "FEMALE") )
    pyramidData$caseCount <- ifelse(pyramidData$sex == "MALE", -1*pyramidData$count, pyramidData$count)
  }
  if(sexCat == "MaleOther"){
    pyramidData = dplyr::filter(pyramidData, sex %in% c("MALE", "OTHER") )
    pyramidData$caseCount <- ifelse(pyramidData$sex == "MALE", -1*pyramidData$count, pyramidData$count)
  }
  if(sexCat == "FemaleOther"){
    pyramidData = dplyr::filter(pyramidData, sex %in% c("FEMALE", "OTHER") )
    pyramidData$caseCount <- ifelse(pyramidData$sex == "FEMALE", -1*pyramidData$count, pyramidData$count)
  }
  pyramidData = pyramidData %>%  
    dplyr::rename(Sex = sex) 
  pyramitPlot = ggplot(data = pyramidData, mapping = aes(x = ageCat, y = caseCount, fill = Sex)) + 
    geom_col() +
    coord_flip() +
    scale_y_continuous(labels = abs, limits = max(pyramidData$count) * c(-1,1)) +
    labs(y = "Case count", x = "Age group") +
    theme(axis.text=element_text(size=12), axis.title=element_text(size=16), legend.title = element_text(size=14)  )
  pyramitPlot =  ggplotly(pyramitPlot) %>% layout(legend = list(orientation = "v", x = 1, y = 1)) # x and y determine legend position
  return(pyramitPlot)
}