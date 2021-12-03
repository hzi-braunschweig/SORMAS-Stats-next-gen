## Event barplot by jurisdiction ----
barplotEventStatusByJurisdiction  = function(data, Var1, count = TRUE){
  # This function depends on twoByTwoTablefunction function and produce the barplot of events by status
  # Var1 and Var2 are any 2 categorical variables, 
  # for now Var2 must be event status, this function can be generalised to take any other varaible
  countData = twoByTwoTablefunction(data = data, Var1 = Var1, Var2 = "eventstatus", spread = FALSE, Proportion = FALSE)
  mypalett = c("CLUSTER" = "#f70707","EVENT" = "#ffa500", "SIGNAL" = "#ffff00", "SCREENING" = "#706c67")
  fig = ggplot(data=countData, aes(x=Var2, y=n, fill=Var1)) 
  if(count==FALSE){
    fig =  fig + geom_bar(stat="identity", position="fill") + #, position=position_dodge()) to place bars side by side
      xlab(NULL) + ylab("Proportion") 
  } else {
    fig =  fig + geom_bar(stat="identity") +
      xlab(NULL) + ylab("Count") 
  }
  fig = fig + 
    labs(fill = NULL) +
    scale_fill_manual(values=mypalett) +
    theme_classic()
  fig = ggplotly(fig)  
  return(fig)
}