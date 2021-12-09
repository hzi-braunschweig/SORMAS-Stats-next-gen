epicurveMonth = function(data){
  # initial formatting
  data$onset <- as.Date(data$reportdate, format="%d/%m/%Y")    # convert to Date class
  data$onset <- strftime(data$onset, format="%Y/%m")      # convert to Year-month
  data$onset <- paste(data$onset, "/01", sep = "")        # add arbitrary day on to end to make compatible w/ ggplot2
  
  # aggregate by month
  onset_counts <- aggregate(data$onset, by = list(date = data$onset, classification = data$caseclassification), length) # aggregate by month
  onset_counts$date = as.Date(onset_counts$date, format = "%Y/%m/%d") # covert to Date class
  onset_counts$classification = factor(onset_counts$classification, levels = c("NOT_CLASSIFIED", "SUSPECT", "PROBABLE","CONFIRMED")) 
  ## plotting
  mypalett = c("CONFIRMED" = "#f70707","PROBABLE" = "#ffa500", "SUSPECT" = "#ffff00", "NOT_CLASSIFIED" = "#706c67")
  # p4 =  ggplot(onset_counts, aes(x=date, y=x)) + geom_bar(stat="identity", aes(fill = factor(classification, levels = c("NOT_CLASSIFIED", "SUSPECT", "PROBABLE","CONFIRMED")) )) +
  p4 =  ggplot(onset_counts, aes(x=date, y=x)) + geom_bar(stat="identity", aes(fill = classification )) +
    theme(axis.text.x = element_text(angle=90, hjust = 1, vjust = 1)) +
    ylab("Number of cases") + xlab("Date of report") + scale_x_date(breaks="month", labels=date_format("%Y-%m"))+
    theme(legend.position="bottom", legend.direction="horizontal",legend.title = element_blank())+
    scale_fill_manual(values=mypalett)
  return(ggplotly(p4))
}