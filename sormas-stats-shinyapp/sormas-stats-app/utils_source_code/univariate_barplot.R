# univariabte bar plot
univariate_barplot = function(var, count=FALSE, x_verticalLayout = FALSE){
  # This function takes a varaible and return a simple bar plot
  # count: to plot count or percentage
  # x_verticalLayout: to aligne x axis names horizontally or vertically
  df = as.data.frame(table(as.character(var), useNA = "ifany"))
  colnames(df) = c("Variable", "Count") 
  if(count == FALSE){
    fig = df %>%
      dplyr::mutate(Percent = prop.table(Count)) %>%
      ggplot(aes(x=Variable, y=Percent, fill = Variable, label = scales::percent(Percent)  )) +
      geom_col(position = 'dodge') + 
      geom_text(position = position_dodge(width = .9),    # move to center of bars
                vjust = -0.5,    # nudge above top of bar
                size = 3) + 
      scale_y_continuous(labels = scales::percent)
  }else{
    fig = ggplot(data=df, aes(x=Variable, y=Count, fill = Variable, label = Count )) +
      geom_col(position = 'dodge') + 
      geom_text(position = position_dodge(width = .9),    # move to center of bars
                vjust = -0.5,    # nudge above top of bar
                size = 3) 
  }
  fig = fig + theme(axis.line=element_blank(),
                    #axis.text.y=element_blank()
                    #,axis.ticks=element_blank(),
                    axis.title.x=element_blank(), # axis.title.y=element_blank(),
                    legend.position="none",
                    panel.background=element_blank(),
                    #panel.border=element_blank(),
                    panel.grid.major=element_blank(),
                    panel.grid.minor=element_blank(),
                    plot.background=element_blank())
  if(x_verticalLayout == TRUE){
    fig = fig + theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5)) 
  }
  fig = ggplotly(fig)
  return(fig)
}