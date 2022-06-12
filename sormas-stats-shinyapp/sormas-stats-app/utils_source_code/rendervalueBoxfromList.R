#' @name rendervalueBoxfromList
#' @title  render a list of shiny columns containing valueBoxes
#'
#' @importFrom shiny column
#' @import stringi
#' @importFrom shinydashboard valueBox
#' @importFrom purrr map
#'
#'
#' @param list List of named lists containing the values for the boxes
#' @param icon An <i> (icon) HTML tag. or a list of html icons
#' @param color A color for the box. Valid colors are listed in validColors. help/library/shinydashboard/help/validColors
#' @return A list of shiny.tag's containing valueBoxes
#'
#'

rendervalueBoxfromList <- function(list,icon,color, fun = infoBox){
  # append cleaned list names to list as item
  x<- mapply(
    append,list,
    gsub("_"," ",stringi::stri_trans_totitle(names(list),opts_brkiter = stri_opts_brkiter(type = "sentence"))),
    SIMPLIFY = FALSE
  ) 
  
  # add icon
  x<-mapply(append,x, icon, SIMPLIFY = FALSE)
  # add icon if specifically linked to list item
  if(!is.null(names(icon))){
    for (i in names(icon)) {
      x[[i]][3]<-icon[[i]]
    }
  }
 
  ## add color
  x<-mapply(append,x, color, SIMPLIFY = FALSE)
  # add color if specifically linked to list item
  
  if(!is.null(names(color))){
    for (i in names(color)) {
      x[[i]][4]<-color[[i]]
    }
  }
  
  
  
  ## truncate  list
  x<-x[1:length(list)] 
  
  # map over column & infoBox
  map(x, ~column(2, fun("" ,.x[[1]],subtitle= .x[[2]], icon(.x[[3]]), color=.x[[4]],width = NULL))) |> fluidRow()
}



