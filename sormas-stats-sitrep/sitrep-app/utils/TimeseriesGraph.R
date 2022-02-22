#' Timeseries graph  
#'
#' @param timeseries_data Output of the AggregateCountsByDate function.
#' @param variable Variable that should be plotted, must be a column of timeseries_data.
#' @param date_column Date column, must be a column of timeseries_data.
#'
#' @return Returns a timeseries graph of the entire time period which is in the 
#' date_column.
#' @seealso [AggregateCountsByDate()]
#' @export
#'
#' @examples
TimeseriesGraph <- function(timeseries_data, variable, date_column){
  
  #Timeseries graph
  timeseries_graph <- ggplot2::ggplot(timeseries_data, aes(x=.data[[date_column]], y = .data[[variable]])) +
    geom_line(size =1.5)+
    labs(x="", y="")+
    scale_x_date(date_breaks = "3 months")
  
  return(timeseries_graph)
}