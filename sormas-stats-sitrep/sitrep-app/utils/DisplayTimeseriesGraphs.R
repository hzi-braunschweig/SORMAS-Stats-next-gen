#' Display Timeseries Graphs
#' @description Displays the timeseries graphic for the SitRep.
#' It contains a time series of cases by report date, a time series
#' of cases by onset date, and a time series of hospitalizations by
#' admission date.
#'
#' @param timeseries_data Outout of the GetTimeseriesData() function.
#' 
#' @param colors A vector of three strings. Defined the colors of the three 
#' graphs. Must be colors recognized by gglpot.
#' Defaults to colors = c("steelblue","darkred","#E69F00").
#'
#' @return A plot containing the following three timeseries graphs:
#' -cases by report date
#' -cases by onset date
#' -hospitalizations by admission date
#' 
#' @seealso [GetTimeseriesData(), TimeseriesGraph()]
#' @export
#'
#' @examples
DisplayTimeseriesGraphs <- function(timeseries_data,
                                    colors = c("steelblue","darkred","#E69F00")){
  
  #theme
  theme_set(theme_bw())
  
  # get timeseries graph for confirmed cases by report date
  cases_by_report_date_graph <- TimeseriesGraph(timeseries_data = timeseries_data,
                                          variable = "REPORT_TOTAL_CONFIRMED",
                                          date_column = "date",
                                          line_color = colors[1])
  
  # get timeseries graph for confirmed cases by onset date
  cases_by_onset_date_graph <- TimeseriesGraph(timeseries_data = timeseries_data,
                                                variable = "ONSET_TOTAL_CONFIRMED",
                                                date_column = "date",
                                               line_color = colors[2])
  
  # get timeseries graph for hospitalizations by admission date
  hospitalizations_by_admission_date_graph <- TimeseriesGraph(timeseries_data = timeseries_data,
                                                variable = "ADMISSION_REPORTED_DISEASE",
                                                date_column = "date",
                                                line_color = colors[3])
  
  # display all three graphs
  gridExtra::grid.arrange(cases_by_report_date_graph, cases_by_onset_date_graph, hospitalizations_by_admission_date_graph)
  
}