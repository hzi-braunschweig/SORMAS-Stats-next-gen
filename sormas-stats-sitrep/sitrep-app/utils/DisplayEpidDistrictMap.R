#' Display district level counts on map
#'
#' @param epid_data Output of the GetEpidBase() function.
#' 
#' @param epidemic_indicator Can be any of the columns of the epid_data.
#'
#' @param geoshapes_data Output of the ExportGeoshapes() function.
#' 
#' @return A district level map of the counts of one of the epidemic indicators
#' total cases, new cases, total hospitalizations, new hospitalizations, 
#' total deaths and new deaths.
#' 
#' @seealso [GetEpidbase(), ExportGeoshapes()] 
#'
#' @export
#'
#' @examples
DisplayEpidDistrictMap <-function(epid_data, geoshapes_data, epidemic_indicator){
  # join geo shapes with the epid base data
  geo_epid_data <- geoshapes_data %>% 
    dplyr::left_join(epid_data, by = "name_district")
  
  # build map for chosen epidemic indicator
  ggplot(geo_epid_data) +
    geom_sf(aes(fill = .data[[epidemic_indicator]], geometry = geometry))+
    labs(x = "", y = "")+
    theme(axis.text.x = element_blank(),
          axis.text.y = element_blank(),
          axis.ticks = element_blank(),
          panel.background = element_rect(fill = "white", color = "black"))+
    viridis::scale_fill_viridis(option= "viridis", name={{epidemic_indicator}})
  
}