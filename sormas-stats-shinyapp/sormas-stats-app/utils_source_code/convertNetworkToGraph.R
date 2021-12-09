# Converting elist and nodeLinelist to graph object
convertNetworkToGraph = convertNetworkToGraph = function(elist, nodeLineList){
  # filter elist with unique personuuid  and # reneming person_id with from and to.
  # this order is needed by the method graph_from_data_frame
  elistSel = elist %>%
    dplyr::distinct_at(., vars(from_uuid_person, to_uuid_person), .keep_all = TRUE) %>% 
    dplyr::mutate(from = from_uuid_person, to = to_uuid_person, .keep = "used") %>% 
    dplyr::relocate(from, to, from_uuid_person, to_uuid_person)
  
  # filter node that also belongs to  selected elist using lebel ie person uuid of the node
  node_uuid = unique(c(elistSel$from, elistSel$to))
  nodeLineListSelResCase = nodeLineList %>%
    dplyr::mutate(id = label, group = group, label=label, .keep = "none") %>%
    dplyr::filter(id %in% node_uuid) %>%
    dplyr::distinct_at(., vars(id), .keep_all = TRUE)
  
  # dropping all nodes in elist that are not in nodeList
  # this is not needed in case the filter at front end generated a data with such a situation
  elistSel = elistSel %>%
    dplyr::filter((from %in% nodeLineListSelResCase$id) &  (to %in% nodeLineListSelResCase$id) )
  # converting elist and nodeList to graph object
  net <- graph_from_data_frame(d=elistSel, vertices = nodeLineListSelResCase, directed=T)
  return(net)
}