# proportion of missing source nodes among nodes 
# This statistic only consider cases in the network ie not all cases in the system
prop_missing_source_case_nodes = function(elist, nodeLineList){
  nodeLineListSel = nodeLineList %>%
    dplyr::select(label, group) %>% # retaining infector (case or event)  nodes only in nodeLineList 
    dplyr::filter( (label %in% unique(c(elist$from_uuid_person, elist$to_uuid_person))) & (group %in% c("EVENT","PROBABLE", "SUSPECT", "CONFIRMED", "NOT_CLASSIFIED")) )  
  # extracting row (contact) in elist where the infector is not an infectee, ie source not contacts only
  # keeping only parent case nodes that were not previousely known to be contact nodes,
  elist_case = elist %>%
    dplyr::filter( !(from_uuid_person %in% to_uuid_person) ) %>%  
    # keeping distict infector nodes only
    dplyr::distinct_at(., vars(from_uuid_person), .keep_all = TRUE) %>%
    # keeping only node ids: from and from_uuid_person
    dplyr::mutate(from = from, from_uuid_person = from_uuid_person, .keep = "none")
  # Computing indicators
  source_node_uuid = sort(elist_case$from_uuid_person) # contains nodes for events and case person in case elist also had event nodes
  sum_missing_source_case_nodes = length(source_node_uuid) 
  sum_caseEvent_nodes = nrow(nodeLineListSel)  # computing total case nodes
  prop_missing_source_case_nodes = round((sum_missing_source_case_nodes/sum_caseEvent_nodes)*100, 2)
  
  return(list(source_node_uuid = source_node_uuid, source_node_data=elist_case,
              sum_caseEvent_nodes=sum_caseEvent_nodes, 
              prop_missing_source_case_nodes = prop_missing_source_case_nodes, 
              sum_missing_source_case_nodes = sum_missing_source_case_nodes ))
}