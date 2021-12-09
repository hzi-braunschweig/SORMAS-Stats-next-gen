# sourceNodeDegreeCounter comput degree for source nodes for complete network
# the source node degree is then used to filter the network
sourceNodeDegreeCounter <- function(elist, nodeLineList){ 
  # this function depends on two others: convertNetworkToGraph and prop_missing_source_case_nodes
  networkGraphObject = convertNetworkToGraph(elist=elist, nodeLineList=nodeLineList) # converting network to graph object
  sourceNodeData = prop_missing_source_case_nodes(elist=elist, nodeLineList = nodeLineList)$source_node_data # computing source node data: node id and uuid
  deg <- degree(graph = networkGraphObject,  mode="all") #   deg <- degree(graph = networkGraphObject(), v = sourceNodes, mode="all")
  ret = data.frame(deg, names(deg))
  rownames(ret) = NULL
  colnames(ret) = c("deg","from_uuid_person")
  ret$from_uuid_person = as.character(ret$from_uuid_person)
  ## Merging ret with sourceNodeData to keep only sorcse nodes and their degree
  ret = ret %>%
    dplyr::right_join(., sourceNodeData,  c( "from_uuid_person" = "from_uuid_person"))  %>%
    distinct_at(., vars(from_uuid_person), .keep_all = TRUE) # source nodes and degree
  return(ret)
}