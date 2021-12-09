# This function extract node ids resulting from  multiple source nodes in the network diagram
contIdsForMultipleChains = function(elist, uuid_vector){
  # This function takes uuids of source nodes and retain all corresponding contacts
  # It depends on the contIdsForSingleChain function
  # uuid_vector = character vector of uuids of source nodes
  # elist = edges ie cotact table
  selected_nodes = c()
  for(i in uuid_vector){
    temp = contIdsForSingleChain(elist= elist, uuid_node = i) # select all contact ids of single chain resulting from i
    selected_nodes = c(selected_nodes, temp) # combine with other chains
  }
  return(selected_nodes)
}
# Test run
#uuid_vector = c("SICIBQ", "X7FWPK", "VSF6EV")
#contIdsForMultipleChains(elist = elist, uuid_vector = uuid_vector)
