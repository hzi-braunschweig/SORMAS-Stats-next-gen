# compute vector of person_uuid of source nodes only
compute_person_node_uuid = function(elist)
{
  # takes elist and return a vector of unique person nodeuuid
  person_eventPart_uuid = 
    elist %>%
    dplyr::select(to_uuid_person, entityType ) %>%
    dplyr::filter(entityType == "Event") %>%
    dplyr::distinct_at(. , vars(to_uuid_person)) 
  
  person_uuid = 
    elist %>%
    dplyr::select(from_uuid_person, to_uuid_person, entityType ) %>%
    dplyr::filter(entityType == "Case") %>%
    dplyr::distinct_at(. , vars(from_uuid_person, to_uuid_person))  
  
  paerson_node_uuid = unique( c(person_eventPart_uuid$to_uuid_person, person_uuid$from_uuid_person, person_uuid$to_uuid_person))
  return(paerson_node_uuid)
}
