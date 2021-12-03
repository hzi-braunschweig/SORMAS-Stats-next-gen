# proportion of contacts converted to case
prop_cont_ep_person_convertedToCase = function(elist)
{
  # number of contact and ep person nodes recorded, ie denominator
  n_contact_ep_nodes = elist %>%
    dplyr::select(to_uuid_person) %>%
    dplyr::distinct_at(. , vars(to_uuid_person)) %>%
    dplyr::summarise(n = n())
  
  #number of contact or ep persons converted to cases
  n_resultingCase_nodes = elist %>%
    dplyr::filter(resultingcase_id != "NA") %>%
    dplyr::select(to_uuid_person) %>%
    dplyr::distinct_at(. , vars(to_uuid_person)) %>%
    dplyr::summarise(n = n())
  
  # proportion of contacts or ep person nodes converted to cases
  ret = round(n_resultingCase_nodes/n_contact_ep_nodes *100, 2)
  
  return(list(prop_converted = ret, n_contact_ep_nodes= n_contact_ep_nodes, n_resultingCase_nodes=n_resultingCase_nodes))
}