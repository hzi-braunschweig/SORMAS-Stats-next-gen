# This function extract all the ids of nodes linked to a specified source node
contIdsForSingleChain = function(elist, uuid_node)
{
  fromId = unique(elist[ elist$from_uuid_person %in% c(uuid_node), ]$from)
  elistNew = elist[, colnames(elist) %in% c("from","to", 'id')]
  datSel = elistNew[elistNew$from %in% fromId,  ]
  #datRetained = elistNew[!(elistNew$from %in% fromId),  ]
  fromId = datSel$to
  repeat{ 
    tempSel = elistNew[elistNew$from %in% fromId,  ] # one option to optimise this can be to select from remaining data set not from all elist each time
    fromId = tempSel$to
    if(dim(tempSel)[1] != 0)
    {
      datSel = rbind(datSel,tempSel)
    } else{
      break
    }
  }
  contId = datSel$id
  return(contId)
}