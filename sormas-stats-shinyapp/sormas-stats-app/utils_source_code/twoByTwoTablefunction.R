# 2by2 function table. This method takes a dataframe and any user specified 2 columns and returned a 2x2 table -----
twoByTwoTablefunction = function(data,Var1, Var2, spread=FALSE, Proportion = FALSE,  spreadVar = "Var2")
{
  data = data[,colnames(data) %in% c(Var1,Var2 )]
  colnames(data) = c("Var1", "Var2")
  data = data %>%
    dplyr::mutate(across(everything(), as.character)) # converting all varaibles to characters
  temp = data %>% 
    dplyr::group_by(Var1, Var2 )  %>% 
    dplyr::summarise( n = n(), sort = TRUE) %>%
    dplyr::select(-sort) 
  if(spread == TRUE){
    if(Proportion == TRUE){
      ret = temp %>%
        dplyr::mutate(Prop = round(n/sum(n)*100 , 2) ) %>%
        dplyr::select(-n) %>%
        tidyr::spread(spreadVar, Prop)
      #ret[is.na(ret)] = 0  or  ret2 = ret %>% replace(is.na(.), 0) # It is not needed to replace NA with 0
    }else{ 
      ret =  temp %>%
        tidyr::spread(spreadVar, n)
      #ret[is.na(ret)] = 0
    }
  } else{
    if(Proportion == TRUE){
      ret = temp %>%
        dplyr::mutate(Prop = round(n/sum(n)*100 , 2) ) %>%
        dplyr::select(-n)
    } else {
      ret = temp 
    }
  }
  ret = as.data.frame(ret)
  return(ret)
}