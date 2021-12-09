summary_statistics = function(x){
  # compute and retun the summary stattistics of the numeric variable x in a dataframe format
  # x can be serial interval, incubation period, etc
  x = x[is.na(x) == FALSE]  # dropping NA
  temp = summary(x)
  summary_temp = data.frame(t( round(data.frame(x=matrix(temp),row.names=names(temp)), 2 ))) # convert summary output to dataframe
  row.names(summary_temp) = NULL
  # compute nunber and proportion of records <=0
  n = length(x)
  n_asymp_trans = length(x[x<=0])
  ret = data.frame(n, summary_temp, n_asymp_trans)
  ret = ret %>%
    dplyr::rename(N = n, Minimum = Min., Maximum = Max.,  Quart.1 = X1st.Qu., Quart.3 = X3rd.Qu., "n_value <= 0" = n_asymp_trans)
  rownames(ret) = c("")
  return(ret)
}