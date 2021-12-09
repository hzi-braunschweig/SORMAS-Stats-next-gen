# To  compute and plot time dependent reproduction number
# weekly estimate of Rt, week is default but can be latered in config
RtPlot = function(mean_si, std_si, method="parametric_si", burnin = 1000, dateSumCase, si_data, rsi="all", dist = "G", rt_legend = FALSE) # rsi = "all","R", "SI"
{
  #the parametric distribution to use when estimating the serial interval from data on dates of 
  #symptoms of pairs of infector/infected individuals dist = "G", "G" (Gamma), "W" (Weibull), "L" (Lognormal) 
  # dateSumCase is a datafram of 2 columens: date and number of cases
  if(method == "parametric_si")
  {
    res <- estimate_R(dateSumCase,method="parametric_si", config = make_config(list(mean_si = mean_si, std_si = std_si))) 
  }
  if(method == "si_from_data")
  {
    MCMC_seed <- 1
    overall_seed <- 2
    mcmc_control <- make_mcmc_control(seed = MCMC_seed, 
                                      burnin = burnin)
    #dist <- "G" # fitting a Gamma dsitribution for the SI
    config <- make_config(list(si_parametric_distr = dist,
                               mcmc_control = mcmc_control,
                               seed = overall_seed, 
                               n1 = 50, 
                               n2 = 50))
    res <- estimate_R(dateSumCase,
                      method = "si_from_data",
                      si_data = si_data,
                      config = config)
  }
  rt_fig = plot(res, rsi, legend = rt_legend) # plot of rt
  rt_mean = res$R$`Mean(R)` # vector containing average of estimated rt values
  return(list(rt_fig = rt_fig, rt_mean = rt_mean))
}
# # test run
# casePersontest = casePerson
# casePersontest$total = 1
# dateSumCase = stats::aggregate(formula = total ~  reportdate, data = casePersontest, FUN = sum, na.rm = T)
# dateSumCase =  dateSumCase %>%
#   dplyr::mutate(Date = as.Date(reportdate)) %>%
#   tidyr::complete(Date = seq.Date(min(Date), max(Date), by="day"), fill = list(total = 0))
# dateSumCase = dateSumCase[,c(1,3)] # dropping old uncompletted date
# colnames(dateSumCase) = c("dates","I")
# 
# temp = infectorInfecteeData  # data having SI as column
# # extracting SI values that are not NA, negative and fall in the range specifird by user
# temp =  temp %>%
#   dplyr::filter((serial_interval > 0) & (serial_interval <= input$siUi))
# n = nrow(temp)
# si_data = data.frame(matrix(0,n,5))
# si_data[,2] = 1
# si_data[,3] = c(temp$serial_interval -1)
# si_data[,4] = temp$serial_interval
# colnames(si_data) =  c("EL", "ER", "SL", "SR", "type")
# si_data[,-5] = apply(si_data[,-5], 2, as.integer) # all columns except type should be integer
# test_res = RtPlot(mean_si = 5, std_si = 2.5, method = "parametric_si",  burnin = 1000, dateSumCase = dateSumCase, si_data = si_data,  dist = "G")