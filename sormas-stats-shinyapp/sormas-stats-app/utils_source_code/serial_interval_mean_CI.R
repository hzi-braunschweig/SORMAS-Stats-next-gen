# This function fit the specified distribution to seriel interval (infectorInfecteePair) and comput CI of the mean usig pivote method
# distr can be Normal, Weibull, Gamma, Lognormal
# distr = "eibull", "gamma", "lnorm", "norm"  
# minSi and maxSi are the min and max user specified values of si to be used for the analysis
# fiting Normal, Weibull, Gamma, Lognormal distributions to serial intervals 
serial_interval_mean_CI = function(infectorInfecteePair, distr = NULL, minSi = NULL, maxSi = NULL){ 
  # dropping rows with missing values for serial interval. This can heppen when one of the pairs has a missing onset date
  selData = infectorInfecteePair[is.na(infectorInfecteePair$serial_interval) == FALSE, ] # dplyr::filter(serial_interval != 'NA')
  
  # filtering selData based on user specified min and max values of serial interval.
  # return a vextor of SI values called x
  if(any(is.null(c(minSi, maxSi)))) {
    minSi = min(selData$serial_interval, na.rm = T)
    maxSi = max(selData$serial_interval, na.rm = T)
  }
  siVector = c(minSi: maxSi)
  x <- selData %>%
    dplyr::filter(serial_interval %in% siVector) %>% 
    pull(serial_interval) # or .$serial_interval
  # Fitting user specified distribution to SI
  ## normal distribution
  if(distr == "Normal"){
    fit  = fitdistrplus::fitdist(data = x, distr = "norm")  # fit a normal distribution to the data
    # Computing 95% CI using pivot method
    n = fit$n # number of data points used to fit the model
    mean_si = fit$estimate[1] # the sample mean
    sd_mean = fit$estimate[2]
    # 95% CI
    ll = mean_si - 1.96*sd_mean/sqrt(n)
    ul = mean_si + 1.96*sd_mean/sqrt(n) # this can also be computed using  confint(fit, level = 0.95)
    ret = data.frame(Distribution = distr, Mean = round(mean_si, 2), round(ll,2),  round(ul, 2) )
    colnames(ret) = c("Distribution", "Mean", "2.5% CI", "97.5% CI")
  }
  ## log normal
  # ref: https://stats.stackexchange.com/questions/469311/how-to-calculate-mean-and-sd-of-lognormal-distribution-based-on-meanlog-an
  if(distr == "Lognormal"){
    fit  = x[x > 0]  %>%  # lnorm can not be used to describe a random varaible with negative or 0 values
      fitdistrplus::fitdist(data = ., distr = "lnorm")  
    # Computing 95% CI using pivot method. converting meanlog and sdlog to mean and sd and computing CI
    n = fit$n # number of data points used to fit the model
    mu = fit$estimate[1] # mean in log scale ie meanlog
    sigma = fit$estimate[2]  # # sd in log scale ie sdlog
    mean_si = exp(mu + sigma^2/2 ) # mean = exp (mu + sigma^2/2)
    sd_mean = (exp(mu + 1/2*sigma^2)) * sqrt( exp(sigma^2)-1) # OR  sqrt(varaince)  = sqrt((exp(sigma^2) -1) * ( exp(2*mu+sigma^2)))
    # 95% CI
    ll = mean_si - 1.96*sd_mean/sqrt(n)
    ul = mean_si + 1.96*sd_mean/sqrt(n) # this can also be computed using  confint(fit, level = 0.95)
    ret = data.frame(Distribution = distr, Mean = round(mean_si, 2), round(ll,2),  round(ul, 2)  )
    colnames(ret) = c("Distribution", "Mean", "2.5% CI", "97.5% CI")
  }
  ## Weibull
  if(distr == "Weibull"){
    fit  = x[x > 0]  %>%  # weibull can not be used to describe a random varaible with negative or 0 values
      fitdistrplus::fitdist(data = ., distr = "weibull")  
    # Computing 95% CI using pivot method. converting meanlog and sdlog to mean and sd and computing CI
    n = fit$n # number of data points used to fit the model
    shape = fit$estimate[1]
    scale = fit$estimate[2]
    mean_si = scale*gamma(1+1/shape)  #lambda*Gamma(1+1/k)\,
    sd_mean = sqrt(scale^2 * (gamma(1+2/shape) - (gamma(1+1/shape))^2) ) # sqrt(varaince)   
    ll = mean_si - 1.96*sd_mean/sqrt(n)
    ul = mean_si + 1.96*sd_mean/sqrt(n) # this can also be computed using  confint(fit, level = 0.95)
    ret = data.frame(Distribution = distr, Mean = round(mean_si, 2), round(ll,2),  round(ul, 2)  )
    colnames(ret) = c("Distribution", "Mean", "2.5% CI", "97.5% CI")
  }
  ## Gamma
  if(distr == "Gamma"){
    fit  = x[x > 0]  %>%  # gamma can not be used to describe a random varaible with negative or 0 values
      fitdistrplus::fitdist(data = ., distr = "gamma")  
    # Computing 95% CI using pivot method. converting meanlog and sdlog to mean and sd and computing CI
    n = fit$n # number of data points used to fit the model
    shape = fit$estimate[1]
    rate = fit$estimate[2]
    mean_si = shape/rate
    sd_mean = sqrt(shape/rate^2) # sqrt(varaince)
    # 95% CI
    ll = mean_si - 1.96*sd_mean/sqrt(n)
    ul = mean_si + 1.96*sd_mean/sqrt(n) # this can also be computed using  confint(fit, level = 0.95)
    ret = data.frame(Distribution = distr, Mean = round(mean_si, 2), round(ll,2),  round(ul, 2) )
    colnames(ret) = c("Distribution", "Mean", "2.5% CI", "97.5% CI")
  }
  rownames(ret) = NULL
  ret = ret %>%
    dplyr::rename(Distribution = Distribution, Mean = Mean, "2.5% CI" = "2.5% CI", "97.5% CI" = "97.5% CI") # this kelps to maintain the same name in shinyapp
  return(ret)
}