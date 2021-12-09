# function that fit 4 distributions to a numeric vector
# This functon can be used to fit a normal, lognormal, wribull and gamma distributions to serial_interval or incubarion period
fit_distribution = function(serial_interval){
  # serial_interval can be any numeric varaible
  serial_interval = serial_interval[is.na(serial_interval) == FALSE] # dropping NA
  #fitting normal dist
  # Parameter1 = mean, Parameter2 = sd
  nfit  = fitdistrplus::fitdist(data = serial_interval, distr = "norm")  # fit a normal distribution to the data
  ## fitting log normal dist
  # Parameter1 = meanlog, Parameter2 = sdlog
  lnfit  = serial_interval[serial_interval > 0]  %>%  # lnorm can not be used to describe a random varaible with negative or 0 values
    fitdistrplus::fitdist(data = ., distr = "lnorm")  
  # fitting a Weibull distribution
  # Parameter1 = shape, Parameter2 = scale
  wfit  = serial_interval[serial_interval > 0]  %>% # weibull can not be used to describe a random varaible with negative or 0 values
    fitdistrplus::fitdist(data = ., distr = "weibull")  
  # fitting gamma dist
  # Parameter1 = shape, Parameter2 = rate
  gfit  = serial_interval[serial_interval > 0]  %>% # gamma can not be used to describe a random varaible with negative or 0 values
    fitdistrplus::fitdist(data = ., distr = "gamma") 
  ### extracting model parameters: model with smaller AIC has better fit to the data
  normal_estimates =  data.frame(Distribution = "Normal", Parameter1 = nfit$estimate[1], Parameter2 = nfit$estimate[2],
                                 AIC = nfit$aic, BIC = nfit$bic, Kolmogorov = gofstat(nfit)$ks, Cramer = gofstat(nfit)$cvm ,
                                 Anderson =  gofstat(nfit)$ad )
  #Goodness-of-fit statistics:  ks = Kolmogorov-Smirnov, cvm = Cramer-von Mises, ad = Anderson-Darling
  # Goodness-of-fit criteria: AIC = Akaike's Information Criterion, BIC = Bayesian Information Criterion
  
  lognormal_estimates =  data.frame(Distribution = "Lognormal", Parameter1 = lnfit$estimate[1], Parameter2 = lnfit$estimate[2],
                                    AIC = lnfit$aic, BIC = lnfit$bic, Kolmogorov = gofstat(lnfit)$ks, Cramer = gofstat(lnfit)$cvm,
                                    Anderson =  gofstat(lnfit)$ad )
  weibull_estimates =  data.frame(Distribution = "Weibull", Parameter1 = wfit$estimate[1], Parameter2 = wfit$estimate[2],
                                  AIC = wfit$aic, BIC = wfit$bic, Kolmogorov = gofstat(wfit)$ks, Cramer = gofstat(wfit)$cvm,
                                  Anderson =  gofstat(wfit)$ad )
  gamma_estimates =  data.frame(Distribution = "Gamma", Parameter1 = gfit$estimate[1], Parameter2 = gfit$estimate[2],
                                AIC = gfit$aic, BIC = gfit$bic, Kolmogorov = gofstat(gfit)$ks, Cramer = gofstat(gfit)$cvm,
                                Anderson =  gofstat(gfit)$ad)
  dist_estimates = rbind(normal_estimates, lognormal_estimates, weibull_estimates, gamma_estimates)
  rownames(dist_estimates) = NULL
  dist_estimates = dplyr::arrange(dist_estimates, AIC, Kolmogorov, BIC ) # sorting with smaller AIC on first row
  dist_estimates[,-1] = round(dist_estimates[,-1], 4)
  return(dist_estimates)
}