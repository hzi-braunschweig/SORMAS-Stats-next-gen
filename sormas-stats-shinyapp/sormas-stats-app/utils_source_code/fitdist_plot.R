fitdist_plot = function(x){
  # x is a numeric vertor (eg serial interval, incubation period, etc) to fit distributions and plot the cdf
  # fitting models. All compared fits must have been obtained with the same dataset, thus we drop all records <=0
  x = x[is.na(x) == FALSE] ## lnorm, gamma, weibull can not be used to describe a random varaible with negative or 0 values
  x = x[x > 0]
  nfit  = fitdistrplus::fitdist(data = x, distr = "norm")  # fit a normal distribution to the data
  # Parameter1 = meanlog, Parameter2 = sdlog
  lnfit  = fitdistrplus::fitdist(data = x, distr = "lnorm")  
  # Parameter1 = shape, Parameter2 = scale
  wfit  = fitdistrplus::fitdist(data = x, distr = "weibull")  
  # Parameter1 = shape, Parameter2 = rate
  gfit  = fitdistrplus::fitdist(data = x, distr = "gamma") 
  # plotting cdf, # main = Empirical and theoretical CDFs
  cdfcomp(list(wfit, gfit, lnfit, nfit), legendtext=c("Weibull", "gamma", "lognormal", "normal"),
          main = NULL, fitlwd = 2) 
  cdf <- recordPlot() # saving immage
  plot.new() ## clean up device
  # plotting histrogram and theretical densities
  denscomp(list(wfit, gfit, lnfit, nfit), legendtext=c("Weibull", "gamma", "lognormal", "normal"),
           main = NULL, probability = FALSE, fitlwd = 2 )
  density <- recordPlot() # saving immage
  plot.new() ## clean up device
  # plotting Q-Q plot
  qqcomp(list(wfit, gfit, lnfit, nfit), legendtext=c("Weibull", "gamma", "lognormal", "normal"),
         main = NULL, fitlwd = 2)
  qq <- recordPlot() # saving immage
  plot.new() ## clean up device
  return(list(cdf = cdf, density = density, qq = qq))
}