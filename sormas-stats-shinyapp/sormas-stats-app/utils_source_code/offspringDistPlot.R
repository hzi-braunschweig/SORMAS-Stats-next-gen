# This method fit a NB dist to the offsprint distr data and estimate R and k
# It uses the data called infectorInfecteePair
offspringDistPlot = function(infectorInfecteePair, niter = 51, polyDegree = NA){ 
  # Deleting duplicate pairs of infector-infectee
  # The data for infectorInfecteePair can have duplicates since a person can infect the same person more than once
  # The infectorInfecteePair should have unique infector-infectee pairs
  infectorInfecteePair = infectorInfecteePair %>% 
    dplyr::distinct_at(. , vars(person_id_case_infector, person_id_case_infectee), .keep_all = TRUE)
  
  #counting the number of offsprings per infector
  offspring <- infectorInfecteePair %>%
    dplyr::select(person_id_case_infector) %>%
    dplyr::group_by(person_id_case_infector) %>%
    dplyr::count() %>%
    dplyr::arrange(desc(n))
  
  # extracting infectee nodes
  infectee <- infectorInfecteePair %>%
    dplyr::select(person_id_case_infectee) %>%
    tidyr::gather() 
  #extracting infector nodes
  infector <-  infectorInfecteePair %>%
    dplyr::select(person_id_case_infector) %>%
    tidyr::gather()
  
  # selecting nodes that are linked to both infectors and infectees, thus duplicates
  duplicate <- infector %>%
    left_join(., infectee, by = 'value') %>%  # leftjoin infector and infectee
    dplyr::filter(key.y != 'NA') %>%  # filter all infectors who were also infectees in the past
    dplyr::select(value) %>% # keep only id
    dplyr::distinct()   # selecte distict node id, this should be person id in this case.
  
  # selecting terminal infectee nodes and sum them. 
  # This is a subset of termainal doses in the db because of infectorInfecteePair data
  nterminal_infectees <- infectee %>%
    dplyr::select(value) %>%
    dplyr::filter(!value %in% duplicate$value) %>% # infectees that are not infectors
    dplyr::transmute(case.no = as.character(value)) %>%
    nrow() 
  
  #create vector of complete offspring distribution with terminal cases having zero secondary cases
  complete_offspringd <- tibble::enframe(c(offspring$n, rep(0,nterminal_infectees))) # convert vector to dataframe
  
  #fit negative binomial distribution to the final offspring distribution
  fit <- complete_offspringd %>%
    dplyr::pull(value) %>%
    fitdistrplus::fitdist(., distr = "nbinom", method = "mle")
  # fit$estimate[[1]] = k or overdispersion parameter and fit$estimate[[1]]  = mu/ mean/ R
  # Estimating CI by bootstrap method 
  fit_boot <- summary(fitdistrplus::bootdist(fit, niter = niter))  
  
  # extracting estimates
  rkEstmate = dplyr::bind_cols(data.frame(fit$estimate), data.frame(fit_boot$CI) ) # extracting estimates and CI as a data frame
  colnames(rkEstmate) = c("Estimate", "Bootstrap median", "2.5% PCI", "97.5% PCI")
  rkEstmate = round(rkEstmate, 3) # mu = nbfit$estimate[[2]] = mean = overall reporoduction number and  size = nbfit$estimate[[1]] = dispersion parameter k 
  rownames(rkEstmate) = c("k" , "R" )
  #plot offspring distribution with negative binomial parameters
  #Setting polynomial degree
  if(is.na(polyDegree) == TRUE){
    polyDegree = length(unique(complete_offspringd$value))
    if(polyDegree > 9){
      polyDegreePlot = 9 # Think of adding parameter for user defined if need be.
    } else {
      polyDegreePlot = polyDegree-1 # polyDegreePlot should be less than the number of unique values
    }
  } else{
    polyDegreePlot = polyDegree
  }
  offspringDistributionPlot = ggplot(data = complete_offspringd) +
    geom_histogram(aes(x=value, y = ..density..), fill = "#dedede", colour = "Black", binwidth = 1) +
    geom_point(aes(x = value, y = dnbinom(x = value, size = fit$estimate[[1]], mu = fit$estimate[[2]])), size = 1) +
    stat_smooth(aes(x = value, y = dnbinom(x = value, size = fit$estimate[[1]], mu = fit$estimate[[2]])), method = 'lm', 
                formula = y ~ poly(x, polyDegreePlot), se = FALSE, size = 0.8, colour = 'black', linetype = 2) +
    expand_limits(x = 0, y = 0) +
    scale_x_continuous("Secondary cases per infector", expand = c(0, 0), breaks = seq(min(complete_offspringd$value), max(complete_offspringd$value), by = 1))  +
    scale_y_continuous("Proportion of onward transmission", expand = c(0, 0)) +
    theme_classic() +
    theme(aspect.ratio = 0.7)
  # extracting node degree
  offspringDegree = complete_offspringd$value
  ret = list(rkEstmate = rkEstmate, offspringDistributionPlot = offspringDistributionPlot, offspringDegree = offspringDegree)  # list object: table of estimates and image
}