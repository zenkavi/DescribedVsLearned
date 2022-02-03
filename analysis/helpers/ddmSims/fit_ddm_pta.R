
fit_ddm_pta = function(data_to_fit_, model_name_, rangeD_, rangeSigma_, posteriors_tbt_ = FALSE){
  numModels = length(rangeD_) * length(rangeSigma_)
  likelihoods = list()
  models = c()
  posteriors = list()
  
  # Get likelihoods for all models and all artificial trials.
  for (i in 1:length(rangeD_)){
    curD = rangeD_[i]
    for (j in 1:length(rangeSigma_)){
      curSigma = rangeSigma_[j]
      model = paste0(as.character(curD), ", ", as.character(curSigma))
      curFit = fit_task(data_to_fit_, model_name = model_name_, pars_ = list(d=curD, sigma = curSigma))
      likelihoods[[model]] = curFit$likelihood
      models = c(models, model)
      posteriors[[model]] = 1/numModels
    }
  }
  
  # Compute the posteriors.
  for(t in 1:nrow(data_to_fit_)){
    denominator = 0
    for(m in 1:length(models)){
      model = models[m]
      denominator = denominator + (posteriors[[model]] * likelihoods[[model]][t]) 
      if(denominator == 0){
        next
      }
    }
    
    for(m in 1:length(models)){
      model = models[m]
      prior = posteriors[[model]]
      posteriors[[model]] = likelihoods[[model]][t] * prior /denominator
    }
    
    posteriors_tbt = NA
    
    if(posteriors_tbt_){
      posteriors_tbt = list()
      
      for (i in 1:length(models)){
        model = models[i]
        posteriors_tbt[[model]] = c(1/numModels, rep(NA,length(likelihoods[[1]])-1))
      }
      
      # Compute the posteriors.
      for(t in 1:nrow(data_to_fit_)){
        denominator = 0
        for(m in 1:length(models)){
          model = models[m]
          denominator = denominator + (posteriors_tbt[[model]][t] * likelihoods[[model]][t]) 
          if(denominator == 0){
            next
          }
        }
        
        for(m in 1:length(models)){
          model = models[m]
          prior = posteriors_tbt[[model]][t]
          posteriors_tbt[[model]][t+1] = likelihoods[[model]][t] * prior /denominator
        }
      }
      
    }
  }
  
  return(list(models = models, likelihoods = likelihoods, posteriors = posteriors, posteriors_tbt = posteriors_tbt))
}

