
fit_ddm_pta = function(data_to_fit_, model_name_, search_space_, posteriors_tbt_ = FALSE){
  
  # Initiallize ranges for the parameter search space
  if("rangeD" %in% names(search_space_)){
    rangeD_ = search_space_[["rangeD"]]
  } else{
    rangeD_ = c(0)
  }
  
  if("rangeSigma" %in% names(search_space_)){
    rangeSigma_ = search_space_[["rangeSigma"]]
  } else{
    rangeSigma_ = c(1e-9)
  }
  
  if("rangeDelta" %in% names(search_space_)){
    rangeDelta_ = search_space_[["rangeDelta"]]
  } else{
    rangeDelta_ = c(1)
  }
  
  if("rangeGamma" %in% names(search_space_)){
    rangeGamma_ = search_space_[["rangeGamma"]]
  } else{
    rangeGamma_ = c(1)
  }
  
  # Initialize additional objects that will hold the outputs of fitting
  numModels = nrow(expand.grid(search_space_))
  likelihoods = list()
  models = c()
  posteriors = list()
  
  # Get likelihoods for all models and all artificial trials.
  for (i in 1:length(rangeD_)){
    curD = rangeD_[i]
    for (j in 1:length(rangeSigma_)){
      curSigma = rangeSigma_[j] 
      for(k in 1:length(rangeDelta_)){
        curDelta = rangeDelta_[k]
        for(l in 1:length(rangeGamma_)){
          curGamma = rangeGamma_[l]
          model = paste0(as.character(curD), ", ", as.character(curSigma), ", ", as.character(curDelta), ", ", as.character(curGamma))
          curFit = fit_task(data_to_fit_, model_name = model_name_, pars_ = list(d=curD, sigma = curSigma, delta = curDelta, gamma = curGamma))
          likelihoods[[model]] = curFit$likelihood
          models = c(models, model)
          posteriors[[model]] = 1/numModels
        }
      }
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
    
    # Compute the posteriors trial by trial
    posteriors_tbt = NA
    
    if(posteriors_tbt_){
      posteriors_tbt = list()
      
      for (i in 1:length(models)){
        model = models[i]
        posteriors_tbt[[model]] = c(1/numModels, rep(NA,length(likelihoods[[1]])-1))
      }
      
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

