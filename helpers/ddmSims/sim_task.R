rbind.all.columns <- function(x, y) {
  
  if(ncol(x) == 0 | ncol(y) == 0){
    out = plyr::rbind.fill(x, y)
  } else{
    x.diff <- setdiff(colnames(x), colnames(y))
    y.diff <- setdiff(colnames(y), colnames(x))
    x[, c(as.character(y.diff))] <- NA
    y[, c(as.character(x.diff))] <- NA
    out = rbind(x, y)
  }
  return(out)
}

sim_task = function(stimuli, model_name, ...){
  
  kwargs = list(...)
  # 
  # Initialize any missing arguments
  if (!("nonDecisionTime" %in% names(kwargs))){
    kwargs$nonDecisionTime = 0
  }
  if (!("barrier" %in% names(kwargs))){
    kwargs$barrier = 1
  }
  if (!("barrierDecay" %in% names(kwargs))){
    kwargs$barrierDecay = 0
  }
  if (!("bias" %in% names(kwargs))){
    kwargs$bias = 0
  }
  if (!("lotteryBias" %in% names(kwargs))){
    kwargs$lotteryBias = 0.1
  }
  if (!("timeStep" %in% names(kwargs))){
    kwargs$timeStep = 10
  }
  if (!("maxIter" %in% names(kwargs))){
    kwargs$maxIter = 400
  }
  if (!("epsilon" %in% names(kwargs))){
    kwargs$epsilon = 0.0002
  }
  if (!("stimDelay" %in% names(kwargs))){
    kwargs$stimDelay = 2000
  }
  if (!("debug" %in% names(kwargs))){
    kwargs$debug = FALSE
  }

  # Extract the correct trial simulator for the model_name
  sim_trial = sim_trial_list[[model_name]]
  
  # Create placeholder output df
  out = data.frame()
  
  if(kwargs$debug){
    print(paste0("Simulating task with parameters: model_name = ", model_name,
          ", non-decision time = ", kwargs$nonDecisionTime,
          ", barrier = ", kwargs$barrier,
          ", barrierDecay = ", kwargs$barrierDecay,
          ", bias = ", kwargs$bias,
          ", lotteryBias = ", kwargs$lotteryBias,
          ", timeStep = ", kwargs$timeStep,
          ", maxIter = ", kwargs$maxIter,
          ", epsilon = ", kwargs$epsilon,
          ", stimDelay = ", kwargs$stimDelay
          ))
  }

  # Loop through  all the rows of the input
  for(i in 1:nrow(stimuli)) {
    # Simulate RT and choice for a single trial with given DDM parameters and trial stimulus values
    if(model_name %in% c("model4", "model5","model6","model7")){
      
      cur_out = sim_trial(dArb=kwargs$dArb, dAttr=kwargs$dAttr, sigmaArb = kwargs$sigmaArb, sigmaAttr = kwargs$sigmaAttr, 
                          barrier = kwargs$barrier,nonDecisionTime = kwargs$nonDecisionTime, barrierDecay = kwargs$barrierDecay,
                          lotteryBias = kwargs$lotteryBias, timeStep = kwargs$timeStep,
                          maxIter = kwargs$maxIter,
                          epsilon = kwargs$epsilon,
                          EVLeft=stimuli$EVLeft[i], EVRight = stimuli$EVRight[i], 
                          QVLeft = stimuli$QVLeft[i], QVRight= stimuli$QVRight[i], 
                          probFractalDraw = stimuli$probFractalDraw[i])
    } else{
      cur_out = sim_trial(d=kwargs$d, sigma = kwargs$sigma, 
                          barrier = kwargs$barrier, nonDecisionTime = kwargs$nonDecisionTime, barrierDecay = kwargs$barrierDecay,
                          bias = kwargs$bias, timeStep = kwargs$timeStep, maxIter = kwargs$maxIter, epsilon = kwargs$epsilon,
                          stimDelay = kwargs$stimDelay, 
                          EVLeft=stimuli$EVLeft[i], EVRight = stimuli$EVRight[i], 
                          QVLeft = stimuli$QVLeft[i], QVRight= stimuli$QVRight[i] , 
                          probFractalDraw = stimuli$probFractalDraw[i])
      
    }
    
    # Append the trial to the rest of the output
    out = rbind.all.columns(out, cur_out)
    
  }
  
  # Add details of the parameters used for the simulation
  out$model = model_name
  if(model_name %in% c("model4", "model5", "model6","model7")){
    out$dArb = kwargs$dArb
    out$dAttr = kwargs$dAttr
    out$sigmaArb = kwargs$sigmaArb
    out$sigmaAttr = kwargs$sigmaAttr
  } else {
    out$d = kwargs$d
    out$sigma = kwargs$sigma
  }
  out$nonDecisionTime = kwargs$nonDecisionTime
  out$barrierDecay = kwargs$barrierDecay
  
  return(out)
}
