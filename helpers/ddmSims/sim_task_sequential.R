# Helper function to combine outputs with different column names
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


# Function to simulate ddm process for a given set of stimuli using a model provided as a string in the model_name argument
sim_task = function(stimuli, model_name, sim_trial_list = stim_trial_list, ...){
  
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
  if (!("recallDelay" %in% names(kwargs))){
    kwargs$recallDelay = 0
  }
  if (!("debug" %in% names(kwargs))){
    kwargs$debug = FALSE
  }
  
  # Extract the correct trial simulator for the model_name
  sim_trial = sim_trial_list[[model_name]]
  
  # Create placeholder output df
  out = data.frame()
  
  # Print arguments that will be used for simulation if in debug mode
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
  
  # Sequential
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

    } else if (model_name %in% c("model4a", "model5a", "model6a", "model7a")){
      cur_out = sim_trial(dArb=kwargs$dArb, dLott=kwargs$dLott, dFrac=kwargs$dFrac, sigmaArb = kwargs$sigmaArb, sigmaLott = kwargs$sigmaLott, sigmaFrac = kwargs$sigmaFrac,
                          barrier = kwargs$barrier,nonDecisionTime = kwargs$nonDecisionTime, barrierDecay = kwargs$barrierDecay,
                          lotteryBias = kwargs$lotteryBias, timeStep = kwargs$timeStep,
                          maxIter = kwargs$maxIter,
                          epsilon = kwargs$epsilon,
                          EVLeft=stimuli$EVLeft[i], EVRight = stimuli$EVRight[i],
                          QVLeft = stimuli$QVLeft[i], QVRight= stimuli$QVRight[i],
                          probFractalDraw = stimuli$probFractalDraw[i])

    } else if(model_name %in% c("model2a")){
      cur_out = sim_trial(d=kwargs$d, sigma = kwargs$sigma, theta = kwargs$theta,
                          barrier = kwargs$barrier, nonDecisionTime = kwargs$nonDecisionTime, barrierDecay = kwargs$barrierDecay,
                          bias = kwargs$bias, timeStep = kwargs$timeStep, maxIter = kwargs$maxIter, epsilon = kwargs$epsilon,
                          stimDelay = kwargs$stimDelay,
                          EVLeft=stimuli$EVLeft[i], EVRight = stimuli$EVRight[i],
                          QVLeft = stimuli$QVLeft[i], QVRight= stimuli$QVRight[i] ,
                          probFractalDraw = stimuli$probFractalDraw[i])

    } else if(model_name %in% c("model2b")){
      cur_out = sim_trial(d=kwargs$d, sigma = kwargs$sigma, delta = kwargs$delta, gamma = kwargs$gamma,
                          barrier = kwargs$barrier, nonDecisionTime = kwargs$nonDecisionTime, barrierDecay = kwargs$barrierDecay,
                          bias = kwargs$bias, timeStep = kwargs$timeStep, maxIter = kwargs$maxIter, epsilon = kwargs$epsilon,
                          stimDelay = kwargs$stimDelay, recallDelay = kwargs$recallDelay,
                          EVLeft=stimuli$EVLeft[i], EVRight = stimuli$EVRight[i],
                          QVLeft = stimuli$QVLeft[i], QVRight= stimuli$QVRight[i] ,
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
  } else if(model_name %in% c("model4a", "model5a", "model6a", "model7a")){
    out$dArb = kwargs$dArb
    out$dLott = kwargs$dLott
    out$dFrac = kwargs$dFrac
    out$sigmaArb = kwargs$sigmaArb
    out$sigmaFrac = kwargs$sigmaFrac
  } else {
    out$d = kwargs$d
    out$sigma = kwargs$sigma
  }
  out$nonDecisionTime = kwargs$nonDecisionTime
  out$barrierDecay = kwargs$barrierDecay
  
  # Stop cluster  
  parallel::stopCluster(cl = my.cluster)
  rm(my.cluster)
  
  return(out)
}

