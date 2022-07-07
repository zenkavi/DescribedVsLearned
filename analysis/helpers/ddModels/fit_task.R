library(foreach)

# Parallelization setup based on this post
# https://www.blasbenito.com/post/02_parallelizing_loops_with_r/
# n.cores <- parallel::detectCores() - 1
n.cores <- 4

#create the cluster
my.fit.cluster <- parallel::makeCluster(
  n.cores, 
  type = "FORK"
)

#register it to be used by %dopar%
doParallel::registerDoParallel(cl = my.fit.cluster)

# Function to fit ddm model to data using a model provided as a string in the model_name argument
fit_task = function(data_, model_name_, pars_, fix_pars_ = list(), fit_trial_list_ = fit_trial_list, debug=FALSE){
   
  # Initialize any missing arguments. Some are useless defaults to make sure different fit_trial functions from different models can run without errors even if they don't make use of that argument
  if (!("d" %in% names(pars_))){
    pars_$d = 0
  }
  if (!("sigma" %in% names(pars_))){
    pars_$sigma = 1e-9
  }
  if (!("dArb" %in% names(pars_))){
    pars_$dArb = 0
  }
  if (!("dAttr" %in% names(pars_))){
    pars_$dAttr = 0
  }
  if (!("dLott" %in% names(pars_))){
    pars_$dLott = 0
  }
  if (!("dFrac" %in% names(pars_))){
    pars_$dFrac = 0
  }
  if (!("sigmaArb" %in% names(pars_))){
    pars_$sigmaArb = 1e-9
  }
  if (!("sigmaAttr" %in% names(pars_))){
    pars_$sigmaAttr = 1e-9
  }
  if (!("sigmaLott" %in% names(pars_))){
    pars_$sigmaLott = 1e-9
  }
  if (!("sigmaFrac" %in% names(pars_))){
    pars_$sigmaFrac = 1e-9
  }
  if (!("theta" %in% names(pars_))){
    pars_$theta = 0
  }
  if (!("delta" %in% names(pars_))){
    pars_$delta = 1
  }
  if (!("gamma" %in% names(pars_))){
    pars_$gamma = 1
  }
  if (!("nonDecisionTime" %in% names(pars_))){
    pars_$nonDecisionTime = 0
  }
  if (!("barrier" %in% names(pars_))){
    pars_$barrier = 1
  }
  if (!("barrierDecay" %in% names(pars_))){
    pars_$barrierDecay = 0
  }
  if (!("bias" %in% names(pars_))){
    pars_$bias = 0
  }
  if (!("lotteryBias" %in% names(pars_))){
    pars_$lotteryBias = 0.1
  }
  if (!("timeStep" %in% names(pars_))){
    pars_$timeStep = 10
  }
  if (!("maxIter" %in% names(pars_))){
    pars_$maxIter = 400
  }
  if (!("epsilon" %in% names(pars_))){
    pars_$epsilon = 0
  }
  if (!("stimDelay" %in% names(pars_))){
    pars_$stimDelay = 2000
  }
  
  # Extract the correct trial simulator for the model_name
  fit_trial = fit_trial_list_[[model_name_]]
  
  # Check if there are any pars to be fixed in the fix_pars_ list and replace them in pars_ if there are
  if(length(fix_pars_) > 0){
    for(i in 1:length(names(fix_pars_))){
      cur_fix_par = names(fix_pars_)[i]
      pars_[[cur_fix_par]] = fix_pars_[[cur_fix_par]]
    }
  }
  
  # Print arguments that will be used for simulation if in debug mode
  if(debug){
    print(paste0("Simulating task with parameters: model_name = ", model_name_,
                 ", barrier = ", pars_$barrier,
                 ", barrierDecay = ", pars_$barrierDecay,
                 ", bias = ", pars_$bias,
                 ", d = ", pars_$d,
                 ", dArb = ", pars_$dArb,
                 ", dAttr = ", pars_$dAttr,
                 ", dFrac = ", pars_$dFrac,
                 ", dLott = ", pars_$dLott,
                 ", delta = ", pars_$delta,
                 ", epsilon = ", pars_$epsilon,
                 ", gamma = ", pars_$gamma,
                 ", lotteryBias = ", pars_$lotteryBias,
                 ", maxIter = ", pars_$maxIter,
                 ", non-decision time = ", pars_$nonDecisionTime,
                 ", sigma = ", pars_$sigma,
                 ", sigmaArb = ", pars_$sigmaArb,
                 ", sigmaAttr = ", pars_$sigmaAttr,
                 ", sigmaFrac = ", pars_$sigmaFrac,
                 ", sigmaLott = ", pars_$sigmaLott,
                 ", stimDelay = ", pars_$stimDelay,
                 ", timeStep = ", pars_$timeStep,

    ))
  }
  
  # Parallel loop
  out <- foreach(
    EVLeft=data_$EVLeft, 
    EVRight = data_$EVRight, 
    QVLeft = data_$QVLeft, 
    QVRight= data_$QVRight , 
    probFractalDraw = data_$probFractalDraw,
    choice = data_$choice,
    reactionTime = data_$reactionTime,
    distortedEVDiff = data_$distortedEVDiff,
    distortedQVDiff = data_$distortedQVDiff,
    .combine = 'rbind'
  ) %dopar% {
    # Simulate RT and choice for a single trial with given DDM parameters and trial stimulus values
    fit_trial(d=pars_$d, sigma = pars_$sigma, 
              dArb=pars_$dArb, dAttr=pars_$dAttr, sigmaArb = pars_$sigmaArb, sigmaAttr = pars_$sigmaAttr,
              dLott=pars_$dLott, dFrac=pars_$dFrac, sigmaLott = pars_$sigmaLott, sigmaFrac = pars_$sigmaFrac,
              theta = pars_$theta, delta = pars_$delta, gamma = pars_$gamma,
              barrier = pars_$barrier, nonDecisionTime = pars_$nonDecisionTime, barrierDecay = pars_$barrierDecay,
              bias = pars_$bias, timeStep = pars_$timeStep, maxIter = pars_$maxIter, epsilon = pars_$epsilon,
              stimDelay = pars_$stimDelay,
              EVLeft=EVLeft, EVRight = EVRight, QVLeft = QVLeft, QVRight= QVRight, probFractalDraw = probFractalDraw, choice=choice, reactionTime = reactionTime,
              distortedEVDiff = distortedEVDiff, distortedQVDiff = distortedQVDiff)
    
  }
  
  # Add details of the parameters used for the simulation
  out$model = model_name_
  
  return(out)
}

# Usage in optim
# optim(par, get_task_nll, data_, par_names, model_name)
get_task_nll = function(data_, par_, par_names_, model_name_, fix_pars_, filter_liks_=FALSE, filter_quant_=.5){
  
  # Initialize parameters
  # Different models will have different sets of parameters. Optim will optimize over all the parameters it is passed in
  
  pars = setNames(as.list(par_), par_names_)
  
  # Get trial likelihoods for the stimuli using the initialized parameters
  out = fit_task(data_ = data_, model_name_ = model_name_, pars_ = pars, fix_pars_ = fix_pars_)
  
  if(filter_liks_){
    out = out %>%
      filter(likelihood>quantile(likelihood, probs=c(filter_quant_)))
  }
  
  nll = -sum(log(out$likelihood+1e-200))
  
  return(nll)
}

