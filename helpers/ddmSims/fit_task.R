library(foreach)

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

# Parallelization setup based on this post
# https://www.blasbenito.com/post/02_parallelizing_loops_with_r/
n.cores <- parallel::detectCores() - 1

#create the cluster
my.fit.cluster <- parallel::makeCluster(
  n.cores, 
  type = "FORK"
)

#check cluster definition (optional)
# print(my.cluster)

#register it to be used by %dopar%
doParallel::registerDoParallel(cl = my.fit.cluster)

#check if it is registered (optional)
# foreach::getDoParRegistered()

#how many workers are available? (optional)
# foreach::getDoParWorkers()


# Function to simulate ddm process for a given set of stimuli using a model provided as a string in the model_name argument
fit_task = function(stimuli, model_name_, pars_, fit_trial_list_ = fit_trial_list, debug=FALSE){
  
  # pars_ = list(...)
  
  # Unpack pars_ if pars_ are passed as a list
  # if(length(pars_) == 1){
  #   pars_ = pars_[[1]]
  # }
  
  # 
  # Initialize any missing arguments. Some are useless defaults to make sure different sim_trial functions from different models can run without errors even if they don't make use of that argument
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
    pars_$epsilon = 0.0002
  }
  if (!("stimDelay" %in% names(pars_))){
    pars_$stimDelay = 2000
  }
  if (!("debug" %in% names(pars_))){
    pars_$debug = FALSE
  }
  
  # Extract the correct trial simulator for the model_name
  fit_trial = fit_trial_list_[[model_name_]]
  
  # Print arguments that will be used for simulation if in debug mode
  if(pars_$debug){
    print(paste0("Simulating task with parameters: model_name = ", model_name_,
                 ", non-decision time = ", pars_$nonDecisionTime,
                 ", barrier = ", pars_$barrier,
                 ", barrierDecay = ", pars_$barrierDecay,
                 ", bias = ", pars_$bias,
                 ", lotteryBias = ", pars_$lotteryBias,
                 ", timeStep = ", pars_$timeStep,
                 ", maxIter = ", pars_$maxIter,
                 ", epsilon = ", pars_$epsilon,
                 ", stimDelay = ", pars_$stimDelay
    ))
  }
  
  # Parallel loop
  out <- foreach(
    EVLeft=stimuli$EVLeft, 
    EVRight = stimuli$EVRight, 
    QVLeft = stimuli$QVLeft, 
    QVRight= stimuli$QVRight , 
    probFractalDraw = stimuli$probFractalDraw,
    choice = stimuli$choice,
    reactionTime = stimuli$reactionTime,
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
              EVLeft=EVLeft, EVRight = EVRight, QVLeft = QVLeft, QVRight= QVRight, probFractalDraw = probFractalDraw, choice=choice, reactionTime = reactionTime)
    
  }
  
  
  # Add details of the parameters used for the simulation
  out$model = model_name_
  
  return(out)
}

# Usage in optim
# optim(par, get_task_nll, data, par_names, model_name)
get_task_nll = function(data, par, par_names, model_name){
  
  # Initialize parameters
  # Different models will have different sets of parameters. Optim will optimize over all the parameters it is passed in
  # There might be a way to use L-BFGS-B and set min's and max's for unused parameters to 0 to avoid optimizing over them but that seems equally bad/worse bc a. you need specify the bounds every time you're fitting anything, b. it won't work with any other algorithm
  
  pars = setNames(as.list(par), par_names)
  
  # Get trial likelihoods for the stimuli using the initialized parameters
  out = fit_task(stimuli = data, model_name_ = model_name, pars_ = pars)
  
  nll = -sum(log(out$likelihood+1e-200))
  
  return(nll)
}

