library(foreach)

# Parallelization setup based on this post
# https://www.blasbenito.com/post/02_parallelizing_loops_with_r/
# n.cores <- parallel::detectCores() - 1
n.cores <- 4

#create the cluster
my.sim.cluster <- parallel::makeCluster(
  n.cores, 
  type = "FORK"
)

#register it to be used by %dopar%
doParallel::registerDoParallel(cl = my.sim.cluster)

# Function to simulate ddm process for a given set of stimuli using a model provided as a string in the model_name argument
sim_task = function(stimuli, model_name, sim_trial_list_ = sim_trial_list, ...){
  
  kwargs = list(...)
  # 
  # Initialize any missing arguments. Some are useless defaults to make sure different sim_trial functions from different models can run without errors even if they don't make use of that argument
  if (!("d" %in% names(kwargs))){
    kwargs$d = 0
  }
  if (!("sigma" %in% names(kwargs))){
    kwargs$sigma = 1e-9
  }
  if (!("dArb" %in% names(kwargs))){
    kwargs$dArb = 0
  }
  if (!("dAttr" %in% names(kwargs))){
    kwargs$dAttr = 0
  }
  if (!("dLott" %in% names(kwargs))){
    kwargs$dLott = 0
  }
  if (!("dFrac" %in% names(kwargs))){
    kwargs$dFrac = 0
  }
  if (!("sigmaArb" %in% names(kwargs))){
    kwargs$sigmaArb = 1e-9
  }
  if (!("sigmaAttr" %in% names(kwargs))){
    kwargs$sigmaAttr = 1e-9
  }
  if (!("sigmaLott" %in% names(kwargs))){
    kwargs$sigmaLott = 1e-9
  }
  if (!("sigmaFrac" %in% names(kwargs))){
    kwargs$sigmaFrac = 1e-9
  }
  if (!("theta" %in% names(kwargs))){
    kwargs$theta = 0
  }
  if (!("delta" %in% names(kwargs))){
    kwargs$delta = 1
  }
  if (!("gamma" %in% names(kwargs))){
    kwargs$gamma = 1
  }
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
    kwargs$epsilon = 0
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
  sim_trial = sim_trial_list_[[model_name]]
  
  # Print arguments that will be used for simulation if in debug mode
  if(kwargs$debug){
    print(paste0("Simulating task with parameters: model_name = ", model_name_,
                ", barrier = ", kwargs$barrier,
                ", barrierDecay = ", kwargs$barrierDecay,
                ", bias = ", kwargs$bias,
                ", d = ", kwargs$d,
                ", dArb = ", kwargs$dArb,
                ", dAttr = ", kwargs$dAttr,
                ", dFrac = ", kwargs$dFrac,
                ", dLott = ", kwargs$dLott,
                ", delta = ", kwargs$delta,
                ", epsilon = ", kwargs$epsilon,
                ", gamma = ", kwargs$gamma,
                ", lotteryBias = ", kwargs$lotteryBias,
                ", maxIter = ", kwargs$maxIter,
                ", non-decision time = ", kwargs$nonDecisionTime,
                ", sigma = ", kwargs$sigma,
                ", sigmaArb = ", kwargs$sigmaArb,
                ", sigmaAttr = ", kwargs$sigmaAttr,
                ", sigmaFrac = ", kwargs$sigmaFrac,
                ", sigmaLott = ", kwargs$sigmaLott,
                ", stimDelay = ", kwargs$stimDelay,
                ", timeStep = ", kwargs$timeStep,
                
    ))
  }
  
  #register it to be used by %dopar%
  # doParallel::registerDoParallel(cl = my.sim.cluster)
  
  if("distortedEVDiff" %in% names(stimuli)==FALSE){
    stimuli$distortedEVDiff = NA
  }
  
  if("distortedQVDiff" %in% names(stimuli)==FALSE){
    stimuli$distortedQVDiff = NA
  }
  
  # Parallel loop
  out <- foreach(
    EVLeft = stimuli$EVLeft, 
    EVRight = stimuli$EVRight, 
    QVLeft = stimuli$QVLeft, 
    QVRight = stimuli$QVRight , 
    probFractalDraw = stimuli$probFractalDraw,
    distortedEVDiff = stimuli$distortedEVDiff,
    distortedQVDiff = stimuli$distortedQVDiff,
    .combine = 'rbind'
  ) %dopar% {
    # Simulate RT and choice for a single trial with given DDM parameters and trial stimulus values
      sim_trial(d = kwargs$d, sigma = kwargs$sigma, 
                dArb = kwargs$dArb, dAttr=kwargs$dAttr, sigmaArb = kwargs$sigmaArb, sigmaAttr = kwargs$sigmaAttr,
                dLott = kwargs$dLott, dFrac = kwargs$dFrac, sigmaLott = kwargs$sigmaLott, sigmaFrac = kwargs$sigmaFrac,
                theta = kwargs$theta, delta = kwargs$delta, gamma = kwargs$gamma,
                barrier = kwargs$barrier, nonDecisionTime = kwargs$nonDecisionTime, barrierDecay = kwargs$barrierDecay,
                bias = kwargs$bias, timeStep = kwargs$timeStep, maxIter = kwargs$maxIter, epsilon = kwargs$epsilon,
                stimDelay = kwargs$stimDelay,
                EVLeft=EVLeft, EVRight = EVRight, QVLeft = QVLeft, QVRight= QVRight, probFractalDraw = probFractalDraw,
                distortedEVDiff = distortedEVDiff, distortedQVDiff = distortedQVDiff)

    }
  
  # parallel::stopCluster(cl = my.sim.cluster)
  
  # Add details of the parameters used for the simulation
  out$model = model_name

  return(out)
}

