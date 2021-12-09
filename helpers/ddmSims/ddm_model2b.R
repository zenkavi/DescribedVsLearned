sim_trial = function(d, sigma, barrierDecay, delta, gamma, barrier=1, nonDecisionTime=0, bias=0, timeStep=10, maxIter=400, epsilon = 0.0002, stimDelay = 2000, debug=FALSE,,...){
  
  # d : drift rate
  # sigma: sd of the normal distribution 
  # timeStep: in ms
  # nonDecisionTime: in ms
  # maxIter: num max samples. if a barrier isn't hit by this sampling of evidence no decision is made. If time step is 10ms and maxIter is 1000 this would be a 10sec timeout maximum
  
  if (debug){
    debug_df = data.frame()
  }
  
  RDV = bias
  time = 1
  elapsedNDT = 0
  choice = 0
  RT = NA
  
  decPreStim = 0
  timeOut = 0
  
  kwargs = list(...)
  
  EVLeft=kwargs$EVLeft
  EVRight=kwargs$EVRight
  QVLeft=kwargs$QVLeft
  QVRight=kwargs$QVRight
  probFractalDraw=kwargs$probFractalDraw
  # Stimulus screen comes on 2 secs after the presentation of probFractalDraw
  stimDelayIters = (stimDelay / timeStep)
  nonDecIters = nonDecisionTime / timeStep
  
  # Integration starts before stim presentation (though meaningful move from 0 happens only for pFrac = 1 trials) but decision can only be indicated after stim. Since total iterations depend on maxIter the addition of iterations before stim presentation controls for the desired max time out for the trial. In the arguments to the function it is specified as the maximum time out duration after stim presentation
  maxIter = maxIter + stimDelayIters
  
  initialBarrier = barrier
  barrier = rep(initialBarrier, maxIter)
  
  # The values of the barriers can change over time
  # Barrier decay starts after stim presentation. Not during any possible sampling before that
  for(t in seq(stimDelayIters, maxIter, 1)){
    barrier[t] = initialBarrier / (1 + barrierDecay * (t-stimDelayIters))
  }
  
  qv_mu_mean = d*(QVLeft - QVRight)
  
  distortedProbFractalDraw = exp((-1)*delta*((-1)*log(probFractalDraw))^gamma)
  leftFractalAdv =  distortedProbFractalDraw * (QVLeft - QVRight)
  leftLotteryAdv = (1-probFractalDraw) * (EVLeft - EVRight)
  weighted_mu_mean = d * (leftFractalAdv + leftLotteryAdv)
  
  while (time<maxIter){
    
    # If the RDV hit one of the barriers, the trial is over.
    if (RDV >= barrier[time] | RDV <= -barrier[time]){
      
      # Convert ms back to secs
      RT = (time * timeStep)/1000 
      
      # Debugging
      if(debug){
        print(paste0("pre subtraction RT = ", RT))
      }
      
      #subtract stimDelay
      RT = RT - (stimDelay/1000) 
      
      # If decision is reached before the stim screen sample rt from log normal distribution similar to choice RT 
      if (RT < 0){
        decPreStim = 1
        RT=rlnorm(1, mean = -.25, sd = 0.5)
      }
      
      if (RDV >= barrier[time]){
        choice = "left"
      } else if (RDV <= -barrier[time]){
        choice = "right"
      }
      break
    } 
    
    if (time < stimDelayIters){
      if (probFractalDraw == 1){
        mu_mean = qv_mu_mean
      } else {
        # No integration before stim presentation for any other trial type
        mu_mean = 0
      }
    } else{
      if (elapsedNDT < nonDecIters){
        mu_mean = 0
        elapsedNDT = elapsedNDT + 1
      } else{
        mu_mean = weighted_mu_mean
      }
    }
    
    # Sample the change in RDV from the distribution.
    mu = rnorm(1, mu_mean, epsilon)
    RDV = RDV + rnorm(1, mu, sigma)
    
    if (debug){
      debug_row = data.frame(time = time, mu_mean = mu_mean, mu = round(mu, 3), RDV = round(RDV, 3), barrier = round(barrier[time], 3))
      debug_df = rbind.all.columns(debug_df, debug_row)
    }
    
    # Increment sampling iteration
    time = time + 1
  }
  
  #If a choice hasn't been made by the time limit
  if(is.na(RT)){
    # Choose whatever you have most evidence for
    if (RDV >= 0){
      choice = "left"
    } else if (RDV <= 0){
      choice = "right"
    }
    if(debug){
      print("Max iterations reached.")
    }
    timeOut = 1
    RT=rlnorm(1, mean = 1.25, sd = 0.1)
  }
  
  #Organize output 
  out = data.frame(EVLeft = EVLeft, EVRight = EVRight, QVLeft = QVLeft, QVRight = QVRight, probFractalDraw = probFractalDraw, choice=choice, reactionTime = RT, timeOut = timeOut, decPreStim = decPreStim, d = d, sigma = sigma, barrierDecay = barrierDecay, delta=delta, gamma=gamma, barrier=barrier[time], nonDecisionTime=nonDecisionTime, bias=bias, timeStep=timeStep, maxIter=maxIter, epsilon = epsilon, stimDelay = stimDelay)
  
  if(debug){
    return(list(out=out, debug_df = debug_df[-1,]))
  } else {
    return(out)
  }
}

fit_trial = function(d, sigma, barrierDecay, delta, gamma, barrier=1, nonDecisionTime=0, bias=0, timeStep=10, epsilon = 0.0002, stimDelay = 2000, approxStateStep = 0.1, ...){
  
  RDV = bias

  kwargs = list(...)
  
  choice=kwargs$choice #must be 1 for left and -1 for left
  reactionTime=kwargs$reactionTime #in ms
  EVLeft=kwargs$EVLeft
  EVRight=kwargs$EVRight
  QVLeft=kwargs$QVLeft
  QVRight=kwargs$QVRight
  probFractalDraw=kwargs$probFractalDraw
  # Stimulus screen comes on 2 secs after the presentation of probFractalDraw
  stimDelayIters = (stimDelay / timeStep)
  nonDecIters = nonDecisionTime / timeStep
  
  numTimeSteps = round(reactionTime / timeStep)
  numTimeSteps = numTimeSteps + stimDelayIters
  
  initialBarrier = barrier
  barrier = rep(initialBarrier, numTimeSteps)
  
  # The values of the barriers can change over time
  for(t in seq(1, numTimeSteps, 1)){
    barrier[t] = initialBarrier / (1 + (barrierDecay * t))
  }
  
  # Obtain correct state step.
  halfNumStateBins = round(initialBarrier / approxStateStep)
  stateStep = initialBarrier / (halfNumStateBins + 0.5)
  
  # The vertical axis is divided into states.
  states = seq(-1*(initialBarrier) + (stateStep / 2), initialBarrier - (stateStep / 2), stateStep)
  
  # Find the state corresponding to the bias parameter.
  biasState = which.min(abs(states - bias))
  
  # Initial probability for all states is zero, except the bias state,
  # for which the initial probability is one.
  prStates = matrix(data = 0, nrow = length(states), ncol = numTimeSteps)
  prStates[biasState,1] = 1
  
  # The probability of crossing each barrier over the time of the trial.
  probUpCrossing = rep(0, numTimeSteps)
  probDownCrossing = rep(0, numTimeSteps)
  
  # Rows of these matrices correspond to array elements in python
  changeMatrix = matrix(data = states, ncol=length(states), nrow=length(states), byrow=FALSE) - matrix(data = states, ncol=length(states), nrow=length(states), byrow=TRUE)
  changeUp = matrix(data = barrier, ncol=numTimeSteps, nrow=length(states), byrow=FALSE) - matrix(data = states, ncol=numTimeSteps, nrow=length(states), byrow=FALSE)
  changeDown = matrix(data = -barrier, ncol=numTimeSteps, nrow=length(states), byrow=FALSE) - matrix(data = states, ncol=numTimeSteps, nrow=length(states), byrow=FALSE)
  
  elapsedNDT = 0
  
  for(time in 1:numTimeSteps){
    
  }
  
  qv_mu_mean = d*(QVLeft - QVRight)
  
  distortedProbFractalDraw = exp((-1)*delta*((-1)*log(probFractalDraw))^gamma)
  leftFractalAdv =  distortedProbFractalDraw * (QVLeft - QVRight)
  leftLotteryAdv = (1-probFractalDraw) * (EVLeft - EVRight)
  weighted_mu_mean = d * (leftFractalAdv + leftLotteryAdv)
  
  out = data.frame(likelihood = likelihood, EVLeft = EVLeft, EVRight = EVRight, QVLeft = QVLeft, QVRight = QVRight, probFractalDraw = probFractalDraw, choice=choice, reactionTime = RT, d = d, sigma = sigma, barrierDecay = barrierDecay, delta=delta, gamma=gamma, barrier=barrier[time], nonDecisionTime=nonDecisionTime, bias=bias, timeStep=timeStep, epsilon = epsilon, stimDelay = stimDelay)
  
}