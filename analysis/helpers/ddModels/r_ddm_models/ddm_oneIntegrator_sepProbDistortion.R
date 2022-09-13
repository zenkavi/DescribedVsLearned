sim_trial = function(d, sigma, barrierDecay=0, barrier=1, nonDecisionTime=0, bias=0, timeStep=10, maxIter=400, epsilon = 0, debug=FALSE,...){
  
  # d : drift rate
  # sigma: sd of the normal distribution 
  # timeStep: in ms
  # nonDecisionTime: in ms
  # maxIter: num max samples. if a barrier isn't hit by this sampling of evidence no decision is made. If time step is 10ms and maxIter is 1000 this would be a 10sec timeout maximum
  
  if (debug){
    debug_df = data.frame(time = 0, mu_mean =NA, mu =NA, RDV = 0, barrier = barrier)
  }
  
  RDV = bias
  time = 1
  elapsedNDT = 0
  choice = 0
  RT = NA
  
  timeOut = 0
  
  kwargs = list(...)
  
  EVLeft=kwargs$EVLeft
  EVRight=kwargs$EVRight
  QVLeft=kwargs$QVLeft
  QVRight=kwargs$QVRight
  probFractalDraw=kwargs$probFractalDraw
  
  distortedEVDiff = kwargs$distortedEVDiff
  distortedQVDiff = kwargs$distortedQVDiff
  
  nonDecIters = nonDecisionTime / timeStep
  
  initialBarrier = barrier
  barrier = rep(initialBarrier, maxIter)
  
  # The values of the barriers can change over time
  # Barrier decay starts after stim presentation. Not during any possible sampling before that
  for(t in seq(2, maxIter, 1)){
    barrier[t] = initialBarrier / (1 + (barrierDecay * t))
  }
  
  # distortedProbFractalDraw = (delta * (probFractalDraw)) / ( (delta * (probFractalDraw)) + (1-probFractalDraw))
  # leftFractalAdv =  distortedProbFractalDraw * (QVLeft - QVRight)
  # leftLotteryAdv = (1-probFractalDraw) * (EVLeft - EVRight)
  # weighted_mu_mean = d * (leftFractalAdv + leftLotteryAdv)
  weighted_mu_mean = d * (distortedEVDiff + distortedQVDiff)
  
  while (time<maxIter){
    
    # If the RDV hit one of the barriers, the trial is over.
    if (RDV >= barrier[time] | RDV <= -barrier[time]){
      
      # Convert ms back to secs
      RT = (time * timeStep)/1000 
      
      if (RDV >= barrier[time]){
        choice = "left"
      } else if (RDV <= -barrier[time]){
        choice = "right"
      }
      break
    } 
    
    
    if (elapsedNDT < nonDecIters){
      mu_mean = 0
      elapsedNDT = elapsedNDT + 1
    } else{
      mu_mean = weighted_mu_mean
    }
    
    # Sample the change in RDV from the distribution.
    mu = rnorm(1, mu_mean, epsilon)
    RDV = RDV + rnorm(1, mu, sigma)
    
    if (debug){
      debug_row = data.frame(time = time, mu_mean = mu_mean, mu = round(mu, 3), RDV = round(RDV, 3), barrier = round(barrier[time], 3))
      debug_df = rbind(debug_df, debug_row)
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
  out = data.frame(EVLeft = EVLeft, EVRight = EVRight, QVLeft = QVLeft, QVRight = QVRight, probFractalDraw = probFractalDraw, distortedEVDiff = distortedEVDiff, distortedQVDiff = distortedQVDiff, choice=choice, reactionTime = RT, timeOut = timeOut, d = d, sigma = sigma, barrierDecay = barrierDecay, barrier=barrier[time], nonDecisionTime=nonDecisionTime, bias=bias, timeStep=timeStep, maxIter=maxIter, epsilon = epsilon)
  
  if(debug){
    return(list(out=out, debug_df = debug_df))
  } else {
    return(out)
  }
}

fit_trial = function(d, sigma, barrierDecay=0, barrier=1, nonDecisionTime=0, bias=0, timeStep=10, epsilon = 0, approxStateStep = 0.1, debug=FALSE, ...){
  
  # RDV = bias
  
  kwargs = list(...)
  
  choice=kwargs$choice #must be 1 for left and -1 for left
  if(choice == "left" | choice == 1){
    choice = 1
  } else if (choice == "right" | choice == 0){
    choice = -1
  }
  reactionTime=kwargs$reactionTime #in ms
  if(reactionTime < 100){
    reactionTime = reactionTime *1000
  }
  EVLeft=kwargs$EVLeft
  EVRight=kwargs$EVRight
  QVLeft=kwargs$QVLeft
  QVRight=kwargs$QVRight
  probFractalDraw=kwargs$probFractalDraw
  
  distortedEVDiff = kwargs$distortedEVDiff
  distortedQVDiff = kwargs$distortedQVDiff

  nonDecIters = nonDecisionTime / timeStep
  
  numTimeSteps = floor(reactionTime / timeStep)
  
  initialBarrier = barrier
  barrier = rep(initialBarrier, numTimeSteps)
  
  # The values of the barriers can change over time
  for(t in seq(2, numTimeSteps, 1)){
    barrier[t] = initialBarrier / (1 + (barrierDecay * (t-1)) )
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
  # p(bottom boundary) is the first value! Don't get confused by seeing it at the top 
  prStates = matrix(data = 0, nrow = length(states), ncol = numTimeSteps)
  prStates[biasState,1] = 1
  
  # The probability of crossing each barrier over the time of the trial.
  probUpCrossing = rep(0, numTimeSteps)
  probDownCrossing = rep(0, numTimeSteps)
  
  # Rows of these matrices correspond to array elements in python
  
  # How much change is required from each state to move onto every other state. From the smallest state (bottom boundary) to the largest state (top boundary)
  changeMatrix = matrix(data = states, ncol=length(states), nrow=length(states), byrow=FALSE) - matrix(data = states, ncol=length(states), nrow=length(states), byrow=TRUE)
  
  # How much change is required from each state to cross the up or down barrier at each time point
  changeUp = matrix(data = barrier, ncol=numTimeSteps, nrow=length(states), byrow=TRUE) - matrix(data = states, ncol=numTimeSteps, nrow=length(states), byrow=FALSE)
  changeDown = matrix(data = -barrier, ncol=numTimeSteps, nrow=length(states), byrow=TRUE) - matrix(data = states, ncol=numTimeSteps, nrow=length(states), byrow=FALSE)
  
  elapsedNDT = 0
  
  # distortedProbFractalDraw = (delta * (probFractalDraw)) / ( (delta * (probFractalDraw)) + (1-probFractalDraw))
  # leftFractalAdv =  distortedProbFractalDraw * (QVLeft - QVRight)
  # leftLotteryAdv = (1-probFractalDraw) * (EVLeft - EVRight)
  # weighted_mu_mean = d * (leftFractalAdv + leftLotteryAdv)
  weighted_mu_mean = d * (distortedEVDiff + distortedQVDiff)
  
  # LOOP of state probability updating up to reaction time
  
  # Start at 2 to match python indexing that starts at 0
  for(nextTime in 2:numTimeSteps){
    curTime = nextTime - 1 
    if (elapsedNDT < nonDecIters){
      mu_mean = 0
      elapsedNDT = elapsedNDT + 1
    } else{
      mu_mean = weighted_mu_mean
    }
    
    mu = rnorm(1, mu_mean, epsilon)
    # print(mu)
    
    # Update the probability of the states that remain inside the
    # barriers. The probability of being in state B is the sum, over
    # all states A, of the probability of being in A at the previous
    # time step times the probability of changing from A to B. We
    # multiply the probability by the stateStep to ensure that the area
    # under the curves for the probability distributions probUpCrossing
    # and probDownCrossing add up to 1.
    # If there is barrier decay and there are next states that are cross
    # the decayed barrier set their probabilities to 0.
    prStatesNew = (stateStep * (dnorm(changeMatrix, mu, sigma) %*% prStates[,curTime]) )
    prStatesNew[states >= barrier[nextTime] | states <= -barrier[nextTime]] = 0
    
    # Calculate the probabilities of crossing the up barrier and the
    # down barrier. This is given by the sum, over all states A, of the
    # probability of being in A at the previous timestep times the
    # probability of crossing the barrier if A is the previous state.
    tempUpCross = (prStates[,curTime] %*% (1 - pnorm(changeUp[,nextTime], mu, sigma)))[1]
    tempDownCross = (prStates[,curTime] %*% (pnorm(changeDown[,nextTime], mu, sigma)))[1]
    
    # Renormalize to cope with numerical approximations.
    sumIn = sum(prStates[,curTime])
    sumIn = ifelse(is.numeric(sumIn), sumIn, 0)
    sumIn = ifelse(is.nan(sumIn), 0, sumIn)
    sumCurrent = sum(prStatesNew) + tempUpCross + tempDownCross
    sumCurrent = ifelse(is.numeric(sumCurrent), sumCurrent, 0)
    sumCurrent = ifelse(is.nan(sumCurrent), 0, sumCurrent)
    
    if( (sumIn>0) && (sumCurrent>0) ){ #to avoid division by 0 and following NaNs
      prStatesNew = prStatesNew * sumIn / sumCurrent
      tempUpCross = tempUpCross * sumIn / sumCurrent
      tempDownCross = tempDownCross * sumIn / sumCurrent
    } else{
      prStatesNew = rep(0, length(states))
      tempUpCross = 0
      tempDownCross = 0
    }
    
    # Avoid NAs for likelihood conditional statements
    if (is.na(tempUpCross)){
      tempUpCross = 0
    }
    if (is.na(tempDownCross)){
      tempDownCross = 0
    }
    
    # Update the probabilities of each state and the probabilities of
    # crossing each barrier at this timestep.
    prStates[, nextTime] = prStatesNew
    probUpCrossing[nextTime] = tempUpCross
    probDownCrossing[nextTime] = tempDownCross
  }
  
  likelihood = 0
  if (choice == 1){ # Choice was left.
    if (probUpCrossing[numTimeSteps] > 0){
      likelihood = probUpCrossing[numTimeSteps]
    }
  } else if (choice == -1){
    if(probDownCrossing[numTimeSteps] > 0){
      likelihood = probDownCrossing[numTimeSteps]
    } 
  }
  
  out = data.frame(likelihood = likelihood, EVLeft = EVLeft, EVRight = EVRight, QVLeft = QVLeft, QVRight = QVRight, probFractalDraw = probFractalDraw,distortedEVDiff = distortedEVDiff, distortedQVDiff = distortedQVDiff, choice=choice, reactionTime = reactionTime, d = d, sigma = sigma, barrierDecay = barrierDecay, barrier=barrier[numTimeSteps], nonDecisionTime=nonDecisionTime, bias=bias, timeStep=timeStep, epsilon = epsilon)
  
  
  return(out)
  
}
