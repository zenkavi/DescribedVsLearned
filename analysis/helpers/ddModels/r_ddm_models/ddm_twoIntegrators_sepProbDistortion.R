sim_trial = function(dLott, dFrac, sigmaLott, sigmaFrac, barrierDecay=0, barrier=1, nonDecisionTime=0, bias=0.1, timeStep=10, maxIter=400, debug=FALSE,...){
  
  # d : drift rate
  # sigma: sd of the normal distribution 
  # timeStep: in ms
  # nonDecisionTime: in ms
  # maxIter: num max samples. if a barrier isn't hit by this sampling of evidence no decision is made. If time step is 10ms and maxIter is 1000 this would be a 10sec timeout maximum
  
  
  fractalRDV = 0
  lotteryRDV = 0
  time = 1
  elapsedNDT = 0
  choice = 0
  RT = NA
  
  timeOut = 0
  
  if(debug){
    debug_df = data.frame(time = 0, barrier = round(barrier[time], 3), lotteryRDV = round(lotteryRDV, 3), fractalRDV = round(fractalRDV, 3))
  }
  
  kwargs = list(...)
  
  # Keep these for output and the sim_sanity_checks function
  EVRight = kwargs$EVRight
  EVLeft = kwargs$EVLeft
  QVRight = kwargs$QVRight
  QVLeft = kwargs$QVLeft
  probFractalDraw=kwargs$probFractalDraw
  
  distortedEVDiff = kwargs$distortedEVDiff
  distortedQVDiff = kwargs$distortedQVDiff
  # rawQVDiff = QVLeft - QVRight
  
  # pFrac == 1 bias
  # if(probFractalDraw == 1){
    # fractalRDV = distortedQVDiff #when using distorted QV diff these choices were not as fast as true data
    # fractalRDV = rawQVDiff
  # }
  
  nonDecIters = nonDecisionTime / timeStep
  
  initialBarrier = barrier
  barrier = rep(initialBarrier, maxIter)
  
  # The values of the barriers can change over time
  for(t in seq(1, maxIter, 1)){
    barrier[t] = initialBarrier / (1 + barrierDecay * t)
  }
  
  lottery_mu = dLott * distortedEVDiff
  fractal_mu = dFrac * distortedQVDiff
  
  while (time<maxIter){
    
    # If the either RDV hits one of the barriers make decision
    if (lotteryRDV >= barrier[time] | lotteryRDV <= -barrier[time] | fractalRDV >= barrier[time] | fractalRDV <= -barrier[time]){
      
      # Convert ms back to secs
      RT = (time * timeStep)/1000 
      
      if (lotteryRDV >= barrier[time] | fractalRDV >= barrier[time]){
        choice = "left"
      } else if (lotteryRDV <= -barrier[time] | fractalRDV <= -barrier[time]){
        choice = "right"
      }
      break
    } 
    
    # Otherwise continue sampling evidence
    if (elapsedNDT < nonDecIters){
      elapsedNDT = elapsedNDT + 1
    } else{
      
      # Two integrators
      
      # Lottery integrator
      lotteryRDV = lotteryRDV + rnorm(1, lottery_mu, sigmaLott)
      
      # Fractal integrator
      fractalRDV = fractalRDV + rnorm(1, fractal_mu, sigmaFrac)
      
    }
    
    if (debug){
      debug_row = data.frame(time = time, barrier = round(barrier[time], 3), lotteryRDV = round(lotteryRDV, 3), fractalRDV = round(fractalRDV, 3))
      debug_df = rbind(debug_df, debug_row)
    }
    
    # Increment sampling iteration
    time = time + 1
  }
  
  #If a choice hasn't been made by the time limit
  if(is.na(RT)){
    # Choose whatever you have most evidence for
    if(abs(lotteryRDV) > abs(fractalRDV)){
      choice = ifelse(lotteryRDV > 0, "left", "right")
    } else if (abs(lotteryRDV) < abs(fractalRDV)){
      choice = ifelse(fractalRDV > 0, "left", "right")
    }
    
    timeOut = 1
    RT=rlnorm(1, mean = 1.25, sd = 0.1)
  }
  
  #Organize output 
  out = data.frame(EVLeft = EVLeft, EVRight = EVRight, QVLeft = QVLeft, QVRight = QVRight, distortedEVDiff = distortedEVDiff, distortedQVDiff = distortedQVDiff, probFractalDraw = probFractalDraw, choice=choice, reactionTime = RT, timeOut = timeOut, dLott=dLott, dFrac=dFrac, sigmaLott=sigmaLott, sigmaFrac=sigmaFrac, barrierDecay=barrierDecay, barrier=barrier[time], nonDecisionTime=nonDecisionTime, bias=bias, timeStep=timeStep, maxIter=maxIter)
  
  if(debug){
    # out = list(out=out, debug_df=debug_df[-1,])
    out = list(out=out, debug_df=debug_df)
  }
  
  return(out)
}


fit_trial = function(dLott, dFrac, sigmaLott, sigmaFrac, barrierDecay=0, barrier=1, nonDecisionTime=0, bias=0, timeStep=10, approxStateStep = 0.1, debug=FALSE, epsilon=0, ...){
  
  
  kwargs = list(...)
  
  choice=kwargs$choice #must be 1 for left and -1 for left
  if(choice == "left"){
    choice = 1
  } else if (choice == "right"){
    choice = -1
  }
  reactionTime=kwargs$reactionTime #in ms
  if(reactionTime < 100){
    reactionTime = reactionTime *1000
  }
  distortedEVDiff = kwargs$distortedEVDiff
  distortedQVDiff = kwargs$distortedQVDiff
  probFractalDraw = kwargs$probFractalDraw
  QVLeft = kwargs$QVLeft
  QVRight = kwargs$QVRight
  
  nonDecIters = nonDecisionTime / timeStep
  
  numTimeSteps = round(reactionTime / timeStep)
  numTimeSteps = numTimeSteps - nonDecIters
  
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
  
  #### !!!!!!!!!! IMPORTANT !!!!!!!!!! ####
  # p(bottom boundary) is the first value! Don't get confused by seeing it at the top 
  #### !!!!!!!!!! IMPORTANT !!!!!!!!!! ####
  prStatesLott = matrix(data = 0, nrow = length(states), ncol = numTimeSteps)
  prStatesLott[biasState,1] = 1
  
  prStatesFrac = matrix(data = 0, nrow = length(states), ncol = numTimeSteps)
  prStatesFrac[biasState,1] = 1
  
  # The probability of crossing each barrier over the time of the trial.
  probUpCrossingLott = rep(0, numTimeSteps)
  probDownCrossingLott = rep(0, numTimeSteps)
  
  probUpCrossingFrac = rep(0, numTimeSteps)
  probDownCrossingFrac = rep(0, numTimeSteps)
  
  # Rows of these matrices correspond to array elements in python
  
  # How much change is required from each state to move onto every other state. From the smallest state (bottom boundary) to the largest state (top boundary)
  changeMatrix = matrix(data = states, ncol=length(states), nrow=length(states), byrow=FALSE) - matrix(data = states, ncol=length(states), nrow=length(states), byrow=TRUE)
  
  # How much change is required from each state to cross the up or down barrier at each time point
  changeUp = matrix(data = barrier, ncol=numTimeSteps, nrow=length(states), byrow=TRUE) - matrix(data = states, ncol=numTimeSteps, nrow=length(states), byrow=FALSE)
  changeDown = matrix(data = -barrier, ncol=numTimeSteps, nrow=length(states), byrow=TRUE) - matrix(data = states, ncol=numTimeSteps, nrow=length(states), byrow=FALSE)
  
  elapsedNDT = 0
  
  muLott_mean = dLott * distortedEVDiff
  muFrac_mean = dFrac * distortedQVDiff
  # fracBias = 0
  # if (probFractalDraw == 1){
    # fracBias = QVLeft - QVRight
  # } 
  
  # LOOP of state probability updating up to reaction time
  
  # Start at 2 to match python indexing that starts at 0
  for(nextTime in 2:numTimeSteps){
    curTime = nextTime - 1 
    
    
    if (elapsedNDT < nonDecIters){
      muLott = 0
      muFrac = 0
      elapsedNDT = elapsedNDT + 1
    } else{
      muLott = muLott_mean
      muFrac = muFrac_mean
    }
    
    
    # Update the probability of the states that remain inside the
    # barriers. The probability of being in state B is the sum, over
    # all states A, of the probability of being in A at the previous
    # time step times the probability of changing from A to B. We
    # multiply the probability by the stateStep to ensure that the area
    # under the curves for the probability distributions probUpCrossing
    # and probDownCrossing add up to 1.
    # If there is barrier decay and there are next states that are cross
    # the decayed barrier set their probabilities to 0.
    prStatesLottNew = (stateStep * (dnorm(changeMatrix, muLott, sigmaLott) %*% prStatesLott[,curTime]) )
    prStatesLottNew[states >= barrier[nextTime] | states <= -barrier[nextTime]] = 0
    
    prStatesFracNew = (stateStep * (dnorm(changeMatrix, muFrac, sigmaFrac) %*% prStatesLott[,curTime]) )
    prStatesFracNew[states >= barrier[nextTime] | states <= -barrier[nextTime]] = 0
    
    # Calculate the probabilities of crossing the up barrier and the
    # down barrier. This is given by the sum, over all states A, of the
    # probability of being in A at the previous timestep times the
    # probability of crossing the barrier if A is the previous state.
    tempUpCrossLott = (prStatesLott[,curTime] %*% (1 - pnorm(changeUp[,nextTime], muLott, sigmaLott)))[1]
    tempDownCrossLott = (prStatesLott[,curTime] %*% (pnorm(changeDown[,nextTime], muLott, sigmaLott)))[1]
    
    tempUpCrossFrac = (prStatesFrac[,curTime] %*% (1 - pnorm(changeUp[,nextTime], muFrac, sigmaFrac)))[1]
    tempDownCrossFrac = (prStatesFrac[,curTime] %*% (pnorm(changeDown[,nextTime], muFrac, sigmaFrac)))[1]
    
    # Renormalize to cope with numerical approximations.
    sumIn = sum(prStatesLott[,curTime])
    sumCurrent = sum(prStatesLottNew) + tempUpCrossLott + tempDownCrossLott
    prStatesLottNew = prStatesLottNew * sumIn / sumCurrent
    tempUpCrossLott = tempUpCrossLott * sumIn / sumCurrent
    tempDownCrossLott = tempDownCrossLott * sumIn / sumCurrent
    
    sumIn = sum(prStatesFrac[,curTime])
    sumCurrent = sum(prStatesFracNew) + tempUpCrossFrac + tempDownCrossFrac
    prStatesFracNew = prStatesFracNew * sumIn / sumCurrent
    tempUpCrossFrac = tempUpCrossFrac * sumIn / sumCurrent
    tempDownCrossFrac = tempDownCrossFrac * sumIn / sumCurrent
    
    
    # Avoid NAs for likelihood conditional statements
    if (is.na(tempUpCrossLott)){
      tempUpCrossLott = 0
    }
    if (is.na(tempDownCrossLott)){
      tempDownCrossLott = 0
    }
    if (is.na(tempUpCrossFrac)){
      tempUpCrossFrac = 0
    }
    if (is.na(tempDownCrossFrac)){
      tempDownCrossFrac = 0
    }
    
    # Update the probabilities of each state and the probabilities of
    # crossing each barrier at this timestep.
    prStatesLott[, nextTime] = prStatesLottNew
    probUpCrossingLott[nextTime] = tempUpCrossLott
    probDownCrossingLott[nextTime] = tempDownCrossLott
    
    prStatesFrac[, nextTime] = prStatesFracNew
    probUpCrossingFrac[nextTime] = tempUpCrossFrac
    probDownCrossingFrac[nextTime] = tempDownCrossFrac
  }
  
  likelihood = 0
  if (choice == 1){ # Choice was left.
    
    # p of left is the p of either the lottery OR (+) the fractal integrator crossing the top boundary
    #... need to think about normalizing the probabilities for boundary crossing using both integrators...
    likelihood = probUpCrossingLott[numTimeSteps] + probUpCrossingFrac[numTimeSteps]
    
  } else if (choice == -1){
    
    likelihood = probDownCrossingLott[numTimeSteps] + probDownCrossingFrac[numTimeSteps]
    
  }
  
  out = data.frame(likelihood = likelihood, distortedEVDiff = distortedEVDiff, distortedQVDiff = distortedQVDiff, probFractalDraw = probFractalDraw, choice = choice, reactionTime = reactionTime, dLott = dLott, dFrac = dFrac, sigmaLott = sigmaLott, sigmaFrac = sigmaFrac, barrierDecay = barrierDecay, barrier = barrier[numTimeSteps], nonDecisionTime = nonDecisionTime, bias = bias, timeStep = timeStep)
  
  if(debug){
    out = list(out = out, prStatesLott = data.frame(prStatesLott), prStatesFrac = data.frame(prStatesFrac))
  }
  
  
  return(out)
  
}
