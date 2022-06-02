sim_trial = function(dArb, dLott, dFrac, sigmaArb, sigmaLott, sigmaFrac, barrierDecay, delta, gamma, barrier=1, nonDecisionTime=0, lotteryBias=0.1, timeStep=10, maxIter=400, epsilon = 0.002, debug=FALSE,...){
  
  # d : drift rate
  # sigma: sd of the normal distribution 
  # timeStep: in ms
  # nonDecisionTime: in ms
  # maxIter: num max samples. if a barrier isn't hit by this sampling of evidence no decision is made. If time step is 10ms and maxIter is 1000 this would be a 10sec timeout maximum
  
  
  arbitratorRDV = lotteryBias
  fractalRDV = 0
  lotteryRDV = 0
  time = 1
  elapsedNDT = 0
  choice = 0
  RT = NA
  
  timeOut = 0
  
  arbitrator_mu_mean = NA
  if(debug){
    debug_df = data.frame()
  }
  
  kwargs = list(...)
  EVLeft=kwargs$EVLeft
  EVRight=kwargs$EVRight
  
  QVLeft=kwargs$QVLeft
  QVRight=kwargs$QVRight
  
  # pFrac == 1 bias
  probFractalDraw=kwargs$probFractalDraw
  if(probFractalDraw == 1){
    fractalRDV = QVLeft - QVRight
  }
  
  nonDecIters = nonDecisionTime / timeStep
  
  initialBarrier = barrier
  barrier = rep(initialBarrier, maxIter)
  
  # The values of the barriers can change over time
  for(t in seq(1, maxIter, 1)){
    barrier[t] = initialBarrier / (1 + barrierDecay * t)
  }
  
  lottery_mu_mean = dLott * (1-probFractalDraw) * (EVLeft - EVRight)
  
  distortedProbFractalDraw = exp((-1)*delta*((-1)*log(probFractalDraw))^gamma)
  fractal_mu_mean = dFrac * distortedProbFractalDraw * (QVLeft - QVRight)
  
  while (time<maxIter){
    
    # If the arbitrator RDV hits one of the barriers make decision
    if (arbitratorRDV >= barrier[time] | arbitratorRDV <= -barrier[time]){
      
      # Convert ms back to secs
      RT = (time * timeStep)/1000 
      
      if (arbitratorRDV >= barrier[time]){
        arbitrator = "EV"
        choice = ifelse(lotteryRDV > 0, "left", "right")
      } else if (arbitratorRDV <= -barrier[time]){
        arbitrator = "QV"
        choice = ifelse(fractalRDV > 0, "left", "right")
      }
      break
    } 
    
    # Otherwise continue sampling evidence
    if (elapsedNDT < nonDecIters){
      mu_mean = 0
      elapsedNDT = elapsedNDT + 1
    } else{
      
      # Three integrators
      
      # Lottery integrator
      lottery_mu = rnorm(1, lottery_mu_mean, epsilon)
      lotteryRDV = lotteryRDV + rnorm(1, lottery_mu, sigmaLott)
      
      # Fractal integrator
      fractal_mu = rnorm(1, fractal_mu_mean, epsilon)
      fractalRDV = fractalRDV + rnorm(1, fractal_mu, sigmaFrac)
      
      # Arbitrator 
      # If abs(lotteryRDV) > abs(fractalRDV) stronger relative preference for a side based on lotteries
      arbitrator_mu_mean = dArb * (abs(lotteryRDV) - abs(fractalRDV))
      arbitrator_mu = rnorm(1, arbitrator_mu_mean, epsilon)
      arbitratorRDV = arbitratorRDV + rnorm(1, arbitrator_mu, sigmaArb)
    }
    
    if (debug){
      debug_row = data.frame(time = time, arbitrator_mu_mean = round(arbitrator_mu_mean, 3), arbitratorRDV = round(arbitratorRDV, 3), barrier = round(barrier[time], 3), lotteryRDV = round(lotteryRDV, 3), fractalRDV = round(fractalRDV, 3))
      debug_df = rbind(debug_df, debug_row)
    }
    
    # Increment sampling iteration
    time = time + 1
  }
  
  #If a choice hasn't been made by the time limit
  if(is.na(RT)){
    # Choose whatever you have most evidence for
    arbitrator = ifelse(arbitratorRDV >= 0 , "EV", "QV")
    if(arbitrator == "EV"){
      choice = ifelse(lotteryRDV > 0, "left", "right")
    } else if (arbitrator == "QV"){
      choice = ifelse(fractalRDV > 0, "left", "right")
    }
    
    timeOut = 1
    RT=rlnorm(1, mean = 1.25, sd = 0.1)
  }
  
  #Organize output 
  out = data.frame(EVLeft = EVLeft, EVRight = EVRight, QVLeft = QVLeft, QVRight = QVRight, probFractalDraw = probFractalDraw, choice=choice, reactionTime = RT, timeOut = timeOut, arbitrator = arbitrator, dArb=dArb, dLott=dLott, dFrac=dFrac, sigmaArb=sigmaArb, sigmaLott=sigmaLott, sigmaFrac=sigmaFrac, barrierDecay=barrierDecay, delta=delta, gamma=gamma, barrier=barrier[time], nonDecisionTime=nonDecisionTime, lotteryBias=lotteryBias, timeStep=timeStep, maxIter=maxIter, epsilon = epsilon)
  
  if(debug){
    out = list(out=out, debug_df=debug_df[-1,])
  }
  
  return(out)
}

fit_trial = function(dArb, dLott, dFrac, sigmaArb, sigmaLott, sigmaFrac, barrierDecay, delta, barrier=1, nonDecisionTime=0, lotteryBias=0.1, timeStep=10, epsilon = 0, approxStateStep = 0.1, debug=FALSE, ...){
  
  # RDV = bias
  
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
  EVLeft=kwargs$EVLeft
  EVRight=kwargs$EVRight
  QVLeft=kwargs$QVLeft
  QVRight=kwargs$QVRight
  probFractalDraw=kwargs$probFractalDraw
  # Stimulus screen comes on 2 secs after the presentation of probFractalDraw
  nonDecIters = nonDecisionTime / timeStep
  
  numTimeSteps = round(reactionTime / timeStep)
  
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
  prStatesArb = matrix(data = 0, nrow = length(states), ncol = numTimeSteps)
  prStatesArb[biasState,1] = 1
  
  prStatesLott = matrix(data = 0, nrow = length(states), ncol = numTimeSteps)
  prStatesLott[biasState,1] = 1
  
  prStatesFrac = matrix(data = 0, nrow = length(states), ncol = numTimeSteps)
  prStatesFrac[biasState,1] = 1
  
  # The probability of crossing each barrier over the time of the trial.
  probUpCrossing = rep(0, numTimeSteps)
  probDownCrossing = rep(0, numTimeSteps)
  
  # Rows of these matrices correspond to array elements in python
  
  # How much change is required from each state to move onto every other state
  changeMatrix = matrix(data = states, ncol=length(states), nrow=length(states), byrow=FALSE) - matrix(data = states, ncol=length(states), nrow=length(states), byrow=TRUE)
  
  # How much change is required from each state to cross the up or down barrier at each time point
  changeUp = matrix(data = barrier, ncol=numTimeSteps, nrow=length(states), byrow=TRUE) - matrix(data = states, ncol=numTimeSteps, nrow=length(states), byrow=FALSE)
  changeDown = matrix(data = -barrier, ncol=numTimeSteps, nrow=length(states), byrow=TRUE) - matrix(data = states, ncol=numTimeSteps, nrow=length(states), byrow=FALSE)
  
  elapsedNDT = 0
  
  lottery_mu_mean = dLott * (1-probFractalDraw) * (EVLeft - EVRight)
  
  distortedProbFractalDraw = exp((-1)*delta*((-1)*log(probFractalDraw))^gamma)
  fractal_mu_mean = dFrac * distortedProbFractalDraw * (QVLeft - QVRight)
  
  # LOOP of state probability updating up to reaction time
  
  # Start at 2 to match python indexing that starts at 0
  for(nextTime in 2:numTimeSteps){
    curTime = nextTime - 1 
    if (elapsedNDT < nonDecIters){
      arb_mu_mean = 0
      elapsedNDT = elapsedNDT + 1
    } else{
      arb_mu_mean = ...
    }
    
    mu = rnorm(1, arb_mu_mean, epsilon)
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
    # prStatesNew = (stateStep * (dnorm(changeMatrix, mu, sigma) %*% prStates[,curTime]) )
    prStatesNew = t(stateStep * (prStates[,curTime] %*% dnorm(changeMatrix, mu, sigma)) )
    prStatesNew[states >= barrier[nextTime] | states <= -barrier[nextTime]] = 0
    
    # Calculate the probabilities of crossing the up barrier and the
    # down barrier. This is given by the sum, over all states A, of the
    # probability of being in A at the previous timestep times the
    # probability of crossing the barrier if A is the previous state.
    # (aze): use pnorm (p of x or less) instead of dnorm (p of x) bc 
    # boundary would be crossed with not only just a change the size 
    # of x but at a change at least as big of x
    tempUpCross = (prStates[,curTime] %*% (1 - pnorm(changeUp[,nextTime], mu, sigma)))[1]
    tempDownCross = (prStates[,curTime] %*% (pnorm(changeDown[,nextTime], mu, sigma)))[1]
    
    # Renormalize to cope with numerical approximations.
    sumIn = sum(prStates[,curTime])
    sumCurrent = sum(prStatesNew) + tempUpCross + tempDownCross
    prStatesNew = prStatesNew * sumIn / sumCurrent
    tempUpCross = tempUpCross * sumIn / sumCurrent
    tempDownCross = tempDownCross * sumIn / sumCurrent
    
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
  
  out = data.frame(likelihood = likelihood, EVLeft = EVLeft, EVRight = EVRight, QVLeft = QVLeft, QVRight = QVRight, probFractalDraw = probFractalDraw, choice=choice, reactionTime = reactionTime, d = d, sigma = sigma, barrierDecay = barrierDecay, delta=delta, barrier=barrier[numTimeSteps], nonDecisionTime=nonDecisionTime, bias=bias, timeStep=timeStep, epsilon = epsilon)
  
  
  return(out)
  
}