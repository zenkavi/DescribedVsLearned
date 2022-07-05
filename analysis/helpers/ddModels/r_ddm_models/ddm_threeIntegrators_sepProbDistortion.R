sim_trial = function(dArb, dLott, dFrac, sigmaLott, sigmaFrac, barrierDecay, barrier=1, nonDecisionTime=0, bias=0.1, timeStep=10, maxIter=400, debug=FALSE,...){
  
  # d : drift rate
  # sigma: sd of the normal distribution 
  # timeStep: in ms
  # nonDecisionTime: in ms
  # maxIter: num max samples. if a barrier isn't hit by this sampling of evidence no decision is made. If time step is 10ms and maxIter is 1000 this would be a 10sec timeout maximum
  
  
  arbitratorRDV = bias
  fractalRDV = 0
  lotteryRDV = 0
  time = 1
  elapsedNDT = 0
  choice = 0
  RT = NA
  
  timeOut = 0
  
  if(debug){
    debug_df = data.frame()
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
  rawQVDiff = QVLeft - QVRight
  
  # pFrac == 1 bias
  if(probFractalDraw == 1){
    # fractalRDV = distortedQVDiff #when using distorted QV diff these choices were not as fast as true data
    fractalRDV = rawQVDiff
  }
  
  nonDecIters = nonDecisionTime / timeStep
  
  initialBarrier = barrier
  barrier = rep(initialBarrier, maxIter)
  
  # The values of the barriers can change over time
  for(t in seq(1, maxIter, 1)){
    barrier[t] = initialBarrier / (1 + barrierDecay * t)
  }
  
  lottery_mu = dLott * distortedEVDiff
  fractal_mu = dFrac * distortedQVDiff
  sigmaArb = sqrt(sigmaLott^2 + sigmaFrac^2)
  
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
      elapsedNDT = elapsedNDT + 1
    } else{
      
      # Three integrators
      
      # Lottery integrator
      lotteryRDV = lotteryRDV + rnorm(1, lottery_mu, sigmaLott)
      
      # Fractal integrator
      fractalRDV = fractalRDV + rnorm(1, fractal_mu, sigmaFrac)
      
      # Arbitrator 
      # If abs(lotteryRDV) > abs(fractalRDV) stronger relative preference for a side based on lotteries
      arbitrator_mu = dArb * (abs(lotteryRDV) - abs(fractalRDV))
      arbitratorRDV = arbitratorRDV + rnorm(1, arbitrator_mu, sigmaArb)
    }
    
    if (debug){
      debug_row = data.frame(time = time, arbitrator_mu = round(arbitrator_mu, 3), arbitratorRDV = round(arbitratorRDV, 3), barrier = round(barrier[time], 3), lotteryRDV = round(lotteryRDV, 3), fractalRDV = round(fractalRDV, 3))
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
  out = data.frame(EVLeft = EVLeft, EVRight = EVRight, QVLeft = QVLeft, QVRight = QVRight, distortedEVDiff = distortedEVDiff, distortedQVDiff = distortedQVDiff, probFractalDraw = probFractalDraw, choice=choice, reactionTime = RT, timeOut = timeOut, arbitrator = arbitrator, dArb=dArb, dLott=dLott, dFrac=dFrac, sigmaArb=sigmaArb, sigmaLott=sigmaLott, sigmaFrac=sigmaFrac, barrierDecay=barrierDecay, barrier=barrier[time], nonDecisionTime=nonDecisionTime, bias=bias, timeStep=timeStep, maxIter=maxIter)
  
  if(debug){
    out = list(out=out, debug_df=debug_df[-1,])
  }
  
  return(out)
}


fit_trial = function(dArb, dLott, dFrac, sigmaLott, sigmaFrac, barrierDecay, barrier=1, nonDecisionTime=0, bias=0.1, timeStep=10, approxStateStep = 0.1, debug=FALSE, epsilon=0, ...){
  
  ######################## HELPER FUNCTIONS ########################
  
  get_abs_dist_moments = function(mu, sigma){
    
    # Definitions of moments
    # https://en.wikipedia.org/wiki/Folded_normal_distribution
    
    mu_y = sigma * sqrt(2/pi) * exp((-mu^2)/(2*sigma^2)) + mu * pracma::erf((mu)/(sqrt(2*sigma^2)))
    
    sigma_y = sqrt(mu^2 + sigma^2 - mu_y^2)
    
    return(list(mu_y = mu_y, sigma_y = sigma_y))
  }
  
  
  get_abs_diff_dist_moments = function(mu1, sigma1, mu2, sigma2){
    
    tmp = get_abs_dist_moments(mu1, sigma1)
    abs_mu1 = tmp$mu_y
    abs_sigma1 = tmp$sigma_y
    
    tmp = get_abs_dist_moments(mu2, sigma2)
    abs_mu2 = tmp$mu_y
    abs_sigma2 = tmp$sigma_y
    
    diff_mu = abs_mu1 - abs_mu2
    diff_sigma = sqrt(abs_sigma1^2 + abs_sigma2^2)
    
    # out = data.frame(abs_mu1 = abs_mu1, abs_sigma1 = abs_sigma1, abs_mu2 = abs_mu2, abs_sigma2 = abs_sigma2, diff_mu = diff_mu, diff_sigma = diff_sigma)
    out = list(diff_mu = diff_mu, diff_sigma = diff_sigma)
    
    return(out)
  }
  
  ######################## HELPER FUNCTIONS ########################

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
  # p(bottom boundary) is the first value! Don't get confused by seeing it at the top 
  prStatesArb = matrix(data = 0, nrow = length(states), ncol = numTimeSteps)
  prStatesArb[biasState,1] = 1
  
  # The probability of crossing each barrier over the time of the trial.
  probUpCrossingArb = rep(0, numTimeSteps)
  probDownCrossingArb = rep(0, numTimeSteps)
  
  # Rows of these matrices correspond to array elements in python
  
  # How much change is required from each state to move onto every other state. From the smallest state (bottom boundary) to the largest state (top boundary)
  changeMatrix = matrix(data = states, ncol=length(states), nrow=length(states), byrow=FALSE) - matrix(data = states, ncol=length(states), nrow=length(states), byrow=TRUE)
  
  # How much change is required from each state to cross the up or down barrier at each time point
  changeUp = matrix(data = barrier, ncol=numTimeSteps, nrow=length(states), byrow=TRUE) - matrix(data = states, ncol=numTimeSteps, nrow=length(states), byrow=FALSE)
  changeDown = matrix(data = -barrier, ncol=numTimeSteps, nrow=length(states), byrow=TRUE) - matrix(data = states, ncol=numTimeSteps, nrow=length(states), byrow=FALSE)
  
  elapsedNDT = 0
  
  muLott = dLott * distortedEVDiff
  muFrac = dFrac * distortedQVDiff
  
  # tmp = get_abs_diff_dist_moments(muLott, sigmaLott, muFrac, sigmaFrac)
  # muArb = tmp$diff_mu
  # sigmaArb = tmp$diff_sigma
  
  
  # LOOP of state probability updating up to reaction time
  
  # Start at 2 to match python indexing that starts at 0
  for(nextTime in 2:numTimeSteps){
    curTime = nextTime - 1 
    
    tmp = get_abs_diff_dist_moments(muLott*curTime, sqrt(sigmaLott^2*curTime), muFrac*curTime, sqrt(sigmaFrac^2*curTime))
    muArb = tmp$diff_mu
    sigmaArb = tmp$diff_sigma
    
    if (elapsedNDT < nonDecIters){
      mu_mean = 0
      elapsedNDT = elapsedNDT + 1
    } else{
      mu_mean = muArb
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
    prStatesArbNew = (stateStep * (dnorm(changeMatrix, mu, sigmaArb) %*% prStatesArb[,curTime]) )
    prStatesArbNew[states >= barrier[nextTime] | states <= -barrier[nextTime]] = 0
    
    # Calculate the probabilities of crossing the up barrier and the
    # down barrier. This is given by the sum, over all states A, of the
    # probability of being in A at the previous timestep times the
    # probability of crossing the barrier if A is the previous state.
    tempUpCrossArb = (prStatesArb[,curTime] %*% (1 - pnorm(changeUp[,nextTime], mu, sigmaArb)))[1]
    tempDownCrossArb = (prStatesArb[,curTime] %*% (pnorm(changeDown[,nextTime], mu, sigmaArb)))[1]
    
    # Renormalize to cope with numerical approximations.
    sumIn = sum(prStatesArb[,curTime])
    sumCurrent = sum(prStatesArbNew) + tempUpCrossArb + tempDownCrossArb
    prStatesArbNew = prStatesArbNew * sumIn / sumCurrent
    tempUpCrossArb = tempUpCrossArb * sumIn / sumCurrent
    tempDownCrossArb = tempDownCrossArb * sumIn / sumCurrent
    
    # Avoid NAs for likelihood conditional statements
    if (is.na(tempUpCrossArb)){
      tempUpCrossArb = 0
    }
    if (is.na(tempDownCrossArb)){
      tempDownCrossArb = 0
    }
    
    # Update the probabilities of each state and the probabilities of
    # crossing each barrier at this timestep.
    prStatesArb[, nextTime] = prStatesArbNew
    probUpCrossingArb[nextTime] = tempUpCrossArb
    probDownCrossingArb[nextTime] = tempDownCrossArb
  }
  
  pLottRight = pnorm(0, mean = muLott*numTimeSteps, sd = sqrt(sigmaLott^2*numTimeSteps))
  pLottLeft = 1-pLottRight
  pFracRight = pnorm(0, mean = muLott*numTimeSteps, sd = sqrt(sigmaLott^2*numTimeSteps))
  pFracLeft = 1-pFracRight
  
  

  likelihood = 0
  if (choice == 1){ # Choice was left.
    
    # p of left is p of crossing the lottery boundary * p of lottery integrator being closer to the left boundary + p of crossing the fractal boundary * p of fractal integrator being closer to the left boundary
    likelihood = probUpCrossingArb[numTimeSteps] * pLottLeft + probDownCrossingArb[numTimeSteps] * pFracLeft
  
  } else if (choice == -1){

    likelihood = probUpCrossingArb[numTimeSteps] * pLottRight + probDownCrossingArb[numTimeSteps] * pFracRight
     
  }
  
  out = data.frame(likelihood = likelihood, distortedEVDiff = distortedEVDiff, distortedQVDiff = distortedQVDiff, probFractalDraw = probFractalDraw, choice=choice, reactionTime = reactionTime, dArb = dArb, dLott = dLott, dFrac = dFrac, sigmaArb = sigmaArb, sigmaLott = sigmaLott, sigmaFrac = sigmaFrac, barrierDecay = barrierDecay, barrier=barrier[numTimeSteps], nonDecisionTime=nonDecisionTime, bias=bias, timeStep=timeStep)
  
  if(debug){
    out = list(out = out, prStatesArb = data.frame(prStatesArb), prStatesLott = data.frame(prStatesLott), prStatesFrac = data.frame(prStatesFrac))
  }
  
  
  return(out)
  
}