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
  out = data.frame(EVLeft = EVLeft, EVRight = EVRight,QVLeft = QVLeft, QVRight = QVRight, distortedEVDiff = distortedEVDiff, distortedQVDiff = distortedQVDiff, probFractalDraw = probFractalDraw, choice=choice, reactionTime = RT, timeOut = timeOut, arbitrator = arbitrator, dArb=dArb, dLott=dLott, dFrac=dFrac, sigmaArb=sigmaArb, sigmaLott=sigmaLott, sigmaFrac=sigmaFrac, barrierDecay=barrierDecay, barrier=barrier[time], nonDecisionTime=nonDecisionTime, bias=bias, timeStep=timeStep, maxIter=maxIter)
  
  if(debug){
    out = list(out=out, debug_df=debug_df[-1,])
  }
  
  return(out)
}


fit_trial = function(dArb, dLott, dFrac, sigmaLott, sigmaFrac, barrierDecay, barrier=1, nonDecisionTime=0, bias=0.1, timeStep=10, approxStateStep = 0.1, debug=FALSE, ...){
  
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
  prStatesArb = matrix(data = 0, nrow = length(states), ncol = numTimeSteps)
  prStatesArb[biasState,1] = 1 #bias affects arbitrator not the attribute integrators
  
  prStatesLott = matrix(data = 0, nrow = length(states), ncol = numTimeSteps)
  prStatesLott[halfNumStateBins+1,1] = 1 
  
  prStatesFrac = matrix(data = 0, nrow = length(states), ncol = numTimeSteps)
  if(probFractalDraw == 1){
    rawQVDiff = kwargs$QVLeft - kwargs$QVRight
    fracBiasState = which.min(abs(states - rawQVDiff))
    prStatesFrac[fracBiasState, 1] = 1
  } else {
    prStatesFrac[halfNumStateBins+1,1] = 1
  }
  
  # How much change is required from each state to move onto every other state
  changeMatrix = matrix(data = states, ncol=length(states), nrow=length(states), byrow=FALSE) - matrix(data = states, ncol=length(states), nrow=length(states), byrow=TRUE)
  
  muLott = dLott * distortedEVDiff
  muFrac = dFrac * distortedQVDiff
  
  tmp = get_abs_diff_dist_moments(muLott, sigmaLott, muFrac, sigmaFrac)
  muArb = tmp$diff_mu
  sigmaArb = tmp$diff_sigma
  
  # LOOP of state probability updating up to reaction time
  # looping only on computation time steps. Non decision time iterations have been subtracted above. Might revisit if this makes sense later
  for(nextTime in 2:numTimeSteps){ 
    curTime = nextTime - 1 
    
    # Update the probability of the states that remain inside the barriers.
    # prStatesNewLott = (stateStep * (dnorm(changeMatrix, muLott, sigmaLott) %*% prStatesLott[,curTime]) )
    prStatesNewLott = (dnorm(changeMatrix, muLott, sigmaLott) %*% prStatesLott[,curTime])
    prStatesNewLott[states >= barrier[nextTime] | states <= -barrier[nextTime]] = 0
    prStatesNewLott = prStatesNewLott/sum(prStatesNewLott)

    # prStatesNewFrac = (stateStep * (dnorm(changeMatrix, muFrac, sigmaFrac) %*% prStatesFrac[,curTime]) )
    prStatesNewFrac = (dnorm(changeMatrix, muFrac, sigmaFrac) %*% prStatesFrac[,curTime])
    prStatesNewFrac[states >= barrier[nextTime] | states <= -barrier[nextTime]] = 0
    prStatesNewFrac = prStatesNewFrac/sum(prStatesNewFrac)

    # prStatesNewArb = (stateStep * (dnorm(changeMatrix, muArb, sigmaArb) %*% prStatesArb[,curTime]) )
    prStatesNewArb = (dnorm(changeMatrix, muArb, sigmaArb) %*% prStatesArb[,curTime]) 
    prStatesNewArb[states >= barrier[nextTime] | states <= -barrier[nextTime]] = 0
    prStatesNewArb = prStatesNewArb/sum(prStatesNewArb)

    # Update the probabilities of each state 
    prStatesLott[, nextTime] = prStatesNewLott
    prStatesFrac[, nextTime] = prStatesNewFrac
    prStatesArb[, nextTime] = prStatesNewArb

  }
  
  penultimateStep = numTimeSteps-1
  
  # How much change is required from each state to cross the up or down barrier in the final time step
  changeUp = matrix(data = barrier[numTimeSteps], ncol=1, nrow=length(states), byrow=TRUE) - matrix(data = states, ncol=1, nrow=length(states), byrow=FALSE)
  changeDown = matrix(data = -barrier[numTimeSteps], ncol=1, nrow=length(states), byrow=TRUE) - matrix(data = states, ncol=1, nrow=length(states), byrow=FALSE)
  
  # What is the p of observing that at least the size change sufficient to cross the bound given the moments of the distribution from which the change will come
  pChangeUpArb = (1-pnorm(changeUp, muArb, sigmaArb))
  pChangeDownArb = (pnorm(changeDown, muArb, sigmaArb))
  
  # p of crossing the boundary is the weighted sum of the prob of observing a given size change from every state bin and and the p of being in state bin
  probUpCrossingArb = (prStatesArb[,penultimateStep] %*% pChangeUpArb)[1]
  probDownCrossingArb = (prStatesArb[,penultimateStep] %*% pChangeDownArb)[1]

  pLottLeft = sum(prStatesLott[(halfNumStateBins+1):nrow(prStatesLott),numTimeSteps]) #first numbers in the vector are the bottom half of the state space
  pLottRight = sum(prStatesLott[1:(halfNumStateBins-1),numTimeSteps])
  pFracLeft = sum(prStatesFrac[(halfNumStateBins+1):nrow(prStatesFrac),numTimeSteps])
  pFracRight = sum(prStatesFrac[1:(halfNumStateBins-1),numTimeSteps])
  
  likelihood = 0
  if (choice == 1){ # Choice was left.
    
    # p of left is p of crossing the lottery boundary * p of lottery integrator being closer to the left boundary + p of crossing the fractal boundary * p of fractal integrator being closer to the left boundary
      likelihood = probUpCrossingArb * pLottLeft + probDownCrossingArb * pFracLeft
  
  } else if (choice == -1){

    likelihood = probUpCrossingArb * pLottRight + probDownCrossingArb * pFracRight
     
  }
  
  out = data.frame(likelihood = likelihood, distortedEVDiff = distortedEVDiff, distortedQVDiff = distortedQVDiff, probFractalDraw = probFractalDraw, choice=choice, reactionTime = reactionTime, dArb = dArb, dLott = dLott, dFrac = dFrac, sigmaArb = sigmaArb, sigmaLott = sigmaLott, sigmaFrac = sigmaFrac, barrierDecay = barrierDecay, barrier=barrier[numTimeSteps], nonDecisionTime=nonDecisionTime, bias=bias, timeStep=timeStep)
  
  if(debug){
    out = list(out = out, prStatesArb = data.frame(prStatesArb), prStatesLott = data.frame(prStatesLott), prStatesFrac = data.frame(prStatesFrac))
  }
  
  
  return(out)
  
}