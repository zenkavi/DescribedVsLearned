sim_trial = function(dArb, dLott, dFrac, sigmaArb, sigmaLott, sigmaFrac, barrierDecay, barrier=1, nonDecisionTime=0, lotteryBias=0.1, timeStep=10, maxIter=400, debug=FALSE,...){
  
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
  
  if(debug){
    debug_df = data.frame()
  }
  
  kwargs = list(...)
  # EVLeft=kwargs$EVLeft
  # EVRight=kwargs$EVRight
  # 
  # QVLeft=kwargs$QVLeft
  # QVRight=kwargs$QVRight
  
  distortedEVDiff = kwargs$distortedEVDiff
  distortedQVDiff = kwargs$distortedQVDiff
  
  # pFrac == 1 bias
  probFractalDraw=kwargs$probFractalDraw
  if(probFractalDraw == 1){
    fractalRDV = distortedQVDiff
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
  out = data.frame(EVLeft = EVLeft, EVRight = EVRight, QVLeft = QVLeft, QVRight = QVRight, probFractalDraw = probFractalDraw, choice=choice, reactionTime = RT, timeOut = timeOut, arbitrator = arbitrator, dArb=dArb, dLott=dLott, dFrac=dFrac, sigmaArb=sigmaArb, sigmaLott=sigmaLott, sigmaFrac=sigmaFrac, barrierDecay=barrierDecay, barrier=barrier[time], nonDecisionTime=nonDecisionTime, lotteryBias=lotteryBias, timeStep=timeStep, maxIter=maxIter)
  
  if(debug){
    out = list(out=out, debug_df=debug_df[-1,])
  }
  
  return(out)
}

library(tidyverse)

getArbStateProbs = function(pLottStates,  pFracStates, pArbStates, states, changeMatrix){
  
  AbsLottStates = unique(abs(round(states, 1)))
  AbsFracStates = unique(abs(round(states, 1)))
  halfStateLen = floor(length(states)/2)
  pAbsLottStates = c( pLottStates[1:halfStateLen]+pLottStates[length(states):(halfStateLen+2)] , pLottStates[halfStateLen+1])
  pAbsFracStates = c( pFracStates[1:halfStateLen]+pFracStates[length(states):(halfStateLen+2)] , pFracStates[halfStateLen+1])
  
  absLottIntegrator = data.frame(AbsLottStates, pAbsLottStates)
  absFracIntegrator = data.frame(AbsFracStates, pAbsFracStates)
  
  tmp = expand.grid(AbsLottStates, AbsFracStates) %>%
    rename(AbsLottStates = Var1, AbsFracStates = Var2) %>%
    left_join(absLottIntegrator, by="AbsLottStates") %>%
    left_join(absFracIntegrator, by="AbsFracStates") %>%
    mutate(DiffStates = round(AbsLottStates-AbsFracStates,1),
           pDiffStates = pAbsLottStates * pAbsFracStates) %>%
    group_by(DiffStates) %>%
    summarise(sumPDiffStates = sum(pDiffStates))
  
  changeMatrixArb = ifelse(abs(changeMatrix)>1, 0, changeMatrix)
  pchangeMatrixArb = changeMatrixArb
  for(i in 1:nrow(changeMatrixArb)){
    for(j in 1:ncol(changeMatrixArb)){
      changeVal = round(changeMatrixArb[i,j], 1)
      if(changeVal %in% tmp$DiffStates){
        pchangeMatrixArb[i, j] = tmp$sumPDiffStates[which(tmp$DiffStates == changeVal)]
      }
    }
  }
  
  pArbStatesNew = pArbStates %*% pchangeMatrixArb
  
  return(pArbStatesNew)
}

fit_trial = function(dArb, dLott, dFrac, sigmaArb, sigmaLott, sigmaFrac, barrierDecay, barrier=1, nonDecisionTime=0, lotteryBias=0.1, timeStep=10, approxStateStep = 0.1, debug=FALSE, ...){
  
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
  distortedEVDiff = kwargs$distortedEVDiff
  distortedQVDiff = kwargs$distortedQVDiff
  probFractalDraw=kwargs$probFractalDraw
  
  bias = lotteryBias
  
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
  halfStateLen = floor(length(states)/2)
  
  # Find the state corresponding to the bias parameter.
  biasState = which.min(abs(states - bias))
  
  # Initial probability for all states is zero, except the bias state,
  # for which the initial probability is one.
  prStatesArb = matrix(data = 0, nrow = length(states), ncol = numTimeSteps)
  prStatesArb[biasState,1] = 1 #bias affects arbitrator not the attribute integrators
  
  prStatesLott = matrix(data = 0, nrow = length(states), ncol = numTimeSteps)
  prStatesLott[halfStateLen+1,1] = 1 
  
  prStatesFrac = matrix(data = 0, nrow = length(states), ncol = numTimeSteps)
  prStatesFrac[halfStateLen+1,1] = 1
  
  # The probability of crossing each barrier over the time of the trial.
  probUpCrossing = rep(0, numTimeSteps)
  probDownCrossing = rep(0, numTimeSteps)
  
  # Rows of these matrices correspond to array elements in python
  
  # How much change is required from each state to move onto every other state
  changeMatrix = matrix(data = states, ncol=length(states), nrow=length(states), byrow=FALSE) - matrix(data = states, ncol=length(states), nrow=length(states), byrow=TRUE)
  
  # How much change is required from each state to cross the up or down barrier at each time point
  changeUp = matrix(data = barrier, ncol=numTimeSteps, nrow=length(states), byrow=TRUE) - matrix(data = states, ncol=numTimeSteps, nrow=length(states), byrow=FALSE)
  changeDown = matrix(data = -barrier, ncol=numTimeSteps, nrow=length(states), byrow=TRUE) - matrix(data = states, ncol=numTimeSteps, nrow=length(states), byrow=FALSE)
  
  muLott = dLott * distortedEVDiff
  muFrac = dFrac * distortedQVDiff
  
  # LOOP of state probability updating up to reaction time
  
  # Start at 2 to match python indexing that starts at 0
  for(nextTime in 2:numTimeSteps){ #looping only on computation time steps. Non decision time iterations have been subtracted above. Might revisit if this makes sense later
    curTime = nextTime - 1 
    
    # Update the probability of the states that remain inside the
    # barriers.
    # prStatesNew = (stateStep * (dnorm(changeMatrix, mu, sigma) %*% prStates[,curTime]) )
    prStatesNewLott = t(stateStep * (prStatesLott[,curTime] %*% dnorm(changeMatrix, muLott, sigmaLott)) )
    prStatesNewLott[states >= barrier[nextTime] | states <= -barrier[nextTime]] = 0
    
    prStatesNewFrac = t(stateStep * (prStatesFrac[,curTime] %*% dnorm(changeMatrix, muFrac, sigmaFrac)) )
    prStatesNewFrac[states >= barrier[nextTime] | states <= -barrier[nextTime]] = 0
    
    prStatesNewArb = getArbStateProbs(prStatesNewLott, prStatesNewFrac, prStatesArb[,curTime], states, changeMatrix)
    
    # Calculate the probabilities of crossing the up barrier and the
    # down barrier. 
    # tempUpCross = (prStates[,curTime] %*% (1 - pnorm(changeUp[,nextTime], mu, sigma)))[1]
    # tempDownCross = (prStates[,curTime] %*% (pnorm(changeDown[,nextTime], mu, sigma)))[1]
    
    # Renormalize to cope with numerical approximations.
    # sumIn = sum(prStates[,curTime])
    # sumCurrent = sum(prStatesNew) + tempUpCross + tempDownCross
    # prStatesNew = prStatesNew * sumIn / sumCurrent
    # tempUpCross = tempUpCross * sumIn / sumCurrent
    # tempDownCross = tempDownCross * sumIn / sumCurrent
    
    # Avoid NAs for likelihood conditional statements
    # if (is.na(tempUpCross)){
    #   tempUpCross = 0
    # }
    # if (is.na(tempDownCross)){
    #   tempDownCross = 0
    # }
    
    # Update the probabilities of each state and the probabilities of
    # crossing each barrier at this timestep.
    prStatesLott[, nextTime] = prStatesNewLott
    prStatesFrac[, nextTime] = prStatesNewFrac
    prStatesArb[, nextTime] = prStatesNewArb
    # probUpCrossing[nextTime] = tempUpCross
    # probDownCrossing[nextTime] = tempDownCross
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
  
  out = data.frame(likelihood = likelihood, EVLeft = EVLeft, EVRight = EVRight, QVLeft = QVLeft, QVRight = QVRight, probFractalDraw = probFractalDraw, choice=choice, reactionTime = reactionTime, d = d, sigma = sigma, barrierDecay = barrierDecay, delta=delta, barrier=barrier[numTimeSteps], nonDecisionTime=nonDecisionTime, bias=bias, timeStep=timeStep)
  
  
  return(out)
  
}