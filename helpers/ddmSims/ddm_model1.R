sim_trial = function(d, sigma,  barrier=1, nonDecisionTime=0, bias=0, timeStep=10, maxIter=400, epsilon = 0.0002, ...){
  
  # d : drift rate
  # sigma: sd of the normal distribution 
  # timeStep: in ms
  # nonDecisionTime: in ms
  # maxIter: num max samples. if a barrier isn't hit by this sampling of evidence no decision is made. If time step is 10ms and maxIter is 1000 this would be a (1000 iterations * 10 ms (per iteration) / 1000 convert back to sec =) 10sec timeout maximum
  
  RDV = bias
  time = 1
  elapsedNDT = 0
  choice = 0
  RT = NA
  
  # Initialize inputs
  kwargs = list(...)
  
  EVLeft=kwargs$EVLeft
  EVRight=kwargs$EVRight
  QVLeft=kwargs$QVLeft
  QVRight=kwargs$QVRight
  probFractalDraw=kwargs$probFractalDraw
  
  valueLeft = probFractalDraw*QVLeft + (1-probFractalDraw)*(EVLeft)
  valueRight = probFractalDraw*QVRight + (1-probFractalDraw)*(EVRight)
  valDiff_mu_mean = d*(valueLeft - valueRight)
  
  while (time<maxIter){
    
    # If the RDV hit one of the barriers, the trial is over.
    if (RDV >= barrier | RDV <= -barrier){
      RT = (time * timeStep)/1000 #convert back to secs
      if (RDV >= barrier){
        choice = "left"
      } else if (RDV <= -barrier){
        choice = "right"
      }
      break
    } 
    
    # nonDecisionTime/timeStep gives how many sampling iterations the nonDecisionTime corresponds to
    nonDecIters = nonDecisionTime / timeStep
    # If elapsed time is less than the number of sampling iterations required for the nonDecisionTime set evidence sampling distribution mean to 0 so any move from starting point would only be noise
    if (elapsedNDT < nonDecIters){
      mu_mean = 0
      elapsedNDT = elapsedNDT + 1
    } else{
      # If the nonDecisionTime has passed set the mean of the distribution the decision variable will be sampled from to a value proportional to the value difference and the drift rate 
      mu_mean = valDiff_mu_mean
    }
    
    # Sample the change in RDV from the distribution with mean proportional to the value difference
    mu = rnorm(1, mu_mean, epsilon)
    RDV = RDV + rnorm(1, mu, sigma)
    
    # Increment sampling iteration
    time = time + 1
  }
  
  #Organize output 
  out = data.frame(EVLeft = EVLeft, EVRight = EVRight, QVLeft = QVLeft, QVRight = QVRight, probFractalDraw = probFractalDraw, choice=choice, reactionTime = RT, d = d, sigma = sigma,  barrier=barrier, nonDecisionTime=nonDecisionTime, bias=bias, timeStep=timeStep, maxIter=maxIter, epsilon = epsilon)
  return(out)
}

fit_trial = function(d, sigma, barrier=1, nonDecisionTime=0, bias=0, timeStep=10, epsilon = 0.0002, approxStateStep = 0.1, debug=FALSE, ...){
  
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
  nonDecIters = nonDecisionTime / timeStep
  
  numTimeSteps = round(reactionTime / timeStep)
  
  initialBarrier = barrier
  barrier = rep(initialBarrier, numTimeSteps)
  
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
  
  # How much change is required from each state to move onto every other state
  changeMatrix = matrix(data = states, ncol=length(states), nrow=length(states), byrow=FALSE) - matrix(data = states, ncol=length(states), nrow=length(states), byrow=TRUE)
  
  # How much change is required from each state to cross the up or down barrier at each time point
  changeUp = matrix(data = barrier, ncol=numTimeSteps, nrow=length(states), byrow=TRUE) - matrix(data = states, ncol=numTimeSteps, nrow=length(states), byrow=FALSE)
  changeDown = matrix(data = -barrier, ncol=numTimeSteps, nrow=length(states), byrow=TRUE) - matrix(data = states, ncol=numTimeSteps, nrow=length(states), byrow=FALSE)
  
  elapsedNDT = 0
  
  valueLeft = probFractalDraw*QVLeft + (1-probFractalDraw)*(EVLeft)
  valueRight = probFractalDraw*QVRight + (1-probFractalDraw)*(EVRight)
  valDiff_mu_mean = d*(valueLeft - valueRight)
  
  # LOOP of state probability updating up to reaction time
  
  # Start at 2 to match python indexing that starts at 0
  for(nextTime in 2:numTimeSteps){
    curTime = nextTime - 1 
   
    if (elapsedNDT < nonDecIters){
      mu_mean = 0
      elapsedNDT = elapsedNDT + 1
    } else{
      mu_mean = valDiff_mu_mean
    }
    
    
    mu = rnorm(1, mu_mean, epsilon)
    print(mu)
    
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
  
  out = data.frame(likelihood = likelihood, EVLeft = EVLeft, EVRight = EVRight, QVLeft = QVLeft, QVRight = QVRight, probFractalDraw = probFractalDraw, choice=choice, reactionTime = reactionTime, d = d, sigma = sigma, nonDecisionTime=nonDecisionTime, bias=bias, timeStep=timeStep, epsilon = epsilon)
  
  return(out)
  
}