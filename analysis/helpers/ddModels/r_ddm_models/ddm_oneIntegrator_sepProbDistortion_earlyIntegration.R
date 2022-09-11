sim_trial = function(d, sigma, barrierDecay=0, barrier=1, nonDecisionTime=0, bias=0, timeStep=10, maxIter=400, epsilon = 0, debug=FALSE, stimDelay = 200,...){
  
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
  
  
  # Integration before stimulus presentation if fractal is more relevant
  if (probFractalDraw > .5){
    stimDelayIters = round(round(stimDelay/timeStep)*sqrt(probFractalDraw)/2) #duration of iteration depends on probFractalDraw
    
    elapsedEarlyInt = 0
    # earlyIntMu = d*(QVLeft-QVRight)
    earlyIntMu = d*distortedQVDiff
    
    while(elapsedEarlyInt<stimDelayIters){
      if (probFractalDraw>.5){
        RDV = RDV + rnorm(1, earlyIntMu, sigma)
      }
      elapsedEarlyInt = elapsedEarlyInt + 1
      
      if(debug){
        debug_row = data.frame(time = -1*(stimDelayIters-elapsedEarlyInt), mu_mean = earlyIntMu, mu = earlyIntMu, RDV = round(RDV, 3), barrier = round(barrier[time], 3))
        debug_df = rbind(debug_df, debug_row)
      }
      
    }
  }
  
  # Integration after stimulus presentation
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
