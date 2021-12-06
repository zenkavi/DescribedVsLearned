sim_trial = function(dArb, dLott, dFrac, sigmaArb, sigmaLott, sigmaFrac, barrierDecay, delta, gamma, barrier=1, nonDecisionTime=0, lotteryBias=0.01, timeStep=10, maxIter=400, epsilon = 0.002, stimDelay = 2000, debug=FALSE,...){
  
  # d : drift rate
  # sigma: sd of the normal distribution 
  # timeStep: in ms
  # nonDecisionTime: in ms
  # maxIter: num max samples. if a barrier isn't hit by this sampling of evidence no decision is made. If time step is 10ms and maxIter is 1000 this would be a 10sec timeout maximum
  
  
  arbitratorRDV = lotteryBias
  # arbitratorRDV = 0
  fractalRDV = 0
  lotteryRDV = 0
  time = 1
  elapsedNDT = 0
  choice = 0
  RT = NA
  
  decPreStim = 0
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
  
  # remove bias from arbitrator RDV if pFrac == 1
  probFractalDraw=kwargs$probFractalDraw
  if(probFractalDraw == 1){
    arbitratorRDV = 0
  }
  distortedProbFractalDraw = exp((-1)*delta*((-1)*log(probFractalDraw))^gamma)
  
  nonDecIters = nonDecisionTime / timeStep
  stimDelayIters = stimDelay / timeStep
  maxIter = maxIter + stimDelayIters
  
  initialBarrier = barrier
  barrier = rep(initialBarrier, maxIter)
  
  # The values of the barriers can change over time
  for(t in seq(1, maxIter, 1)){
    barrier[t] = initialBarrier / (1 + barrierDecay * t)
  }
  
  lottery_mu_mean = dLott * (1-probFractalDraw) * (EVLeft - EVRight)
  fractal_mu_mean = dFrac * distortedProbFractalDraw * (QVLeft - QVRight)
  
  while (time<maxIter){
    
    # If the arbitrator RDV hits one of the barriers make decision
    if (arbitratorRDV >= barrier[time] | arbitratorRDV <= -barrier[time]){
      
      # Convert ms back to secs
      RT = (time * timeStep)/1000 
      
      #subtract stimDelay
      RT = RT - (stimDelay/1000) 
      
      # If decision is reached before the stim screen sample rt from log normal distribution similar to choice RT 
      if (RT < 0){
        decPreStim = 1
        RT=rlnorm(1, mean = -.25, sd = 0.5)
      }
      
      if (arbitratorRDV >= barrier[time]){
        arbitrator = "EV"
        choice = ifelse(lotteryRDV > 0, "left", "right")
      } else if (arbitratorRDV <= -barrier[time]){
        arbitrator = "QV"
        choice = ifelse(fractalRDV > 0, "left", "right")
      }
      break
    } 
    
    # Start early integration when fractals are the more relevant attribute
    if(time < stimDelayIters){
      
      if(probFractalDraw == 1){
        fractal_mu = rnorm(1, fractal_mu_mean, epsilon)
        fractalRDV = fractalRDV + rnorm(1, fractal_mu, sigmaFrac)
        
        arbitrator_mu_mean = dArb * (-1) * ((probFractalDraw) * abs(fractalRDV))
      } else {
        arbitrator_mu_mean = 0
      }
      arbitrator_mu = rnorm(1, arbitrator_mu_mean, epsilon)
      arbitratorRDV = arbitratorRDV + rnorm(1, arbitrator_mu, sigmaArb)
      
      # Integration after stim presentation  
    } else{
      if (elapsedNDT < nonDecIters){
        mu_mean = 0
        elapsedNDT = elapsedNDT + 1
      } else{
        
        # Switch to integrating only about lottery for a while when the stim comes on
        
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
  out = data.frame(EVLeft = EVLeft, EVRight = EVRight, QVLeft = QVLeft, QVRight = QVRight, probFractalDraw = probFractalDraw, choice=choice, reactionTime = RT, timeOut = timeOut, arbitrator = arbitrator, decPreStim = decPreStim)
  
  if(debug){
    out = list(out=out, debug_df=debug_df)
  }
  
  return(out)
}