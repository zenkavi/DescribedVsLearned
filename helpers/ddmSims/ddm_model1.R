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
  out = data.frame(EVLeft = EVLeft, EVRight = EVRight, QVLeft = QVLeft, QVRight = QVRight, probFractalDraw = probFractalDraw, choice=choice, reactionTime = RT)
  return(out)
}