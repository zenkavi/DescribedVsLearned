sim_data = function(trials, pars){
  
  data = trials
  
  a = pars$alpha
  b = pars$beta
  g = pars$gamma
  d = pars$delta
  
  # Initialize Q Values
  data$leftQV = 0
  data$rightQV = 0
  
  # Most of below could be faster if vectorized but looping over it here for clarity
  for (i in 1:nrow(data)){
    data$leftEV[i] = data$leftLotteryValue[i] * data$leftLotteryProb[i]
    data$rightEV[i] = data$rightLotteryValue[i] * data$rightLotteryProb[i]
    
    if(i < nrow(data)){
      data$leftQV[i+1] = data$leftQV[i] + a * (data$leftFractalReward[i] - data$leftQV[i])
      data$rightQV[i+1] = data$rightQV[i] + a * (data$rightFractalReward[i] - data$rightQV[i])
    }
    
    data$wProbFrac[i] = (d * (data$probFractalDraw[i]^g) ) / ( (d * (data$probFractalDraw[i]^g)) + (1-data$probFractalDraw[i])^g )
    
    data$optValLeft[i] = (1-data$wProbFrac[i])*data$leftEV[i] + data$wProbFrac[i]*data$leftQV[i]
    data$optValRight[i] = (1-data$wProbFrac[i])*data$rightEV[i] + data$wProbFrac[i]*data$rightQV[i]
    
    # Inverse logit of value difference weighted by beta
    x = b * (data$optValLeft[i] - data$optValRight[i])
    choice_prob = exp(x)/(1+exp(x))
    data$choiceLeft[i] = rbinom(1, 1, choice_prob)
    data$logLik[i] = dbinom(data$choiceLeft[i], 1, prob = choice_prob, log=T)
  }
  
  logLik = sum(data$logLik)
  out = list(data = data, logLik = logLik)
  return(out)
}