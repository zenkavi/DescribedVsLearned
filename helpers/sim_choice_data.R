sim_choice_data = function(trials, pars, logLik=F, data=NA){
  
  if(is.na(data)[1]){
    data = trials
  }
  
  a = pars$alpha
  b = pars$beta
  g = pars$gamma
  d = pars$delta
  
  # Initialize Q Values
  leftQV = 0
  rightQV = 0
  
  for (i in 1:nrow(data)){
    
    if(i < nrow(data)){
      leftQV[i+1] = leftQV[i] + a * (leftFractalReward[i] - leftQV[i])
      rightQV[i+1] = rightQV[i] + a * (rightFractalReward[i] - rightQV[i])
    }
    
  }
  
  leftEV = leftLotteryValue * leftLotteryProb
  rightEV = rightLotteryValue * rightLotteryProb

  wProbFrac = (d * (probFractalDraw^g) ) / ( (d * (probFractalDraw^g)) + (1-probFractalDraw)^g )

  optValLeft = (1-wProbFrac)*leftEV + wProbFrac*leftQV
  optValRight = (1-wProbFrac)*rightEV + wProbFrac*rightQV

  # Inverse logit of value difference weighted by beta
  x = b * (optValLeft - optValRight)
  choice_prob = exp(x)/(1+exp(x))
  
  if(logLik){
    logLik = dbinom(data$choiceLeft, 1, prob = choice_prob, log=T)
    logLik = sum(data$logLik)
    return(logLik)
  } else{
    data$choiceLeft = rbinom(nrow(data), 1, choice_prob)
    return(data)
  }
}