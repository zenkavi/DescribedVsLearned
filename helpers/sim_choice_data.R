sim_choice_data = function(trials, pars){
  
  data = trials
  
  a = pars$alpha
  b = pars$beta
  g = pars$gamma
  d = pars$delta
  
  # Initialize Q Values
  data$leftQV = 0
  data$rightQV = 0
  
  for (i in 1:nrow(data)){
    
    if(i < nrow(data)){
      data$leftQV[i+1] = data$leftQV[i] + a * (data$leftFractalReward[i] - data$leftQV[i])
      data$rightQV[i+1] = data$rightQV[i] + a * (data$rightFractalReward[i] - data$rightQV[i])
    }
    
  }
  
  data$leftEV = data$leftLotteryValue * data$leftLotteryProb
  data$rightEV = data$rightLotteryValue * data$rightLotteryProb

  data$wProbFrac = (d * (data$probFractalDraw^g) ) / ( (d * (data$probFractalDraw^g)) + (1-data$probFractalDraw)^g )

  data$optValLeft = (1-data$wProbFrac)*data$leftEV + data$wProbFrac*data$leftQV
  data$optValRight = (1-data$wProbFrac)*data$rightEV + data$wProbFrac*data$rightQV

  # Inverse logit of value difference weighted by beta
  x = b * (data$optValLeft - data$optValRight)
  choice_prob = exp(x)/(1+exp(x))
  data$choiceLeft = rbinom(nrow(data), 1, choice_prob)
  data$logLik = dbinom(data$choiceLeft, 1, prob = choice_prob, log=T)

  logLik = sum(data$logLik)
  out = list(data = data, logLik = logLik)
  return(out)
}