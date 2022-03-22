sim_choice_data = function(trials, pars, logLik=F, data=NA, asymm_prob_distortion=FALSE){
  
  if(is.na(data)[1]){
    data = trials
  }
  
  a = pars$alpha
  d = pars$delta
  if("beta" %in% names(pars)){
    b = pars$beta
  }
  if("gamma" %in% names(pars)){
    g = pars$gamma
  } else {
    g = 1
  }
  
  
  
  # Initialize Q Values
  leftQV = 0
  rightQV = 0
  
  for (i in 1:nrow(data)){
    
    if(i < nrow(data)){
      leftQV[i+1] = leftQV[i] + a * (data$leftFractalReward[i] - leftQV[i])
      rightQV[i+1] = rightQV[i] + a * (data$rightFractalReward[i] - rightQV[i])
    }
    
  }
  
  leftEV = data$leftLotteryValue * data$leftLotteryProb
  rightEV = data$rightLotteryValue * data$rightLotteryProb
  data$leftEV = leftEV
  data$rightEV = rightEV
  
  wProbFrac = (d * (data$probFractalDraw^g) ) / ( (d * (data$probFractalDraw^g)) + (1-data$probFractalDraw)^g )

  if(asymm_prob_distortion){
    optValLeft = (1-probFractalDraw)*leftEV + wProbFrac*leftQV
    optValRight = (1-probFractalDraw)*rightEV + wProbFrac*rightQV 
  } else {
    optValLeft = (1-wProbFrac)*leftEV + wProbFrac*leftQV
    optValRight = (1-wProbFrac)*rightEV + wProbFrac*rightQV
  }

  # Inverse logit of value difference weighted by beta
  x = b * (optValLeft - optValRight)
  choice_prob = exp(x)/(1+exp(x))
  
  if(logLik){
    logLik = dbinom(data$choiceLeft, 1, prob = choice_prob, log=T)
    logLik = sum(logLik)
    return(logLik)
  } else{
    data$choiceLeft = rbinom(nrow(data), 1, choice_prob)
    return(data)
  }
}