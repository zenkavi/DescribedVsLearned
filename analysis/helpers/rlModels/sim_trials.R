sim_trials = function(numTrials = 60, 
                      numRuns = 5,
                      randomWalkSigma = .025,
                      randomWalkLowBound= 0.25,
                      randomWalkUpBound= 0.75){
  
  numTotalTrials = numTrials * numRuns
  
  df_names = c("session", "trialNum", "probFractalDraw", "rightLotteryValue", "rightLotteryProb", "leftLotteryValue", "leftLotteryProb", "fractalLeftProb", "fractalRightProb", "fractalDraw", "leftFractalReward", "rightFractalReward")
  trials = data.frame(matrix(NA, nrow=numTotalTrials, ncol=length(df_names)))
  names(trials) = df_names
  trials$rightLotteryValue = 1
  trials$rightLotteryProb = .5
  
  probBundles = c(rep(0, 5), rep(.1, 5), rep(.2, 5), 
                  rep(0.3, 6), rep(0.4, 6), rep(0.5, 6), rep(0.6, 6), rep(0.7, 6),
                  rep(0.8, 5), rep(0.9, 5), rep(1, 5))
  
  values = rep(c(0.5, 2, 2.5, 5, 0.1, 1, 2, 10, 0.3, 1, 2, 3, 0.7, 1, 2, 7, 0.9, 1, 2, 9), 3)
  probs = rep(c(1, 0.25, 0.2, 0.1, 1, 0.1, 0.05, 0.01, 1, 0.3, 0.15, 0.1, 1, 0.7, 0.35, 0.1, 1, 0.9, 0.45, 0.1), 3)
  
  # Initial fractal probabilities
  probLeft = randomWalkLowBound + (randomWalkUpBound - randomWalkLowBound) * runif(1)
  probRight = randomWalkLowBound + (randomWalkUpBound - randomWalkLowBound) * runif(1)
  
  for (i in 1:numRuns){
    
    cur_block = ((i-1)*numTrials+1) : (i*numTrials)
    
    trials$probFractalDraw[cur_block] = sample(probBundles)
    trials$leftLotteryValue[cur_block] = values
    trials$leftLotteryProb[cur_block] = probs
    
    for (j in 1:numTrials){
      
      cur_row = (i-1)*numTrials + j
      
      trials$session[cur_row] = i
      trials$trialNum[cur_row] = j
      
      # Update fractal left probability
      step = randomWalkSigma * rnorm(1)
      probLeft = probLeft + step
      if (probLeft > randomWalkUpBound){
        probLeft = probLeft - (2 * abs(step))
      } else if (probLeft < randomWalkLowBound){
        probLeft = probLeft + (2 * abs(step))
      }
    
      # Update fractal right probability
      step = randomWalkSigma * rnorm(1);
      probRight = probRight + step;
      if (probRight > randomWalkUpBound){
        probRight = probRight - (2 * abs(step))
      } else if (probRight < randomWalkLowBound){
        probRight = probRight + (2 * abs(step))
      }
      trials$fractalLeftProb[cur_row] = probLeft
      trials$fractalRightProb[cur_row] = probRight
      
      # Draw left fractal reward
      if (runif(1) <= trials$fractalLeftProb[cur_row]){
        trials$leftFractalReward[cur_row] = 1
      } else {
        trials$leftFractalReward[cur_row] = 0
      }
      
      # Draw right fractal reward  
      if (runif(1) <= trials$fractalRightProb[cur_row]){
        trials$rightFractalReward[cur_row] = 1
      } else {
        trials$rightFractalReward[cur_row] = 0
      }
      
      # Draw fractal or lottery
      if(runif(1) <= trials$probFractalDraw[cur_row]){
        trials$fractalDraw[cur_row] = 1
      } else{
        trials$fractalDraw[cur_row] = 0
        }
    }
  }

  return(trials)
}
