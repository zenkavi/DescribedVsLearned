sim_trials = function(num_trials = 60, 
                      num_runs = 5){
  
  num_total_trials = num_trials * num_runs
  
  df_names = c("session", "trialNum", "probFractalDraw", "rightLotteryValue", "rightLotteryProb", "leftLotteryValue", "leftLotteryProb", "fractalLeftProb", "fractalRightProb", "fractalDraw", "choiceLeft", "reward", "leftFractalReward", "rightFractalReward")
  trials = data.frame(matrix(NA, nrow=num_total_trials, ncol=length(df_names)))
  names(trials) = df_names
  trials$rightLotteryValue = 1
  trials$rightLotteryProb = .5
  
  for (i in 1:num_runs){
    for (j in 1:num_trials){
      cur_row = (i-1)*num_trials + j
      trials$session[cur_row] = i
      trials$trialNum[cur_row] = j
    }
  }

  return(trials)
}
