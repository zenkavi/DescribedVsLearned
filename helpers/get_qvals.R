get_qvals = function(subj_data){
  subj_data$leftQValue = 0
  subj_data$rightQValue = 0
  subj_data$leftFractalRpe = 0
  subj_data$rightFractalRpe = 0
  for (i in 2:nrow(subj_data)){
    subj_data$leftFractalRpe[i] = subj_data$leftFractalReward[i-1] - subj_data$leftQValue[i-1]
    subj_data$rightFractalRpe[i] = subj_data$rightFractalReward[i-1] - subj_data$rightQValue[i-1]
    subj_data$leftQValue[i] = subj_data$leftQValue[i-1] + subj_data$alpha[i] * (subj_data$leftFractalRpe[i])
    subj_data$rightQValue[i] = subj_data$rightQValue[i-1] + subj_data$alpha[i] * (subj_data$rightFractalRpe[i])
  }
  return(subj_data)
}
