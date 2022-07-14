get_qvals = function(subj_data, model_name='rpeBoth'){
  
  subj_data$leftQValue = 0
  subj_data$rightQValue = 0
  subj_data$leftFractalRpe = 0
  subj_data$rightFractalRpe = 0
  
  if(model_name == "rpeBoth"){
    
    for (i in 2:nrow(subj_data)){
      subj_data$leftFractalRpe[i] = subj_data$leftFractalReward[i-1] - subj_data$leftQValue[i-1]
      subj_data$rightFractalRpe[i] = subj_data$rightFractalReward[i-1] - subj_data$rightQValue[i-1]
      subj_data$leftQValue[i] = subj_data$leftQValue[i-1] + subj_data$alpha[i] * (subj_data$leftFractalRpe[i])
      subj_data$rightQValue[i] = subj_data$rightQValue[i-1] + subj_data$alpha[i] * (subj_data$rightFractalRpe[i])
    }
    
  } else if(model_name == "rpeChosenBundleFractal"){
    for (i in 2:nrow(subj_data)){
      
      if(subj_data$choiceLeft[i] == 1){
        subj_data$leftFractalRpe[i] = subj_data$leftFractalReward[i-1] - subj_data$leftQValue[i-1]
        subj_data$rightFractalRpe[i] = 0
      } else {
        subj_data$leftFractalRpe[i] = 0
        subj_data$rightFractalRpe[i] = subj_data$rightFractalReward[i-1] - subj_data$rightQValue[i-1]
      }
      
      subj_data$leftQValue[i] = subj_data$leftQValue[i-1] + subj_data$alpha[i] * (subj_data$leftFractalRpe[i])
      subj_data$rightQValue[i] = subj_data$rightQValue[i-1] + subj_data$alpha[i] * (subj_data$rightFractalRpe[i])
    }
  } else if(model_name == "rpeWhenFractalRewarded"){
    for (i in 2:nrow(subj_data)){
      
      if(subj_data$fractalDraw[i] == 1){
        subj_data$leftFractalRpe[i] = subj_data$leftFractalReward[i-1] - subj_data$leftQValue[i-1]
        subj_data$rightFractalRpe[i] = subj_data$rightFractalReward[i-1] - subj_data$rightQValue[i-1]
        
      } else {
        subj_data$rightFractalRpe[i] = 0
        subj_data$leftFractalRpe[i] = 0
      }
      
      subj_data$leftQValue[i] = subj_data$leftQValue[i-1] + subj_data$alpha[i] * (subj_data$leftFractalRpe[i])
      subj_data$rightQValue[i] = subj_data$rightQValue[i-1] + subj_data$alpha[i] * (subj_data$rightFractalRpe[i])
    }
    
  }else {
    print("model_name not specified.")
  }
  
  
  
  
  return(subj_data)
}
