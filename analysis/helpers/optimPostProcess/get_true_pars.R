library(tidyverse)

get_true_pars = function(data_, true_pars_path_, return_str_ = FALSE, ex_cols_ = c("EVLeft", "EVRight", "QVLeft", "QVRight", "leftFractalReward", "rightFractalReward","probFractalDraw", "choice", "reactionTime", "timeOut", "barrierDecay", "barrier", "nonDecisionTime", "bias", "timeStep", "maxIter", "epsilon", "model")){
  
  ########## This part depends on the parameters in the model. Either needs a smarter solution or many conditionals to extend to models with other parameters
  ########## Also note that the model name is not in the data file name but is in the data file
  true_pars = read.csv(paste0(true_pars_path_,data_,'.csv'))
  true_pars = true_pars[,!names(true_pars) %in% ex_cols_]
  
  true_pars = true_pars %>%
    distinct()
  
  par_names = names(true_pars)

  true_pars_str = ""
  for(i in 1:length(par_names)){
    cur_par = par_names[i]
    true_pars_str = paste0(true_pars_str, cur_par,  " = ", true_pars[i][1], " ")
  }
  
  true_pars = true_pars %>%
    gather(key, value)
  ##########
  
  if(return_str_){
    return(true_pars_str)
  } else{
    return(list(true_pars = true_pars, true_pars_str = true_pars_str))
  }

}