library(tidyverse)

get_true_pars = function(data_, true_pars_path_, return_str_ = FALSE){
  
  ########## This part depends on the parameters in the model. Either needs a smarter solution or many conditionals to extend to models with other parameters
  ########## Also note that the model name is not in the data file name but is in the data file
  true_pars = read.csv(paste0(true_pars_path_,data_,'.csv'))
  true_pars = true_pars %>%
    select(d, sigma, delta, gamma) %>%
    distinct()
  
  true_pars_str = paste0("d = ", true_pars$d, ", sigma = ", true_pars$sigma, ", delta = ", true_pars$delta, ", gamma = ", true_pars$gamma)
  
  true_pars = true_pars %>%
    gather(key, value)
  ##########
  
  if(return_str_){
    return(true_pars_str)
  } else{
    return(list(true_pars = true_pars, true_pars_str = true_pars_str))
  }

}