library(here)

helpers_path = here('helpers/')

source(paste0(helpers_path, 'sim_choice_data.R'))

sim_logLiks = function(numSims, pars, data){
  
  logLiks = rep(NA, numSims)
  
  for(i in 1: numSims){
    logLiks[i] = sim_choice_data(trials, pars, logLik=T, data)
  }
  
  return(logLiks)
}