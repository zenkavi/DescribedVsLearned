library(here)

helpers_path = here('analysis/helpers/rlModels')

source(paste0(helpers_path, 'sim_choice_data.R'))

sim_logLiks = function(numSims, testPars, truePars, 
                       numTrials, numRuns, randomWalkSigma, randomWalkLowBound, randomWalkUpBound){
  
  logLiks = rep(NA, numSims)
  
  for(i in 1: numSims){
    trials = sim_trials(numTrials,
                        numRuns,
                        randomWalkSigma,
                        randomWalkLowBound,
                        randomWalkUpBound)
    
    data = sim_choice_data(trials, truePars)
    logLiks[i] = sim_choice_data(trials=NA, pars=testPars, logLik=T, data)
  }
  
  return(logLiks)
}