library(here)

helpers_path = here('helpers/')

source(paste0(helpers_path, 'sim_trials.R'))
source(paste0(helpers_path, 'sim_choice_data.R'))

sim_logLiks = function(numSims, pars, numTrials = 60,
                       numRuns = 5,
                       randomWalkSigma = .025,
                       randomWalkLowBound= 0.25,
                       randomWalkUpBound= 0.75){
  
  logLiks = rep(NA, numSims)
  
  for(i in 1: numSims){
    trials = sim_trials(numTrials,
                        numRuns,
                        randomWalkSigma,
                        randomWalkLowBound,
                        randomWalkUpBound)
    data = sim_choice_data(trials, pars)
    logLiks[i] = data$logLik
  }
  
  return(logLiks)
}