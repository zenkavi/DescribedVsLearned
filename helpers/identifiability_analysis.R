set.seed(2394239)
library(here)
library(tidyverse)
helpers_path = here('helpers/')

source(paste0(helpers_path, 'sim_logLiks.R'))

identifiability_analysis = function(true_pars,
                                    numTrials = 60, 
                                    numRuns = 5,
                                    randomWalkSigma = .025,
                                    randomWalkLowBound= 0.25,
                                    randomWalkUpBound= 0.75,
                                    alphas = seq(0.1, 1, .1),
                                    betas = seq(0.1, 5, .5),
                                    deltas = seq(0.1, 1.6, .2),
                                    gammas = seq(0.1, 1.6, .2),
                                    numSims = 10){
  trials = sim_trials(numTrials,
                      numRuns,
                      randomWalkSigma,
                      randomWalkLowBound,
                      randomWalkUpBound)
  data = sim_choice_data(trials, true_pars)
  
  sim_out = data.frame(alpha=NA, beta=NA, delta=NA, gamma=NA, logLik=NA)
  x = length(betas)*length(deltas)*length(gammas)
  y = length(alphas)*length(betas)*length(deltas)*length(gammas)
  
  for(i in 1:length(alphas)){
    done_pct = round(((i-1)*x)/(y),2)
    print(paste0(done_pct, "% done!") )
    for(j in 1:length(betas)){
      for(k in 1:length(deltas)){
        for(l in 1:length(gammas)){
          cur_pars = data.frame(alpha=alphas[i], beta=betas[j], delta=deltas[k], gamma=gammas[l])
          logLiks = sim_logLiks(numSims, cur_pars, true_data)
          cur_out = data.frame(alpha=alphas[i], beta = betas[j], delta=deltas[k], gamma=gammas[l], logLik = logLiks)
          sim_out = rbind(sim_out, cur_out)
        }
      }
    }
  }
  
  sim_out = sim_out %>%
    drop_na() %>%
    mutate(true_alpha = true_pars$alpha,
           true_beta = true_pars$beta,
           true_gamma = true_pars$gamma,
           true_delta = true_pars$delta)
  
  fn = paste0('identifiability_analysis_a', true_pars$alpha, '_b', true_pars$beta, '_g', true_pars$gamma,'_d',true_pars$delta,'.RDS')
  saveRDS(sim_out, paste0(helpers_path, fn))
  
  return(out)
}

