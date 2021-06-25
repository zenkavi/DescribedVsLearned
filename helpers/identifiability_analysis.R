set.seed(2394239)
library(here)
library(tidyverse)
library(rstan)
helpers_path = here('helpers/')

source(paste0(helpers_path, 'sim_trials.R'))
source(paste0(helpers_path, 'sim_choice_data.R'))
source(paste0(helpers_path, 'extract_var_for_stan.R'))
source(paste0(helpers_path, 'organize_stan_output.R'))

identifiability_analysis = function(truePars,
                                    modelName,
                                    numSims = 10,
                                    numTrials = 60, 
                                    numRuns = 5,
                                    randomWalkSigma = .025,
                                    randomWalkLowBound= 0.25,
                                    randomWalkUpBound= 0.75,
                                    subj_par_names = c("alpha","gamma", "delta", "beta"), 
                                    group_par_names=NA){
  
  for(i in 1:numSims){
    trials = sim_trials(numTrials, 
                        numRuns,
                        randomWalkSigma,
                        randomWalkLowBound,
                        randomWalkUpBound)
    
    if(i == 1){
      data = sim_choice_data(trials, truePars)
      data = data %>% mutate(subnum=1)
    } else{
      cur_data = sim_choice_data(trials, truePars)
      cur_data = cur_data %>% mutate(subnum=i)
      data = rbind(data, cur_data)
    }
    
  }
  
  num_subjs = numSims
  
  num_trials = rep(numTrials * numRuns, numSims)
  
  #subjects in rows, trials in columns
  choices = extract_var_for_stan(data, choiceLeft)
  
  ev_left = extract_var_for_stan(data, leftEV)
  
  ev_right = extract_var_for_stan(data, rightEV)
  
  fractal_outcomes_left = extract_var_for_stan(data, leftFractalReward)
  
  fractal_outcomes_right = extract_var_for_stan(data, rightFractalReward)
  
  trial_pFrac = extract_var_for_stan(data, probFractalDraw)
  
  m_data=list(num_subjs = num_subjs,
              num_trials = num_trials,
              choices = choices,
              ev_left = ev_left,
              ev_right = ev_right,
              fractal_outcomes_left = fractal_outcomes_left,
              fractal_outcomes_right = fractal_outcomes_right,
              trial_pFrac = trial_pFrac)
  
  rm(num_subjs, num_trials, choices, ev_left, ev_right, fractal_outcomes_left, fractal_outcomes_right, trial_pFrac)
  
  if(file.exists(paste0(helpers_path, 'stanModels/ida_', modelName,'.RDS'))){
    fit = readRDS(paste0(helpers_path, 'stanModels/ida_', modeName, '.RDS'))
    rm(m_data)
  } else {
    m = stan_model(paste0(helpers_path, 'stanModels/', modelName, '.stan'))
    fit = sampling(m, data=m_data)
    saveRDS(fit, paste0(helpers_path, 'stanModels/ida_', modelName, '.RDS'))
    rm(m, m_data)}
  
  if(is.na(group_par_names)){
    out = organize_stan_output(fit, subnums = 1:numSims, subj_par_names=subj_par_names, group_par_names=group_par_names)
    par_ests = out$par_ests
    return(par_ests)
  } else{
    out = organize_stan_output(fit, subnums = 1:numSims, subj_par_names=subj_par_names, group_par_names=group_par_names)
    par_ests = out$par_ests
    g_par_ests = out$g_par_ests
    return(list(par_ests=par_ests, g_par_ests = g_par_ests))
  }
}

