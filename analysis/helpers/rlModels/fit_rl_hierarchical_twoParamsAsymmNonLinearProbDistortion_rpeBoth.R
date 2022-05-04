library(tidyr)
library(rstan)
library(here)

helpers_path = here('analysis/helpers/')

## Read in data if not already there
if (!exists('clean_beh_data')){
  source(paste0(helpers_path,'01_clean_behavioral_data.R'))
}

if(!exists('extract_var_for_stan')){
  source(paste0(helpers_path, 'rlModels/extract_var_for_stan.R'))
}

if(!exists('organize_stan_output')){
  source(paste0(helpers_path, 'rlModels/organize_stan_output.R'))
}

## If there is a fit object read it in
if(file.exists(paste0(helpers_path, 'rlModels/stanModels/fit_rl_hierarchical_twoParamsAsymmNonLinearProbDistortion_rpeBoth.RDS'))){
  fit = readRDS(paste0(helpers_path, 'rlModels/stanModels/fit_rl_hierarchical_twoParamsAsymmNonLinearProbDistortion_rpeBoth.RDS'))
} else {## Otherwise fit the model
  
  ## Reshape data
  num_subjs = length(unique(clean_beh_data$subnum))
  
  num_trials = clean_beh_data %>%
    count(subnum) %>%
    select(n)
  num_trials = num_trials$n
  
  #subjects in rows, trials in columns
  choices = extract_var_for_stan(clean_beh_data, choiceLeft)
  
  clean_beh_data = clean_beh_data %>%
    mutate(leftLotteryEV = lotteryValue*lotteryProb,
           rightLotteryEV = referenceValue*referenceProb)
  
  ev_left = extract_var_for_stan(clean_beh_data, leftLotteryEV)
  
  ev_right = extract_var_for_stan(clean_beh_data, rightLotteryEV)
  
  fractal_outcomes_left = extract_var_for_stan(clean_beh_data, leftFractalReward)
  
  fractal_outcomes_right = extract_var_for_stan(clean_beh_data, rightFractalReward)
  
  trial_pFrac = extract_var_for_stan(clean_beh_data, probFractalDraw)
  
  m_data=list(num_subjs = num_subjs,
              num_trials = num_trials,
              choices = choices,
              ev_left = ev_left,
              ev_right = ev_right,
              fractal_outcomes_left = fractal_outcomes_left,
              fractal_outcomes_right = fractal_outcomes_right,
              trial_pFrac = trial_pFrac)
  
  rm(num_subjs, num_trials, choices, ev_left, ev_right, fractal_outcomes_left, fractal_outcomes_right, trial_pFrac)
  
  ## Fit model for all subjects
  m = stan_model(paste0(helpers_path,'rlModels/stanModels/fit_rl_hierarchical_twoParamsAsymmNonLinearProbDistortion_rpeBoth.stan'))
  
  fit = sampling(m, data=m_data)
  saveRDS(fit, paste0(helpers_path, 'rlModels/stanModels/fit_rl_hierarchical_twoParamsAsymmNonLinearProbDistortion_rpeBoth.RDS'))
}

## Organize output
out = organize_stan_output(fit, 
                           subj_par_names=c("alpha","gamma", "delta", "beta"),
                           group_par_names=c("g_alpha","g_gamma", "g_delta", "g_beta"))
par_ests = out$par_ests
g_par_ests = out$g_par_ests
rm(out)
