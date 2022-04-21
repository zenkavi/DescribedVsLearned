library(tidyr)
library(rstan)
library(here)

options(mc.cores = 4)
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
if(file.exists(paste0(helpers_path, 'ddrlModels/stanModels/fit_model1d.RDS'))){
  fit = readRDS(paste0(helpers_path, 'ddrlModels/stanModels/fit_model1d.RDS'))
} else {## Otherwise fit the model
  
  ## Reshape data
  num_subjs = length(unique(clean_beh_data$subnum))
  
  num_trials = clean_beh_data %>%
    count(subnum) %>%
    select(n)
  num_trials = num_trials$n
  
  #subjects in rows, trials in columns
  clean_beh_data = clean_beh_data %>% mutate(choices = ifelse(choiceLeft == 1, 1, -1))
  choices = extract_var_for_stan(clean_beh_data, choices)
  
  clean_beh_data = clean_beh_data %>%
    mutate(leftLotteryEV = lotteryValue*lotteryProb,
           rightLotteryEV = referenceValue*referenceProb,
           numTimeSteps = round(reactionTime * 1000 /10))
  
  ev_left = extract_var_for_stan(clean_beh_data, leftLotteryEV)
  
  ev_right = extract_var_for_stan(clean_beh_data, rightLotteryEV)
  
  fractal_outcomes_left = extract_var_for_stan(clean_beh_data, leftFractalReward)
  
  fractal_outcomes_right = extract_var_for_stan(clean_beh_data, rightFractalReward)
  
  trial_pFrac = extract_var_for_stan(clean_beh_data, probFractalDraw)
  
  response_times = extract_var_for_stan(clean_beh_data, reactionTime)
  response_times = ifelse(response_times < 0.11, 0.11, response_times) # artificial lower bound to avoid issues with the nondecision time fixed at .1
  
  m_data=list(num_subjs = num_subjs,
              num_trials = num_trials,
              choices = choices,
              ev_left = ev_left,
              ev_right = ev_right,
              fractal_outcomes_left = fractal_outcomes_left,
              fractal_outcomes_right = fractal_outcomes_right,
              trial_pFrac = trial_pFrac,
              response_times = response_times)
  
  rm(num_subjs, num_trials, choices, ev_left, ev_right, fractal_outcomes_left, fractal_outcomes_right, trial_pFrac, response_times)
  
  ## Fit model for all subjects
  m = stan_model(paste0(helpers_path,'ddrlModels/stanModels/fit_model1d.stan'))
  
  fit = sampling(m, data=m_data)
  saveRDS(fit, paste0(helpers_path, 'ddrlModels/stanModels/fit_model1d.RDS'))
}

## Organize output
out = organize_stan_output(fit, 
                           subj_par_names=c("alpha", "d", "delta", "s"),
                           group_par_names=c("g_alpha", "g_d", "g_delta", "g_s"),
                           log_lik_var_name = NA)
par_ests = out$par_ests
g_par_ests = out$g_par_ests
rm(out)
