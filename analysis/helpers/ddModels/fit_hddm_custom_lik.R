library(tidyr)
library(rstan)
options(mc.cores = 4)
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
if(file.exists(paste0(helpers_path, 'ddModels/stanModels/fit_hddm_custom_lik.RDS'))){
  fit = readRDS(paste0(helpers_path, 'ddModels/stanModels/fit_hddm_custom_lik.RDS'))
} else {## Otherwise fit the model
  
  ## Reshape data
  num_subjs = length(unique(clean_beh_data$subnum))
  
  num_trials = clean_beh_data %>%
    count(subnum) %>%
    select(n)
  num_trials = num_trials$n
  
  #subjects in rows, trials in columns
  clean_beh_data = clean_beh_data %>%
    mutate(choiceLeft = ifelse(choiceLeft == 0, -1, choiceLeft))
  choices = extract_var_for_stan(clean_beh_data, choiceLeft)
  
  clean_beh_data = clean_beh_data %>%
    mutate(leftLotteryEV = lotteryValue*lotteryProb,
           rightLotteryEV = referenceValue*referenceProb,
           numTimeSteps = round(reactionTime * 1000 /10))
  
  num_time_steps = extract_var_for_stan(clean_beh_data, numTimeSteps)
  
  model_name = 'fit_rl_hierarchical_oneParamSymmNonLinearProbDistortion_rpeBoth.R'
  source(paste0(helpers_path, 'rlModels/', model_name))
  source(paste0(helpers_path, 'get_qvals.R'))
  
  # Add mean posterior estimates to clean_beh_data
  clean_beh_data = par_ests %>%
    group_by(subnum, par) %>%
    summarise(est = mean(value), .groups='keep') %>%
    spread(par, est) %>%
    left_join(clean_beh_data, by='subnum')
  
  ## Add Q values of fractals to each trial
  clean_beh_data = clean_beh_data %>%
    group_by(subnum) %>%
    do(get_qvals(., model_name="original")) %>%
    ungroup()
  
  clean_beh_data = clean_beh_data %>%
    mutate(w_pi = (delta*probFractalDraw) / ((delta*probFractalDraw) + (1-probFractalDraw)),
           opt_val_left = (w_pi * leftQValue) + ((1-w_pi) * leftLotteryEV),
           opt_val_right = (w_pi * rightQValue) + ((1-w_pi) * rightLotteryEV),
           opt_val_diff = opt_val_left - opt_val_right)
  
  opt_val_diffs = extract_var_for_stan(clean_beh_data, opt_val_diff)

  m_data=list(num_subjs = num_subjs,
              num_trials = num_trials,
              choices = choices,
              num_time_steps = num_time_steps,
              opt_val_diffs = opt_val_diffs)
  
  rm(num_subjs, num_trials, choices, num_time_steps, opt_val_diffs, par_ests, g_par_ests, fit)
  
  ## Fit model for all subjects
  m = stan_model(paste0(helpers_path,'ddModels/stanModels/fit_hddm_custom_lik.stan'))
  
  fit = sampling(m, data=m_data)
  saveRDS(fit, paste0(helpers_path, 'ddModels/stanModels/fit_hddm_custom_lik.RDS'))
}

## Organize output
out = organize_stan_output(fit, 
                           subj_par_names=c("d", "s"),
                           group_par_names=c("g_d", "g_s"))
par_ests = out$par_ests
g_par_ests = out$g_par_ests
rm(out)
