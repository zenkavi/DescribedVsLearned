library(tidyr)
library(rstan)
library(here)

helpers_path = here('helpers/')

## Read in data if not already there
if (!exists('clean_beh_data')){
  source(paste0(helpers_path,'01_clean_behavioral_data.R'))
}

if(!exists('extract_var_for_stan')){
  source(paste0(helpers_path, 'extract_var_for_stan.R'))
}

if(!exists('get_qvals')){
  source(paste0(helpers_path, 'get_qvals.R'))
}

## If there is a fit object read it in
if(file.exists(paste0(helpers_path, 'stanModels/fit_twoValSystemsWithRL.RDS'))){
  fit = readRDS(paste0(helpers_path, 'stanModels/fit_twoValSystemsWithRL.RDS'))
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
  m = stan_model('stanModels/fit_twoValSystemsWithRL.stan')
  
  m_data=list(num_subjs = num_subjs,
              num_trials = num_trials,
              choices = choices,
              outcomes_left = outcomes_left,
              outcomes_right=outcomes_right)
  
  fit = sampling(m, data=m_data)
  saveRDS(fit, paste0(helpers_path, 'stanModels/fit_twoValSystemsWithRL.RDS'))
}

# Extract parameters from fit object
par_ests = data.frame(extract(fit, c("alpha","gamma", "delta", "beta")))  %>%
  gather(key, value) %>%
  separate(key, c('par', 'subj'), sep='\\.')

# Add correct subject identifiers
par_ests = data.frame(subnum = unique(clean_beh_data$subnum)) %>%
  mutate(subj = as.character(1:n())) %>%
  right_join(par_ests, by='subj') %>%
  select(-subj)

## Save median estimates to `clean_beh_data`
clean_beh_data = par_ests %>%
  group_by(subnum, par) %>%
  summarise(est = median(value), .groups='keep') %>%
  spread(par, est) %>%
  left_join(clean_beh_data, by='subnum')

## Add Q values to each trial
clean_beh_data = clean_beh_data %>%
  group_by(subnum) %>%
  do(get_qvals(.)) %>%
  ungroup()

rm(get_qvals)
