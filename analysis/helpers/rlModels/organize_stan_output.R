library(tidyverse)
library(here)

if(!exists("clean_beh_data")){
  helpers_path = here('analysis/helpers/')
  source(paste0(helpers_path,'01_clean_behavioral_data.R'))
}

organize_stan_output = function(fit, subj_par_names, subnums=unique(clean_beh_data$subnum), group_par_names=NA, log_lik_var_name = c("logLikelihood")){
  
  # Extract parameters from fit object
  par_ests = data.frame(extract(fit, subj_par_names))  %>%
    gather(key, value) %>%
    separate(key, c('par', 'subj'), sep='\\.')
  
  # Add correct subject identifiers
  par_ests = data.frame(subnum = subnums) %>%
    mutate(subj = as.character(1:n())) %>%
    right_join(par_ests, by='subj') %>%
    select(-subj) %>%
    group_by(subnum, par) %>%
    mutate(iter = 1:n()) %>%
    ungroup()
  
  if(!is.na(log_lik_var_name)[1]){
    # Extract loglikelihoods from fit object
    log_liks = data.frame(extract(fit, log_lik_var_name))  %>%
      gather(key, value) %>%
      separate(key, c('par', 'subj'), sep='\\.')
    
    # Add correct subject identifiers
    log_liks = data.frame(subnum = subnums) %>%
      mutate(subj = as.character(1:n())) %>%
      right_join(log_liks, by='subj') %>%
      select(-subj) %>%
      group_by(subnum, par) %>%
      mutate(iter = 1:n()) %>%
      ungroup() %>%
      select(-par) %>%
      rename(logLik=value)
    
    # Add log likelihoods to subject par estimates
    par_ests = par_ests %>%
      left_join(log_liks, by=c("subnum", "iter")) %>%
      select(-iter)
  }
  
  # Extract group parameters if any
  if (!is.na(group_par_names[1])){
    
    g_par_ests = data.frame(extract(fit, group_par_names))  %>%
      gather(key, value)
    
    out = list(par_ests = par_ests,
               g_par_ests = g_par_ests)
  } else{
    out = list(par_ests = par_ests)}
  
  return (out)
}