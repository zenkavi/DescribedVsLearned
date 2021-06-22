library(tidyverse)
library(here)

if(!exists(clean_beh_data)){
  helpers_path = here('helpers/')
  source(paste0(helpers_path,'01_clean_behavioral_data.R'))
}

organize_stan_output = function(fit, par_names, subnums=unique(clean_beh_data$subnum)){
  # Extract parameters from fit object
  par_ests = data.frame(extract(fit, par_names))  %>%
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
  
  log_liks = data.frame(extract(fit, c("logLikelihood")))  %>%
    gather(key, value) %>%
    separate(key, c('par', 'subj'), sep='\\.')
  
  # Add correct subject identifiers
  log_liks = data.frame(subnum = unique(clean_beh_data$subnum)) %>%
    mutate(subj = as.character(1:n())) %>%
    right_join(log_liks, by='subj') %>%
    select(-subj) %>%
    group_by(subnum, par) %>%
    mutate(iter = 1:n()) %>%
    ungroup() %>%
    select(-par) %>%
    rename(logLik=value)
  
  par_ests = par_ests %>%
    left_join(log_liks, by=c("subnum", "iter")) %>%
    select(-iter)
  
  return (par_ests)
}