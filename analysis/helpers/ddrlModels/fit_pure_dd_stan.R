
library(tidyr)
library(rstan)
library(here)

helpers_path = here('analysis/helpers/')

## Read in data if not already there
if (!exists('clean_beh_data')){
  source(paste0(helpers_path,'01_clean_behavioral_data.R'))
}

if(!exists('organize_stan_output')){
  source(paste0(helpers_path, 'rlModels/organize_stan_output.R'))
}

if(file.exists(paste0(helpers_path, 'ddrlModels/stanModels/pure_dd.RDS'))){
  fit = readRDS(paste0(helpers_path, 'ddrlModels/stanModels/pure_dd.RDS'))
} else {
  
  N = length(unique(clean_beh_data$subnum))
  
  tmp = clean_beh_data %>%
    group_by(subnum) %>%
    summarise(sumLeft = sum(choiceLeft),
              sumRight= sum(1-choiceLeft))
  
  Nu_max = max(tmp$sumLeft)
  Nl_max = max(tmp$sumRight)
  
  Nu = tmp$sumLeft
  Nl = tmp$sumRight
  
  RTu = as.matrix(clean_beh_data %>%
                    filter(choiceLeft ==1) %>%
                    select(subnum, reactionTime) %>%
                    group_by(subnum) %>%
                    mutate(trial= 1:n()) %>%
                    spread(trial, reactionTime) %>%
                    ungroup()%>%
                    select(-subnum)%>%
                    replace(is.na(.), -1))
  
  RTl = as.matrix(clean_beh_data %>%
                    filter(choiceLeft ==0) %>%
                    select(subnum, reactionTime) %>%
                    group_by(subnum) %>%
                    mutate(trial= 1:n()) %>%
                    spread(trial, reactionTime) %>%
                    ungroup()%>%
                    select(-subnum)%>%
                    replace(is.na(.), -1))
  
  tmp = clean_beh_data %>%
    group_by(subnum) %>%
    summarise(minRT = min(reactionTime))
  
  minRT = tmp$minRT
  
  # RTbound = min(minRT) #Sampling didn't work with this
  RTbound = .1
  
  m_data=list(N = N,
              Nu_max = Nu_max,
              Nl_max = Nl_max,
              Nu = Nu,
              Nl = Nl,
              RTu = RTu,
              RTl = RTl,
              minRT = minRT,
              RTbound = RTbound)
  
  rm(N, Nu_max, Nl_max, Nu, Nl, RTu, RTl, minRT, RTbound)
  
  ## Fit model for all subjects
  # For all subjects individually (non-hierarchical) estimates thresholds, NDTs, bias and drift rate
  m = stan_model(paste0(helpers_path,'ddrlModels/stanModels/pure_dd.stan'))
  
  fit = sampling(m, data=m_data)
  saveRDS(fit, paste0(helpers_path, 'ddrlModels/stanModels/pure_dd.RDS')) 
}

out = organize_stan_output(fit, 
                           subj_par_names=c("alpha","beta", "delta", "tau"),
                           log_lik_var_name = c("log_lik"))

par_ests = out$par_ests
rm(out)

# Simplest
# Add value difference to drift
# Add learning to fractal values 
# Add hierarchical estimation