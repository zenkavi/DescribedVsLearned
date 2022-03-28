
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

# int<lower=1> N;      // Number of subjects
# int<lower=0> Nu_max; // Max (across subjects) number of upper boundary responses
# int<lower=0> Nl_max; // Max (across subjects) number of lower boundary responses
# int<lower=0> Nu[N];  // Number of upper boundary responses for each subj
# int<lower=0> Nl[N];  // Number of lower boundary responses for each subj
# real RTu[N, Nu_max];  // upper boundary response times
# real RTl[N, Nl_max];  // lower boundary response times
# real minRT[N];       // minimum RT for each subject of the observed data
# real RTbound;        // lower bound or RT across all subjects (e.g., 0.1 second)

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

RTbound = min(minRT)

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
m = stan_model(paste0(helpers_path,'ddrlModels/stanModels/pure_dd.stan'))

fit = sampling(m, data=m_data)

# Simplest
# Add value difference to drift
# Add learning to fractal values 
# Add hierarchical estimation