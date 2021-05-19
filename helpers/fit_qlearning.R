library(tidyr)
library(rstan)

## Read in data if not already tjere

if (!exists('clean_beh_data')){
  helpers_path = '~/Dropbox/RangelLab/DescribedVsLearned/helpers/'
  source(paste0(helpers_path,'clean_behavioral_data.R'))
}

## Reshape data

num_subjs = length(unique(clean_beh_data$subnum))

num_trials = clean_beh_data %>%
  count(subnum) %>%
  select(n)
num_trials = num_trials$n

choices = as.matrix(clean_beh_data %>% 
                      select(subnum, choiceLeft) %>%
                      group_by(subnum) %>%
                      mutate(trial= 1:n()) %>%
                      spread(trial, choiceLeft) %>%
                      ungroup()%>%
                      select(-subnum) %>%
                      replace(is.na(.), -1))

outcomes_left = as.matrix(clean_beh_data %>% 
                            select(subnum, leftFractalReward) %>%
                            group_by(subnum) %>%
                            mutate(trial= 1:n()) %>%
                            spread(trial, leftFractalReward) %>%
                            ungroup()%>%
                            select(-subnum)%>%
                            replace(is.na(.), -1))

outcomes_right = as.matrix(clean_beh_data %>% 
                             select(subnum, rightFractalReward) %>%
                             group_by(subnum) %>%
                             mutate(trial= 1:n()) %>%
                             spread(trial, rightFractalReward) %>%
                             ungroup()%>%
                             select(-subnum)%>%
                             replace(is.na(.), -1))

## Fit model for all subjects

m = stan_model('stanModels/fit_qlearning.stan')

m_data=list(num_subjs = num_subjs,
            num_trials = num_trials,
            choices = choices,
            outcomes_left = outcomes_left,
            outcomes_right=outcomes_right)

fit = sampling(m, data=m_data)
