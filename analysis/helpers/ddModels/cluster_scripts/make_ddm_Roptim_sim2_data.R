#######################
# Purpose
#######################

# Simulate datasets using the same stimuli (EVs and QVs), fixed d and sigma and a grid of deltas and gammas for parameter recovery exercise sim2

library(tidyverse)
library(here)
helpers_path = here('analysis/helpers/')

true_d = .1
true_sigma = 0.08
true_deltas = c(.1, .2, 1, 3, 5)
true_gammas = c(.1, .2, 1, 3, 5)

out_path = paste0(helpers_path, 'ddModels/cluster_scripts/test_data/')

source(paste0(helpers_path,'ddModels/sim_task.R'))
model = "model1a"
source(paste0(helpers_path, 'ddModels/r_ddm_models/ddm_', model,'.R'))
sim_trial_list = list()
fit_trial_list = list()
sim_trial_list[[model]] = sim_trial
fit_trial_list[[model]] = fit_trial

sub_data = read.csv(paste0(out_path, 'true_single_sub_data.csv'))
sub_data = sub_data %>%
  select(leftQValue, rightQValue, leftLotteryEV, rightLotteryEV, probFractalDraw) %>%
  rename(QVLeft = leftQValue, QVRight = rightQValue, EVLeft = leftLotteryEV, EVRight = rightLotteryEV)

set.seed(38923)

count = 21 # There are 20 test datasets from before. Starting these from 21.
for(i in 1:length(true_deltas)){
  true_delta = true_deltas[i]
  
  for(j in 1:length(true_gammas)){
    true_gamma = true_gammas[j]
    
    sim_data = sim_task(sub_data, model_name = model, d = true_d, sigma = true_sigma, delta = true_delta, gamma = true_gamma) %>% drop_na()
    
    write.csv(sim_data, file = paste0(out_path, 'sim_single_sub_data', count,'.csv'), row.names = F)
    count = count+1
  }
}