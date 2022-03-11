#######################
# Purpose
#######################

# Simulate datasets using the same stimuli (EVs and QVs), fixed d and sigma and a grid of deltas and gammas for parameter recovery exercise sim2

library(tidyverse)
library(here)
helpers_path = here('analysis/helpers/')

true_ds = c(.06 , .5)
true_sigmas = c(.08, .3)
true_alphas = c(.1, .5)
true_deltas = c(.1, 1, 3)

out_path = paste0(helpers_path, 'ddrlModels/cluster_scripts/test_data/')

source(paste0(helpers_path,'ddrlModels/sim_task_sequential.R'))
model = "model1c"
source(paste0(helpers_path, 'ddrlModels/r_dd_rl_models/ddrl_', model,'.R'))
sim_trial_list = list()
fit_trial_list = list()
sim_trial_list[[model]] = sim_trial
fit_trial_list[[model]] = fit_trial

sub_data = read.csv(paste0(out_path, 'true_single_sub_data.csv'))
sub_data = sub_data %>%
  select(leftLotteryEV, rightLotteryEV, probFractalDraw, leftFractalReward, rightFractalReward) %>%
  rename(EVLeft = leftLotteryEV, EVRight = rightLotteryEV)

set.seed(38923)

count = 1 # There are 20 test datasets from before. Starting these from 21.
for(i in 1:length(true_ds)){
  true_d = true_ds[i]
  
  for(j in 1:length(true_sigmas)){
    true_sigma = true_sigmas[j]
    
    for(k in 1:length(true_alphas)){
      true_alpha = true_alphas[k]
      
      for(l in 1:length(true_deltas)){
        
        true_delta = true_deltas[l]
        
        sim_data = sim_task_sequential(sub_data, model_name = model, d = true_d, sigma = true_sigma, alpha = true_alpha, delta = true_delta) %>% drop_na()
        
        write.csv(sim_data, file = paste0(out_path, 'sim_single_sub_data', count,'.csv'), row.names = F)
        count = count+1
        
      }
    }
  }
}
