#######################
# Purpose
#######################

# Simulate datasets using the same stimuli (EVs and QVs), fgrid of drift rates, sigmas and deltasfor parameter recovery exercise sim3

library(tidyverse)
library(here)
helpers_path = here('analysis/helpers/')

true_ds = c(.001, .06 , .5)
true_sigmas = c(.001, .08, .3)
true_deltas = c(.1, .5 , 1, 3)

out_path = paste0(helpers_path, 'ddModels/cluster_scripts/test_data/')

source(paste0(helpers_path,'ddModels/sim_task.R'))
model = "model1c"
source(paste0(helpers_path, 'ddModels/r_ddm_models/ddm_', model,'.R'))
sim_trial_list = list()
fit_trial_list = list()
sim_trial_list[[model]] = sim_trial
fit_trial_list[[model]] = fit_trial

sub_data = read.csv(paste0(out_path, 'true_single_sub_data.csv'))
sub_data = sub_data %>%
  select(leftQValue, rightQValue, leftLotteryEV, rightLotteryEV, probFractalDraw) %>%
  rename(QVLeft = leftQValue, QVRight = rightQValue, EVLeft = leftLotteryEV, EVRight = rightLotteryEV)

set.seed(37943)

count = 46 # There are 45 test datasets from before. Starting these from 46.
for(i in 1:length(true_ds)){
  true_d = true_ds[i]
  
  for(j in 1:length(true_sigmas)){
    true_sigma = true_sigmas[j]
    
    for(k in 1:length(true_deltas)){
      true_delta = true_deltas[k]
      
      sim_data = sim_task(sub_data, model_name = model, d = true_d, sigma = true_sigma, delta = true_delta) %>% drop_na()
      
      write.csv(sim_data, file = paste0(out_path, 'sim_single_sub_data', count,'.csv'), row.names = F)
      count = count+1
    }
  }
}
