#######################
# Usage
#######################

# Rscript --vanilla make_ddm_Roptim_testData.R --model model1a --n_datasets 3

#######################
# Setup
#######################

set.seed(38923)

library(here)
library(optparse)
library(tidyverse)
helpers_path = here('analysis/helpers/ddModels/')
source(paste0(helpers_path,'sim_task.R'))

#######################
# Parse input arguments
#######################
option_list = list(
  make_option("--n_datasets", type="integer", default = 20),
  make_option("--model", type="character"),
  make_option("--par_names", type="character", default = c("d", "sigma", "delta", "gamma")),
  make_option("--out_path", type="character", default = 'cluster_scripts/test_data/')
) 

opt_parser = OptionParser(option_list=option_list)
opt = parse_args(opt_parser)

#######################
# Initialize parameters from input arguments
#######################
n_datasets = opt$n_datasets

model = opt$model
source(paste0(helpers_path, 'r_ddm_models/ddm_', model,'.R'))
sim_trial_list = list()
fit_trial_list = list()
sim_trial_list[[model]] = sim_trial
fit_trial_list[[model]] = fit_trial

# If using string input must be separated by ", " (with trailing space)
par_names = opt$par_names
if(length(par_names) == 1){
  if(grepl(',', par_names)){
    par_names = strsplit(par_names, ', ')[[1]] 
  }
}

# Must end with /
# out_path = opt$out_data
out_path = paste0(helpers_path, opt$out_path)

#######################
# Generate simulated datasets
#######################
for(i in 1:n_datasets){

  # Sample 
  if("d" %in% par_names){
    true_d = round(runif(1, 0, .1), 2)
  }
  
  if("sigma" %in% par_names){
    true_sigma = round(runif(1, 0, .1), 2)
  }
  
  if("delta" %in% par_names){
    true_delta = round(runif(1, 1, 8), 1)
  }
  
  if("gamma" %in% par_names){
    true_gamma = round(runif(1, 1, 8), 1)
  }
  
  sub_data = read.csv(paste0(out_path, 'true_single_sub_data.csv'))
  sub_data = sub_data %>%
    select(leftQValue, rightQValue, leftLotteryEV, rightLotteryEV, probFractalDraw) %>%
    rename(QVLeft = leftQValue, QVRight = rightQValue, EVLeft = leftLotteryEV, EVRight = rightLotteryEV)
  
  sub_data = sim_task(sub_data, model_name = model, d = true_d, sigma = true_sigma, delta = true_delta, gamma = true_gamma) %>% drop_na()
  
  #######################
  # Save output
  #######################
  write.csv(sub_data, file = paste0(out_path, 'sim_single_sub_data', i,'.csv'), row.names = F)
}