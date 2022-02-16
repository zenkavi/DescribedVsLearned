#!/usr/bin/env Rscript

library(here)
library(optparse)
library(visualMLE)

helpers_path = here('analysis/helpers/')
source(paste0(helpers_path,'ddmSims/fit_task.R'))


#######################
# Parse input arguments
#######################
option_list = list(
  make_option("--data", type="character", default=NULL),
  make_option("--start_vals", type="character"),
  make_option("--model", type="character"),
  make_option("--max_iter", type="integer", default = 500),
  make_option("--par_names", type="character", default = c("d", "sigma", "delta", "gamma")),
  make_option("--out_path", type="character")
) 

opt_parser = OptionParser(option_list=option_list)
opt = parse_args(opt_parser)

#######################
# Initialize parameters from input arguments
#######################
data = read.csv(opt$data)

start_vals = strsplit(opt$start_vals, ",")[[1]]

model = opt$model
source(paste0(helpers_path, 'ddmSims/r_ddm_models/', model,'.R'))
sim_trial_list = list()
fit_trial_list = list()
sim_trial_list[[model]] = sim_trial
fit_trial_list[[model]] = fit_trial

max_iter = opt$max_iter

par_names = opt$par_names
if(grepl(',', par_names)){
  par_names = strsplit(par_names, ',')[[1]]
}

out_path = opt$out_path

#######################
# Run optim
#######################
optim_out = optim_save(par = start_vals, get_task_nll, data_= data, par_names_ = par_names, model_name_ = model, control = list(maxit=max_iter))

suffix = paste(format(Sys.time(), "%F-%H-%M-%S"), round(runif(1, max=1000)), sep="_")

#######################
# Save output
#######################
write.csv(optim_out$iterations_df, paste0(out_path, 'optim_iter_', model ,'_', suffix,'.csv'), row.names=FALSE)
write.csv(optim_out$par, paste0(out_path, 'optim_par_', model ,'_', suffix,'.csv'), row.names=FALSE)

