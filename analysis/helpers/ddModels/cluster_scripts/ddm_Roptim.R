#!/usr/bin/env Rscript

library(optparse)
library(visualMLE)
source(paste0(helpers_path,'ddmSims/fit_task.R'))

option_list = list(
  make_option(c("--data"), type="character", default=NULL),
  make_option(c("--start_vals"), type="character"),
  make_option(c("--model_name"), type="character"),
  make_option(c("--max_iter"), type="integer", default = 500),
  make_option(c("--par_names"), type="character", default = c("d", "sigma", "delta", "gamma")),
  make_option(c("--out_path"), type="character")
) 

opt_parser = OptionParser(option_list=option_list)
opt = parse_args(opt_parser)

# Initialize parameters from input arguments

data = read.csv(opt$data)

start_vals = ...

model_name = opt$model_name
source(paste0(helpers_path, 'ddmSims/r_ddm_models/', model_name,'.R'))
sim_trial_list = list()
fit_trial_list = list()
sim_trial_list[[model_name]] = sim_trial
fit_trial_list[[model_name]] = fit_trial

max_iter = opt$max_iter

par_names = ...

out_path = opt$out_path

# Run optim
optim_out = optim_save(par = start_vals, get_task_nll, data_=..., par_names_ = par_names, model_name_ = model_name, control = list(maxit=max_iter))

# Save iterations
'optim_iter_', start_vals, '_.csv'

# Save converged parameters
'optim_par_', start_vals, '_.csv'