#!/usr/bin/env Rscript

library(here)
library(optparse)
library(visualMLE)

# Note this will be run in docker container so /ddModels must be mounted
helpers_path = here("/ddModels/")
source(paste0(helpers_path,'fit_task.R'))


#######################
# Parse input arguments
#######################
option_list = list(
  make_option("--data", type="character", default='test_trial_conditions'),
  make_option("--start_vals", type="character"),
  make_option("--model", type="character"),
  make_option("--max_iter", type="integer", default = as.integer(500)),
  make_option("--par_names", type="character", default = c("d", "sigma", "delta", "gamma")),
  make_option("--fix_par_names", type="character"),
  make_option("--fix_par_vals", type="character"),
  make_option("--out_path", type="character", default = 'sim0')
) 

opt_parser = OptionParser(option_list=option_list)
opt = parse_args(opt_parser)

#######################
# Initialize parameters from input arguments
#######################
data_suffix = opt$data
data = read.csv(paste0(helpers_path, 'cluster_scripts/test_data/', opt$data, '.csv'))

# Convert to numeric so optim can work with it
start_vals = as.numeric(strsplit(opt$start_vals, ",")[[1]])

model = opt$model
source(paste0(helpers_path, 'r_ddm_models/ddm_', model,'.R'))
sim_trial_list = list()
fit_trial_list = list()
sim_trial_list[[model]] = sim_trial
fit_trial_list[[model]] = fit_trial

max_iter = opt$max_iter

par_names = opt$par_names
# If using string input convert to vector
if(length(par_names) == 1){
  if(grepl(',', par_names)){
    par_names = gsub(" ", "", par_names) #remove spaces
    par_names = strsplit(par_names, ',')[[1]] 
  }
}

# Convert fixed parameter info to list format that get_nll > fit_task needs
if(length(opt$fix_par_names)>1){
  fix_par_names = opt$fix_par_names
  if(length(fix_par_names) == 1){
    if(grepl(',', fix_par_names)){
      fix_par_names = gsub(" ", "", fix_par_names) #remove spaces
      fix_par_names = strsplit(fix_par_names, ',')[[1]] 
    }
  }
  fix_par_vals = as.numeric(strsplit(opt$fix_par_vals, ",")[[1]])
  fix_pars = setNames(as.list(fix_par_vals), fix_par_names)
  
} else {
  fix_pars = list()
}


# Must end with /
out_path = paste0(helpers_path, 'cluster_scripts/optim_out/',opt$out_path)

#######################
# Run optim
#######################
optim_out = optim_save(par = start_vals, get_task_nll, data_= data, par_names_ = par_names, model_name_ = model, fix_pars_ = fix_pars, control = list(maxit=max_iter))

suffix = paste(format(Sys.time(), "%F-%H-%M-%S"), round(runif(1, max=1000)), sep="_")
suffix = paste0(model ,'_', data_suffix, '_', suffix, '.csv')

#######################
# Save output
#######################
dir.create(out_path, showWarnings = FALSE)

write.csv(optim_out$iterations_df, paste0(out_path, '/optim_iter_', suffix), row.names=FALSE)
write.csv(optim_out$par, paste0(out_path, '/optim_par_', suffix), row.names=FALSE)

