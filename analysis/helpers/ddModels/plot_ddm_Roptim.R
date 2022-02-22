#######################
# Usage
#######################

# Rscript --vanilla plot_ddm_Roptim.R --data_names sim_single_sub_data1, sim_single_sub_data2

#######################
# Setup
#######################

library(here)
library(optparse)
helpers_path = here('analysis/helpers/')

#######################
# Parse input arguments
#######################
option_list = list(
  make_option("--data_names", type="character", default = c('sim_single_sub_data1', 'sim_single_sub_data2')),
  make_option("--fig_out_path", type="character", default = '/outputs/fig/'),
  make_option("--optim_out_path", type="character", default = 'ddModels/cluster_scripts/optim_out'),
  make_option("--model", type="character", default = 'model1a')
) 

opt_parser = OptionParser(option_list=option_list)
opt = parse_args(opt_parser)

#######################
# Initialize parameters from input arguments
#######################

# If using string input must be separated by ", " (with trailing space)
data_names = opt$data_names
if(length(data_names) == 1){
  if(grepl(',', data_names)){
    data_names = strsplit(data_names, ', ')[[1]] 
  }
}

# Must end with /
# out_path = opt$out_data
fig_out_path = paste0(here(), opt$fig_out_path)

optim_out_path = paste0(helpers_path, opt$optim_out_path)

model = opt$model

#######################
# Loop through datasets
#######################

source(paste0(helpers_path, 'ddModels/ddm_par_recovery_report.R'))

n_datasets = length(data_names)

# Split by 4 rows

for(i in 1:n_datasets){
  tmp = ddm_par_recovery_report(model_ = model, data_ = data_names[i], optim_out_path_ = optim_out_path)
}