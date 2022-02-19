set.seed(38573)

library(here)
library(optparse)
helpers_path = here('analysis/helpers/')

#######################
# Parse input arguments
#######################
option_list = list(
  make_option("--n_vals", type="integer", default=1000),
  make_option("--n_datasets", type="integer", default = 5),
  make_option("--par_names", type="character", default = c("d", "sigma", "delta", "gamma")),
  make_option("--out_path", type="character", default = '/ddModels/cluster_scripts/start_vals/')
) 

opt_parser = OptionParser(option_list=option_list)
opt = parse_args(opt_parser)

#######################
# Initialize parameters from input arguments
#######################
n_vals = opt$n_vals

n_datasets = opt$n_datasets

# If using string input must be separated by ", " (with trailing space)
par_names = opt$par_names
if(length(par_names) == 1){
  if(grepl(',', par_names)){
    par_names = strsplit(par_names, ', ')[[1]] 
  }
}

# Must end with /
out_path = opt$out_data

#######################
# Generate start values
#######################
if()

for(i in 1:n_datasets){
  out = data.frame(start_d = runif(n, 0, 1),
                   start_sigma = runif(n, 0, 1),
                   start_delta = runif(n, 1, 5),
                   start_gamma = runif(n, 1, 5))
  
  #######################
  # Save output
  #######################
  write.table(out, file = paste0(out_path, 'ddm_Roptim_start_vals', i,'.csv'), row.names = F, col.names = F, sep=',')
}

