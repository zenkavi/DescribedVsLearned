#######################
# Usage
#######################

# Rscript --vanilla make_ddm_Roptim_startVals.R --n_vals 1000 --n_datasets 5

#######################
# Setup
#######################

set.seed(38573)

library(here)
library(optparse)
helpers_path = here('analysis/helpers/')

#######################
# Parse input arguments
#######################
option_list = list(
  make_option("--n_vals", type="integer", default=1000),
  make_option("--n_datasets", type="integer", default = 20),
  make_option("--par_names", type="character", default = c("d", "sigma", "delta", "gamma")),
  make_option("--out_path", type="character", default = 'ddModels/cluster_scripts/start_vals/')
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
# out_path = opt$out_data
out_path = paste0(helpers_path, opt$out_path)

#######################
# Generate start values
#######################
for(i in 1:n_datasets){
  out = data.frame(tmp = rep(NA, n_vals))
  
  if("d" %in% par_names){
    out$start_d = runif(n_vals, 0, 1)
  }
  
  if("sigma" %in% par_names){
    out$start_sigma = runif(n_vals, 0, 1)
  }
  
  if("delta" %in% par_names){
    out$start_delta = runif(n_vals, 1, 8)
  }
  
  if("gamma" %in% par_names){
    out$start_gamma = runif(n_vals, 1, 8)
  }
  
  
  out = out[,-which(names(out)=="tmp")]
  
  #######################
  # Save output
  #######################
  write.table(out, file = paste0(out_path, 'ddm_Roptim_start_vals', i,'.csv'), row.names = F, col.names = F, sep=',')
}

