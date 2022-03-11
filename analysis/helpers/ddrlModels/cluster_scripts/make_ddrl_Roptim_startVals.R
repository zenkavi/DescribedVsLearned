#######################
# Usage
#######################

# Rscript --vanilla make_ddrl_Roptim_startVals.R --n_vals 1000 --n_datasets 25 --count_start 20
# Rscript --vanilla make_ddrl_Roptim_startVals.R --n_vals 250 --n_datasets 25 --count_start 0 --par_names d,sigma,alpha,delta --fn_prefix sub_start_vals

# Push these back to s3
# docker run --rm -it -v ~/.aws:/root/.aws -v $(pwd):/cluster_scripts amazon/aws-cli s3 sync /cluster_scripts s3://described-vs-experienced/ddrlModels/cluster_scripts 

#######################
# Setup
#######################

set.seed(34573)

library(here)
library(optparse)
helpers_path = here('analysis/helpers/')

#######################
# Parse input arguments
#######################
option_list = list(
  make_option("--n_vals", type="integer", default=1000),
  make_option("--n_datasets", type="integer", default = 20),
  make_option("--par_names", type="character", default = c("d", "sigma", "alpha", "delta")),
  make_option("--out_path", type="character", default = 'ddrlModels/cluster_scripts/start_vals/'),
  make_option("--count_start", type="integer", default = 0),
  make_option("--fn_prefix", type="character", default='ddrl_Roptim_start_vals')
) 

opt_parser = OptionParser(option_list=option_list)
opt = parse_args(opt_parser)

#######################
# Initialize parameters from input arguments
#######################
n_vals = opt$n_vals

n_datasets = opt$n_datasets

# If using string input convert to vector
par_names = opt$par_names
if(length(par_names) == 1){
  if(grepl(',', par_names)){
    par_names = gsub(" ", "", par_names) #remove spaces
    par_names = strsplit(par_names, ',')[[1]]
  }
}

# Must end with /
# out_path = opt$out_data
out_path = paste0(helpers_path, opt$out_path)

count_start = opt$count_start

fn_prefix = opt$fn_prefix

#######################
# Generate start values
#######################
for(i in 1:n_datasets){
  out = data.frame(tmp = rep(NA, n_vals))
  
  if("d" %in% par_names){
    out$start_d = runif(n_vals, 0.00001, 1)
  }
  
  if("sigma" %in% par_names){
    out$start_sigma = runif(n_vals, 0.00001, 1)
  }
  
  if("alpha" %in% par_names){
    out$start_alpha = runif(n_vals, 0.00001, 1)
  }
  
  if("delta" %in% par_names){
    out$start_delta = runif(n_vals, 0.00001, 4)
  }
  
  
  # Drop temporary initializing column
  out = out[,-which(names(out)=="tmp")]
  
  #######################
  # Save output
  #######################
  write.table(out, file = paste0(out_path, fn_prefix, i+count_start,'.csv'), row.names = F, col.names = F, sep=',')
}

