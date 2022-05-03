library(tidyverse)
library(here)

helpers_path = here('analysis/helpers/')
source(paste0(helpers_path,'rlModels/fit_rl_hierarchical__twoParamsSymmNonLinearProbDistortion_rpeBoth.R'))
source(paste0(helpers_path,'add_inferred_pars.R'))

clean_beh_data = add_inferred_pars(clean_beh_data, par_ests)

# Read in data with excluded trials
source(paste0(helpers_path,'00_get_behavioral_data.R'))

junk_trials = beh_data %>%
  mutate(junk = ifelse(responded != 1 | reactionTime <= .2, 1, 0)) %>%
  filter(junk == 1)

# Function to rbind df's when they don't have all the same columns
rbind.all.columns <- function(x, y) {
  
  if(ncol(x) == 0 | ncol(y) == 0){
    out = plyr::rbind.fill(x, y)
  } else{
    x.diff <- setdiff(colnames(x), colnames(y))
    y.diff <- setdiff(colnames(y), colnames(x))
    x[, c(as.character(y.diff))] <- NA
    y[, c(as.character(x.diff))] <- NA
    out = rbind(x, y)
  }
  return(out)
}

# Add junk trials to clean trials and order by subject, run, trial number
all_trials = rbind.all.columns(clean_beh_data, junk_trials) %>%
  arrange(subnum, session, trialNum)

# Write out output
write_csv(all_trials, '~/Downloads/GTavares_2017_arbitration/behavioral_data/all_trials.csv')
