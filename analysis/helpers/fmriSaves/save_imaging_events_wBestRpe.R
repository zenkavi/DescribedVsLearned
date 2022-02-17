library(tidyverse)
library(here)

helpers_path = here('analysis/helpers/')
source(paste0(helpers_path,'add_inferred_pars.R'))

source(paste0(helpers_path,'rlModels/fit_rl_hierarchical_rpeWhenFractalRewarded.R'))
fit_rpeWhenFractalRewarded = fit
g_par_ests_rpeWhenFractalRewarded = g_par_ests
par_ests_rpeWhenFractalRewarded = par_ests

source(paste0(helpers_path,'rlModels/fit_rl_hierarchical_rpeChosenBundleFractal.R'))
fit_rpeChosenBundleFractal = fit
g_par_ests_rpeChosenBundleFractal = g_par_ests
par_ests_rpeChosenBundleFractal = par_ests

source(paste0(helpers_path,'rlModels/fit_rl_hierarchical.R'))

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

clean_beh_data_original = add_inferred_pars(clean_beh_data, par_ests, model_name="original")
clean_beh_data_rpeChosenBundleFractal = add_inferred_pars(clean_beh_data, par_ests_rpeChosenBundleFractal, model_name="rpeChosenBundleFractal")
clean_beh_data_rpeWhenFractalRewarded = add_inferred_pars(clean_beh_data, par_ests_rpeWhenFractalRewarded, model_name="rpeWhenFractalRewarded")

best_model_for_subj = par_ests %>%
  mutate(model = "original") %>%
  rbind(par_ests_rpeChosenBundleFractal %>% mutate(model = "rpeChosenBundleFractal")) %>%
  rbind(par_ests_rpeWhenFractalRewarded %>% mutate(model = "rpeWhenFractalRewarded")) %>%
  group_by(model, subnum) %>%
  summarise(aveLL = mean(logLik),.groups='keep') %>%
  ungroup() %>%
  group_by(subnum) %>%
  mutate(minaveLL = min(aveLL)) %>%
  spread(model, aveLL) %>%
  mutate(winningModel = ifelse(minaveLL == original, "original", ifelse(minaveLL == rpeChosenBundleFractal, "rpeChosenBundleFractal", "rpeWhenFractalRewarded"))) %>%
  select(-minaveLL)

clean_beh_data = as.data.frame(matrix(ncol = ncol(clean_beh_data_original)))
names(clean_beh_data) = names(clean_beh_data_original)

for(i in 1:nrow(best_model_for_subj)){
  cur_sub = best_model_for_subj$subnum[i]
  if(best_model_for_subj$winningModel[i] == "rpeChosenBundleFractal"){
    sub_data = clean_beh_data_rpeChosenBundleFractal %>% filter(subnum == cur_sub)
  } else if(best_model_for_subj$winningModel[i] == "rpeWhenFractalRewarded"){
    sub_data = clean_beh_data_rpeWhenFractalRewarded %>% filter(subnum == cur_sub)
  } else {
    sub_data = clean_beh_data_original %>% filter(subnum == cur_sub)
  }
  
  clean_beh_data = rbind(clean_beh_data, sub_data)
}

clean_beh_data = clean_beh_data %>% drop_na()

# Read in data with excluded trials
source(paste0(helpers_path,'00_get_behavioral_data.R'))

junk_trials = beh_data %>%
  mutate(junk = ifelse(responded != 1 | reactionTime <= .2, 1, 0)) %>%
  filter(junk == 1)

# Add junk trials to clean trials and order by subject, run, trial number
all_trials = rbind.all.columns(clean_beh_data, junk_trials) %>%
  arrange(subnum, session, trialNum)

# Write out output
write_csv(all_trials, '~/Downloads/GTavares_2017_arbitration/behavioral_data/all_trials_wBestRpe.csv')
