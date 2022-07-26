# Usage
# Rscript --vanilla save_sub_data.R --rl_model oneParamSymmLinear --no_ext F

library(tidyverse)
library(here)
library(optparse)
helpers_path = here('analysis/helpers/')

source(paste0(helpers_path,'01_clean_behavioral_data.R'))
source(paste0(helpers_path, 'get_qvals.R'))

#######################
# Parse input arguments
#######################
option_list = list(
  make_option("--rl_model", type="character"),
  make_option("--no_ext", type="character")
) 

opt_parser = OptionParser(option_list=option_list)
opt = parse_args(opt_parser)

rl_model = opt$rl_model
no_ext = ifelse(opt$no_ext == "T", T, F)

out_dir = paste0(helpers_path, 'ddModels/cluster_scripts/sub_data_',rl_model)

source(paste0(helpers_path, 'rlModels/fit_rl_hierarchical_', rl_model, 'ProbDistortion_rpeBoth.R'))

clean_beh_data = par_ests %>%
  group_by(subnum, par) %>%
  summarise(est = mean(value), .groups='keep') %>%
  spread(par, est) %>%
  left_join(clean_beh_data, by='subnum')

rm(fit, g_par_ests, par_ests)

## Add Q values of fractals to each trial
clean_beh_data = clean_beh_data %>%
  group_by(subnum) %>%
  do(get_qvals(., model_name="rpeBoth")) %>%
  ungroup()

if(no_ext){
  clean_beh_data = clean_beh_data %>%
    filter(probFractalDraw != 0 & probFractalDraw !=1)
  
  out_dir = paste0(out_dir, '_noExt')
}

clean_beh_data = clean_beh_data %>%
  mutate(EVRight = referenceProb * referenceValue,
         EVLeft = lotteryValue * lotteryProb,
         lottery_ev_diff = EVLeft - EVRight,
         fractal_qv_diff = leftQValue - rightQValue,
         # distortedEVDiff = (1-theta)*(1-probFractalDraw)*lottery_ev_diff,
         distortedQVDiff = theta*probFractalDraw*fractal_qv_diff) %>%
  rename(QVLeft = leftQValue, QVRight = rightQValue, choice = choiceLeft)

if(grepl("Symm", rl_model)){
  clean_beh_data = clean_beh_data %>%
    mutate(distortedEVDiff = (1-(theta*probFractalDraw))*lottery_ev_diff)
}

if(grepl("Asymm", rl_model)){
  clean_beh_data = clean_beh_data %>%
    mutate(distortedEVDiff = (1-probFractalDraw)*lottery_ev_diff)
}
  
clean_beh_data = clean_beh_data %>%
  select(subnum, EVLeft, EVRight, QVLeft, QVRight, probFractalDraw, choice, reactionTime, distortedEVDiff, distortedQVDiff)

subnums = unique(clean_beh_data$subnum)

dir.create(file.path(out_dir), showWarnings = FALSE)

for(i in 1:length(subnums)){
  cur_sub = subnums[i]
  
  cur_data = clean_beh_data %>%
    filter(subnum == cur_sub)
  
  write.csv(cur_data, paste0(out_dir, '/sub', cur_sub, "_data.csv"), row.names = F)
}
