
library(tidyverse)
library(here)
helpers_path = here('analysis/helpers/')

source(paste0(helpers_path,'01_clean_behavioral_data.R'))
source(paste0(helpers_path, 'get_qvals.R'))

source(paste0(helpers_path, 'rlModels/fit_rl_hierarchical_oneParamDoubleSymmLinearProbDistortion_rpeBoth.R'))

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

clean_beh_data = clean_beh_data %>%
  mutate(EVRight = referenceProb * referenceValue,
         EVLeft = lotteryValue * lotteryProb,
         lottery_ev_diff = EVLeft - EVRight,
         fractal_qv_diff = leftQValue - rightQValue,
         distortedEVDiff = (1-theta)*(1-probFractalDraw)*lottery_ev_diff,
         distortedQVDiff = theta*probFractalDraw*fractal_qv_diff) %>%
  rename(QVLeft = leftQValue, QVRight = rightQValue, choice = choiceLeft) %>%
  select(subnum, EVLeft, EVRight, QVLeft, QVRight, probFractalDraw, choice, reactionTime, distortedEVDiff, distortedQVDiff)

subnums = unique(clean_beh_data$subnum)

for(i in 1:length(subnums)){
  cur_sub = subnums[i]
  
  cur_data = clean_beh_data %>%
    filter(subnum == cur_sub)
  
  write.csv(cur_data, paste0(helpers_path, 'ddModels/cluster_scripts/sub_data_distV/sub', cur_sub, "_data.csv"), row.names = F)
}
