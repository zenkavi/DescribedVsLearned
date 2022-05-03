library(tidyr)
library(here)

helpers_path = here('analysis/helpers/')
source(paste0(helpers_path,'rlModels/fit_rl_hierarchical_twoParamsSymmNonLinearProbDistortion_rpeBoth.R'))

learner_info = par_ests %>%
  filter(par == "alpha") %>%
  group_by(subnum) %>%
  summarise(alpha = mean(value)) %>%  
  ungroup() %>%
  mutate(slow_learner = ifelse(alpha<median(alpha), 1, 0), 
         fast_learner = ifelse(alpha>=median(alpha), 1, 0)) %>%
  select(-alpha)

write_csv(learner_info, '/Users/zeynepenkavi/Documents/RangelLab/DescribedVsLearned_fmri/analysis/03_level3/learner_info.csv')
