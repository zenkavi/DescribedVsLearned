library(tidyverse)
library(here)
helpers_path = paste0(here(), '/analysis/helpers/')
source(paste0(helpers_path,'rlModels/fit_rl_hierarchical.R'))
source(paste0(helpers_path,'add_inferred_pars.R'))

clean_beh_data = add_inferred_pars(clean_beh_data, par_ests, model_name="original")

subnums = unique(clean_beh_data$subnum)

#Cols you need for ddm_Roptim > fit_task
# "EVLeft","EVRight","QVLeft","QVRight","probFractalDraw","choice","reactionTime",

ddm_Roptim_data = clean_beh_data %>%
  select(subnum, leftLotteryEV, rightLotteryEV, leftQValue, rightQValue, probFractalDraw, choiceLeft, reactionTime) %>%
  rename(EVLeft = leftLotteryEV, EVRight = rightLotteryEV, QVLeft = leftQValue, QVRight = rightQValue) %>%
  mutate(choice = ifelse(choiceLeft == 1, "left", "right"))

out_path = paste0(helpers_path, 'ddModels/cluster_scripts/sub_data/')

for (i in 1:length(subnums)){
  cur_sub = subnums[i]
  cur_fn = paste0('sub_data', cur_sub, '.csv')
  cur_sub_data = ddm_Roptim_data %>%
    filter(subnum == cur_sub)
  write.csv(cur_sub_data, paste0(out_path, cur_fn), row.names = FALSE)
  
}

# Cols you need for ddrl_Roptim > fit_task_sequential
# "EVLeft","EVRight","leftFractalReward","rightFractalReward","probFractalDraw","choice","reactionTime",

ddrl_Roptim_data = clean_beh_data %>%
  select(subnum, leftLotteryEV, rightLotteryEV, leftFractalReward, rightFractalReward, probFractalDraw, choiceLeft, reactionTime) %>%
  rename(EVLeft = leftLotteryEV, EVRight = rightLotteryEV) %>%
  mutate(choice = ifelse(choiceLeft == 1, "left", "right"))

out_path = paste0(helpers_path, 'ddrlModels/cluster_scripts/sub_data/')

for (i in 1:length(subnums)){
  cur_sub = subnums[i]
  cur_fn = paste0('sub_data', cur_sub, '.csv')
  cur_sub_data = ddrl_Roptim_data %>%
    filter(subnum == cur_sub)
  write.csv(cur_sub_data, paste0(out_path, cur_fn), row.names = FALSE)
  
}
