set.seed(2394579)

library(here)
library(tidyverse)

helpers_path = here('analysis/helpers/')

source(paste0(helpers_path, 'rlModels/sim_choice_data.R'))

sample_from_posterior = function(parEsts, subId){
  
  subParEsts = parEsts %>% 
    filter(subnum == subId) %>%
    spread(par, value)
  
  if("gamma" %in% names(subParEsts)){
    sampled_rows = sample(1:nrow(subParEsts), 4)
    
    sampled_pars = data.frame(alpha = subParEsts$alpha[sampled_rows[1]], 
                              beta = subParEsts$beta[sampled_rows[2]], 
                              gamma = subParEsts$gamma[sampled_rows[3]], 
                              delta = subParEsts$delta[sampled_rows[4]])
  } else {
    sampled_rows = sample(1:nrow(subParEsts), 3)
    
    sampled_pars = data.frame(alpha = subParEsts$alpha[sampled_rows[1]], 
                              beta = subParEsts$beta[sampled_rows[2]], 
                              delta = subParEsts$delta[sampled_rows[3]])
  }
  
  
  return(sampled_pars)
}

make_posterior_predictive_data = function(numDraws,
                                          modelName){
  
  if(!exists('par_ests')){
    source(paste0(helpers_path, modelName,'.R'))
  }
  
  if(!exists('clean_beh_data')){
    source(paste0(helpers_path,'01_clean_behavioral_data.R'))
  }
  
  all_subs = unique(clean_beh_data$subnum)
  
  for(i in 1:length(all_subs)){
    cur_sub = all_subs[i]
    subj_trials = clean_beh_data %>% 
      filter(subnum == cur_sub) %>% 
      rename(rightLotteryValue=referenceValue, rightLotteryProb=referenceProb, 
             leftLotteryValue=lotteryValue, leftLotteryProb=lotteryProb)
    print(paste0("Simulating data for ", cur_sub, "..."))
    for(j in 1:numDraws){
      sampled_pars = sample_from_posterior(par_ests, cur_sub)
      
      if(i == 1 & j == 1){
        pp_data = sim_choice_data(subj_trials, sampled_pars)
        pp_data = pp_data %>% mutate(subnum=cur_sub, sampleNum = j)
        pp_data = cbind(pp_data, sampled_pars)
      } else{
        cur_data = sim_choice_data(subj_trials, sampled_pars)
        cur_data = cur_data %>% mutate(subnum=cur_sub, sampleNum = j)
        cur_data = cbind(cur_data, sampled_pars)
        pp_data = rbind(pp_data, cur_data)
      }
    }
  }
  
  saveRDS(pp_data, paste0(helpers_path, 'rlModels/pp_data_', modelName, '.RDS'))
  
  return(pp_data)
}