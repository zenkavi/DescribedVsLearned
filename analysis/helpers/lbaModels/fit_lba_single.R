library(tidyr)
library(rstan)
library(here)

options(mc.cores = 4)
helpers_path = here('analysis/helpers/')

## Read in data if not already there
if (!exists('clean_beh_data')){
  source(paste0(helpers_path,'01_clean_behavioral_data.R'))
}


## If there is a fit object read it in
if(file.exists(paste0(helpers_path, 'lbaModels/stanModels/lba_single.RDS'))){
  fit = readRDS(paste0(helpers_path, 'lbaModels/stanModels/lba_single.RDS'))
} else {## Otherwise fit the model
  
  ## Reshape data
  
  rt_choice = clean_beh_data %>%
    filter(subnum == "01") %>%
    select(reactionTime, choiceLeft) %>%
    mutate(choiceLeft = ifelse(choiceLeft == 0, 2, choiceLeft))
  
  rt_choice = as.matrix(rt_choice)
  
  m_data=list(LENGTH = nrow(rt_choice),
              RT = rt_choice,
              NUM_CHOICES = 2)
  
  rm(rt_choice)
  
  ## Fit model for all subjects
  m = stan_model(paste0(helpers_path,'lbaModels/stanModels/lba_single.stan'))
  
  fit = sampling(m, data=m_data)
  saveRDS(fit, paste0(helpers_path, 'lbaModels/stanModels/lba_single.RDS'))
}

## Organize output

### Plot posteriors
data.frame(extract(fit, c("k", "A", "tau", "v"))) %>%
  gather(key, value) %>%
  ggplot(aes(value))+
  geom_histogram(bins=30)+
  facet_wrap(~key, scales="free")
