library(tidyverse)
library(here)

helpers_path = here('analysis/helpers/')

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

source(paste0(helpers_path,'01_clean_behavioral_data.R'))
source(paste0(helpers_path, 'get_qvals.R'))
source(paste0(helpers_path, 'rlModels/fit_rl_hierarchical_oneParamAsymmLinearProbDistortion_rpeBoth.R'))

clean_beh_data_asymmnonorm = par_ests %>%
  group_by(subnum, par) %>%
  summarise(est = mean(value), .groups='keep') %>%
  spread(par, est) %>%
  left_join(clean_beh_data, by='subnum')

## Add Q values of fractals to each trial
clean_beh_data_asymmnonorm = clean_beh_data_asymmnonorm %>%
  group_by(subnum) %>%
  do(get_qvals(., model_name="rpeBoth")) %>%
  ungroup()

rm(fit, g_par_ests, par_ests)

clean_beh_data_asymmnonorm = clean_beh_data_asymmnonorm %>%
  mutate(rightLotteryEV = referenceProb * referenceValue,
         leftLotteryEV = lotteryValue * lotteryProb,
         wpFrac = theta*probFractalDraw, 
         left_tv_pFrac = fractalLeftProb * probFractalDraw,
         right_tv_pFrac = fractalRightProb * probFractalDraw,
         left_tv_wpFrac = fractalLeftProb * wpFrac,
         right_tv_wpFrac = fractalRightProb * wpFrac,
         left_qv_pFrac = leftQValue * probFractalDraw,
         right_qv_pFrac = rightQValue * probFractalDraw,
         left_qv_wpFrac = leftQValue * wpFrac,
         right_qv_wpFrac = rightQValue * wpFrac,
         left_ev_pLott = leftLotteryEV * (1-probFractalDraw),
         right_ev_pLott = rightLotteryEV * (1-probFractalDraw)) %>%
  mutate(valSumTvpFrac = left_ev_pLott + right_ev_pLott + left_tv_pFrac + right_tv_pFrac,
         valSumTvwpFrac = left_ev_pLott + right_ev_pLott + left_tv_wpFrac + right_tv_wpFrac,
         valSumQvpFrac = left_ev_pLott + right_ev_pLott + left_qv_pFrac + right_qv_pFrac,
         valSumQvwpFrac = left_ev_pLott + right_ev_pLott + left_qv_wpFrac + right_qv_wpFrac)

# Read in data with excluded trials
source(paste0(helpers_path,'00_get_behavioral_data.R'))

junk_trials = beh_data %>%
  mutate(junk = ifelse(responded != 1 | reactionTime <= .2, 1, 0)) %>%
  filter(junk == 1)

# Add junk trials to clean trials and order by subject, run, trial number
all_trials = rbind.all.columns(clean_beh_data_asymmnonorm, junk_trials) %>%
  arrange(subnum, session, trialNum)

# Write out output
write_csv(all_trials, '~/Downloads/GTavares_2017_arbitration/behavioral_data/all_trials_valSumComp.csv')
