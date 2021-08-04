library(tidyverse)
library(here)

helpers_path = here('helpers/')
source(paste0(helpers_path,'fit_twoValSystemsWithRL_hierarchical.R'))

## Save posterior mean estimates to `clean_beh_data`
clean_beh_data = par_ests %>%
  group_by(subnum, par) %>%
  summarise(est = mean(value), .groups='keep') %>%
  spread(par, est) %>%
  left_join(clean_beh_data, by='subnum')

## Add Q values of fractals to each trial
clean_beh_data = clean_beh_data %>%
  group_by(subnum) %>%
  do(get_qvals(.)) %>%
  ungroup()

## Add EVs for lotteries, conflict trial info and value difference
clean_beh_data = clean_beh_data %>%
  mutate(leftLotteryEV = lotteryValue*lotteryProb,
         rightLotteryEV = referenceValue*referenceProb,
         leftLotteryBetter = leftLotteryEV>rightLotteryEV,
         choseBetterLottery = ifelse(leftLotteryBetter == 1 & choiceLeft == 1, 1, 
                                     ifelse(leftLotteryBetter == 0 & choiceLeft == 0, 1, 0)),
         leftFractalBetter = leftQValue>rightQValue,
         conflictTrial = ifelse(leftFractalBetter != leftLotteryBetter, "conflict", "no conflict")) %>%
  mutate(leftQVAdv = leftQValue - rightQValue,
         leftEVAdv = leftLotteryEV - rightLotteryEV,
         wpFrac = (delta*probFractalDraw^gamma)/(delta*probFractalDraw^gamma + (1-probFractalDraw)^gamma),
         leftBundleVal = (1-wpFrac)*leftLotteryEV + wpFrac*leftQValue,
         rightBundleVal = (1-wpFrac)*rightLotteryEV + wpFrac*rightQValue,
         leftbundleValAdv = leftBundleVal - rightBundleVal) %>%
  mutate(rpe = ifelse(choiceLeft, reward - leftBundleVal, reward - rightBundleVal),
         junk = 0)

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
