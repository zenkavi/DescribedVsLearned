library(tidyr)
library(here)

helpers_path = here('helpers/')

if(!exists('get_qvals')){
  source(paste0(helpers_path, 'get_qvals.R'))
}

add_inferred_pars = function(clean_beh_data, par_ests){
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
  
  ## Add EVs for lotteries to each trial
  clean_beh_data = clean_beh_data %>%
    mutate(leftLotteryEV = lotteryValue*lotteryProb,
           rightLotteryEV = referenceValue*referenceProb)

  ## Add conflict trial info and value difference
  clean_beh_data = clean_beh_data %>%
    mutate(leftLotteryBetter = leftLotteryEV>rightLotteryEV,
           choseBetterLottery = ifelse(leftLotteryBetter == 1 & choiceLeft == 1, 1, 
                                       ifelse(leftLotteryBetter == 0 & choiceLeft == 0, 1, 0)),
           leftFractalBetter = leftQValue>rightQValue,
           conflictTrial = ifelse(leftFractalBetter != leftLotteryBetter, "conflict", "no conflict")) %>%
    mutate(leftQVAdv = leftQValue - rightQValue,
           leftEVAdv = leftLotteryEV - rightLotteryEV)
  
  ## Add bundle values depending on the model parameters 
  if("delta" %in% names(clean_beh_data)){
    clean_beh_data = clean_beh_data %>%
      mutate(wpFrac = (delta*probFractalDraw^gamma)/(delta*probFractalDraw^gamma + (1-probFractalDraw)^gamma),
             leftBundleVal = (1-wpFrac)*leftLotteryEV + wpFrac*leftQValue,
             rightBundleVal = (1-wpFrac)*rightLotteryEV + wpFrac*rightQValue) 
    
  } else if("w_int" %in% names(clean_beh_data)){
    clean_beh_data = clean_beh_data %>%
      mutate(wpFrac = w_int + w_slope*probFractalDraw,
             leftBundleVal = (1-wpFrac)*leftLotteryEV + wpFrac*leftQValue,
             rightBundleVal = (1-wpFrac)*rightLotteryEV + wpFrac*rightQValue)
  } else{
    clean_beh_data = clean_beh_data %>%
      mutate(leftBundleVal = (1-probFractalDraw)*leftLotteryEV + probFractalDraw*leftQValue,
             rightBundleVal = (1-probFractalDraw)*rightLotteryEV + probFractalDraw*rightQValue)
  }
  
  ## Add value difference for bundles, val chosen and unchosen, trial rpe
  clean_beh_data = clean_beh_data %>%
    mutate(leftbundleValAdv = leftBundleVal - rightBundleVal,
           ppe = fractalDraw - wpFrac,
           valChosen = ifelse(choiceLeft, leftBundleVal, rightBundleVal),
           valUnchosen = ifelse(choiceLeft == 0, leftBundleVal, rightBundleVal),
           valChosenLottery = ifelse(choiceLeft, leftLotteryEV, rightLotteryEV),
           valUnchosenLottery = ifelse(choiceLeft==0, leftLotteryEV, rightLotteryEV),
           valChosenFractal = ifelse(choiceLeft, leftQValue, rightQValue),
           valUnchosenFractal = ifelse(choiceLeft==0, leftQValue, rightQValue),
           junk = 0)
  
  return(clean_beh_data)
}

