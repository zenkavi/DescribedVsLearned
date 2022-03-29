library(tidyr)
library(here)

helpers_path = here('analysis/helpers/')

if(!exists('get_qvals')){
  source(paste0(helpers_path, 'get_qvals.R'))
}

add_inferred_pars = function(clean_beh_data, par_ests, model_name="original"){
  
  # Add mean posterior estimates to clean_beh_data
  clean_beh_data = par_ests %>%
    group_by(subnum, par) %>%
    summarise(est = mean(value), .groups='keep') %>%
    spread(par, est) %>%
    left_join(clean_beh_data, by='subnum')
  
  ## Add Q values of fractals to each trial
  clean_beh_data = clean_beh_data %>%
    group_by(subnum) %>%
    do(get_qvals(., model_name=model_name)) %>%
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
    if("gamma" %in% names(clean_beh_data)){
      clean_beh_data = clean_beh_data %>%
        mutate(wpFrac = (delta*probFractalDraw^gamma)/(delta*probFractalDraw^gamma + (1-probFractalDraw)^gamma),
               valLeftBundle = (1-wpFrac)*leftLotteryEV + wpFrac*leftQValue,
               valRightBundle = (1-wpFrac)*rightLotteryEV + wpFrac*rightQValue)
    } else{
      clean_beh_data = clean_beh_data %>%
        mutate(wpFrac = (delta*probFractalDraw)/(delta*probFractalDraw + (1-probFractalDraw)),
               valLeftBundle = (1-wpFrac)*leftLotteryEV + wpFrac*leftQValue,
               valRightBundle = (1-wpFrac)*rightLotteryEV + wpFrac*rightQValue)
    }
    
  } else if("w_int" %in% names(clean_beh_data)){
    clean_beh_data = clean_beh_data %>%
      mutate(wpFrac = w_int + w_slope*probFractalDraw,
             valLeftBundle = (1-wpFrac)*leftLotteryEV + wpFrac*leftQValue,
             valRightBundle = (1-wpFrac)*rightLotteryEV + wpFrac*rightQValue)
  } else{
    clean_beh_data = clean_beh_data %>%
      mutate(valLeftBundle = (1-probFractalDraw)*leftLotteryEV + probFractalDraw*leftQValue,
             valRightBundle = (1-probFractalDraw)*rightLotteryEV + probFractalDraw*rightQValue)
  }
  
  ## Add value difference for bundles, val chosen and unchosen, trial rpe
  clean_beh_data = clean_beh_data %>%
    mutate(leftBundleValAdv = valLeftBundle - valRightBundle,
           valChosen = ifelse(choiceLeft, valLeftBundle, valRightBundle),
           valUnchosen = ifelse(choiceLeft == 0, valLeftBundle, valRightBundle),
           valChosenLottery = ifelse(choiceLeft, leftLotteryEV, rightLotteryEV),
           valUnchosenLottery = ifelse(choiceLeft==0, leftLotteryEV, rightLotteryEV),
           valChosenFractal = ifelse(choiceLeft, leftQValue, rightQValue),
           valUnchosenFractal = ifelse(choiceLeft==0, leftQValue, rightQValue),
           valBundleSum = valLeftBundle + valRightBundle,
           valChosenMinusUnchosen = valChosen - valUnchosen,
           rpeLeftRightSum = leftFractalRpe + rightFractalRpe,
           junk = 0)
  
  return(clean_beh_data)
}

