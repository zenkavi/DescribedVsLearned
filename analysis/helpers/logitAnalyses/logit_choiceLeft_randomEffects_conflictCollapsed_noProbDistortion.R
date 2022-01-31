library(tidyverse)
library(here)
library(brms)
helpers_path = here('analysis/helpers/')

if (!exists('clean_beh_data')){
  source(paste0(helpers_path,'fit_twoValSystemsWithRL_hierarchical_noProbDistortion.R'))
  source(paste0(helpers_path,'add_inferred_pars.R'))
  clean_beh_data = add_inferred_pars(clean_beh_data, par_ests_noProbDistortion)
}


if(file.exists(paste0(helpers_path, 'logitOut_choiceLeft_randomEffects_conflictCollapsed_noProbDistortion.RDS'))){
  out_choiceLeft_re_noProbDistortion = readRDS(paste0(helpers_path, 'logitOut_choiceLeft_randomEffects_conflictCollapsed_noProbDistortion.RDS'))
} else {
  
  probFractalDrawVals = unique(clean_beh_data$probFractalDraw)
  out_choiceLeft_re_noProbDistortion = data.frame(Estimate=NA, Est.Error=NA, Q2.5=NA, Q97.5=NA, subnum=NA, var=NA, probFractalDraw=NA)
  
  for(i in 1:length(probFractalDrawVals)){
    curProbFractalDraw = probFractalDrawVals[i]
    
    dat = clean_beh_data %>%
      filter(probFractalDraw == curProbFractalDraw)
    
    m = brm(choiceLeft ~ (1 + leftQVAdv + leftEVAdv |subnum),
            data=dat, family=bernoulli(link="logit"), silent=2, refresh=0)
    
    intercepts = data.frame(coef(m)$subnum[,,1]) %>%
      mutate(subnum = row.names(.),
             var = "intercept",
             probFractalDraw = curProbFractalDraw)
    
    qv_slopes = data.frame(coef(m)$subnum[,,2]) %>%
      mutate(subnum = row.names(.),
             var = "qv_slopes",
             probFractalDraw = curProbFractalDraw)
    
    ev_slopes = data.frame(coef(m)$subnum[,,3]) %>%
      mutate(subnum = row.names(.),
             var = "ev_slopes",
             probFractalDraw = curProbFractalDraw)
    
    out_choiceLeft_re_noProbDistortion = rbind(out_choiceLeft_re_noProbDistortion, intercepts, qv_slopes, ev_slopes)
  }
  
  out_choiceLeft_re_noProbDistortion = out_choiceLeft_re_noProbDistortion %>%
    drop_na()
  
  saveRDS(out_choiceLeft_re_noProbDistortion, paste0(helpers_path, 'logitOut_choiceLeft_randomEffects_conflictCollapsed_noProbDistortion.RDS'))
  rm(dat, m, probFractalDrawVals, curProbFractalDraw)
}
