set.seed(2394239)
library(here)
library(tidyverse)
helpers_path = here('helpers/')

source(paste0(helpers_path, 'sim_logLiks.R'))

alphas = seq(0, 1, .1)
betas = seq(0, 5, .5)
deltas = seq(0, 1.6, .2)
gammas = seq(0, 1.6, .2)

numSims = 50

sim_out = data.frame(alpha=NA, beta=NA, delta=NA, gamma=NA, logLik=NA)
x = length(betas)*length(deltas)*length(gammas)
y = (length(alphas)*length(betas)*length(deltas)*length(gammas)

for(i in 1:length(alphas)){
  done_pct = ((i-1)*x)/(y)
  print(paste0())
  for(j in 1:length(betas)){
    for(k in 1:length(deltas)){
      for(l in 1:length(gammas)){
        cur_pars = data.frame(alpha=alphas[i], beta=betas[j], delta=deltas[k], gamma=gammas[l])
        logLiks = sim_logLiks(numSims, cur_pars)
        cur_out = data.frame(alpha=alphas[i], beta = betas[j], delta=deltas[k], gamma=gammas[l], logLik = logLiks)
        sim_out = rbind(sim_out, cur_out)
      }
    }
  }
}

sim_out = sim_out %>%
  drop_na()