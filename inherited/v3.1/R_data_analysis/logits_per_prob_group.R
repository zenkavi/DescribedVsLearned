rm(list=ls(all=TRUE))
# setwd("~/Documents/Caltech/Fall_2016/fmri_bundles/behavioral_analysis/v2.1/R_data_analysis/")
data_path = '/Users/zeynepenkavi/Dropbox/RangelLab/DescribedVsLearned/v3.1/R_data_analysis'
setwd(data_path)

# load data
#########
# data = read.csv("~/Documents/Caltech/Fall_2016/fmri_bundles/behavioral_analysis/v2.1/R_data_analysis/bundles_data.csv")
data = read.csv(paste0(data_path, '/bundles_data.csv'))
attach(data)

library("lme4")

# prelims
#######
subjects <- c(1, 3:8, 11:20, 22:25, 27)
evLeft = prob_lottery * value_lottery
evDiff = evLeft - 0.5
qDiff = q_left - q_right
choice = choice_left
prob_lot_draw = 1 - prob_fractal_draw

# choice vs. EV diff and Q diff mixed effects logits grouped by probability of lottery draw
prob_list = list(c(0),c(0.1,0.2,0.3),c(0.4,0.5,0.6),c(0.7,0.8,0.9),c(1))

print("CHOICE GROUPED BY PROB OF LOTTERY DRAW")
i=1
for (probs in prob_list) {
  select_choice = c()
  select_evdiff = c()
  select_qdiff = c()
  select_subj = c()
  for (p in probs) {
    select_choice = append(select_choice, choice[abs(prob_lot_draw - p) < 0.01])
    select_evdiff = append(select_evdiff, evDiff[abs(prob_lot_draw - p) < 0.01])
    select_qdiff = append(select_qdiff, qDiff[abs(prob_lot_draw - p) < 0.01])
    select_subj = append(select_subj, subject[abs(prob_lot_draw - p) < 0.01])
  }
  model = glmer(select_choice ~ 1 + select_evdiff + select_qdiff + (1 + select_evdiff + select_qdiff | select_subj),
                family=binomial(logit),
                control = glmerControl(optimizer = c("bobyqa"), optCtrl=list(maxfun=100000)));
  print(summary(model));
  print("#######################")
  i = i + 1
}

# RT vs. EV diff and Q diff mixed effects model grouped by probability of lottery draw
print("RT GROUPED BY PROB OF LOTTERY DRAW")
i=1
for (probs in prob_list) {
  select_rt = c()
  select_evdiff = c()
  select_qdiff = c()
  select_subj = c()
  for (p in probs) {
    select_rt = append(select_rt, rt[abs(prob_lot_draw - p) < 0.01])
    select_evdiff = append(select_evdiff, evDiff[abs(prob_lot_draw - p) < 0.01])
    select_qdiff = append(select_qdiff, qDiff[abs(prob_lot_draw - p) < 0.01])
    select_subj = append(select_subj, subject[abs(prob_lot_draw - p) < 0.01])
  }
  model = lmer(select_rt ~ 1 + abs(select_evdiff) + abs(select_qdiff) + (1 + abs(select_evdiff) + abs(select_qdiff) | select_subj));
  print(summary(model));
  print("#######################")
  i = i + 1
}

detach(data)