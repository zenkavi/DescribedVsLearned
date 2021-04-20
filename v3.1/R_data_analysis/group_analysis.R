rm(list=ls(all=TRUE))
library("lme4")
library(xtable)

# setwd("~/Documents/Caltech/Fall_2016/fmri_bundles/behavioral_analysis/v2.1/R_data_analysis/")
data_path = '/Users/zeynepenkavi/Dropbox/RangelLab/DescribedVsLearned/v3.1/R_data_analysis'
setwd(data_path)

# load data
#########
# data = read.csv("~/Documents/Caltech/Fall_2016/fmri_bundles/behavioral_analysis/v2.1/R_data_analysis/bundles_data.csv")
data = read.csv(paste0(data_path, '/bundles_data.csv'))
attach(data)

# defined variables
wRelLot = (1 - prob_fractal_draw) * (prob_lottery * value_lottery - 0.5)
wRelQ   = prob_fractal_draw * (q_left - q_right)
relLot = (prob_lottery * value_lottery - 0.5)
relQ   = (q_left - q_right)

conflict = rep(0,length(relQ))
conflict[relLot>0 & relQ<0]=1
conflict[relLot<0 & relQ>0]=1
agree = 1 - conflict

cwRelLot = conflict * wRelLot
cwRelQ   = conflict * wRelQ

cRelLot = conflict * relLot
cRelQ   = conflict * relQ

aRelLot   = agree * relLot
aRelQ     = agree * relQ

awRelLot   = agree * wRelLot
awRelQ     = agree * wRelQ

conflictSize = abs(cwRelLot) + abs(cwRelQ)

prob_lot_draw = 1 - prob_fractal_draw

# 
# # aggregate choice analyses
# model = glmer(choice_left ~ 1 + awRelLot + cwRelLot + awRelQ + cwRelQ +
#                 (1 + awRelLot + cwRelLot + awRelQ + cwRelQ | subject),
#               family=binomial(logit),
#               control = glmerControl(optimizer = c("bobyqa"), optCtrl=list(maxfun=100000)));
# print(summary(model));
# print("#######################")
# 
# 
# model = glmer(choice_left ~ 1 + awRelLot + cwRelLot + awRelQ + cwRelQ + (conflictSize * cwRelLot) + (conflictSize * cwRelQ) +
#                 (1 + awRelLot + cwRelLot + awRelQ + cwRelQ + (conflictSize * cwRelLot) + (conflictSize * cwRelQ) | subject),
#               family=binomial(logit),
#               control = glmerControl(optimizer = c("bobyqa"), optCtrl=list(maxfun=100000)));
# print(summary(model));
# print("#######################")
# 
# model = glmer(choice_left[prob_lot_draw<0.5] ~ 1 + awRelLot[prob_lot_draw<0.5] +
#                 cwRelLot[prob_lot_draw<0.5] + awRelQ[prob_lot_draw<0.5] +
#                 cwRelQ[prob_lot_draw<0.5] +
#                 (1 + awRelLot[prob_lot_draw<0.5] + cwRelLot[prob_lot_draw<0.5] +
#                    awRelQ[prob_lot_draw<0.5] + cwRelQ[prob_lot_draw<0.5] | subject[prob_lot_draw<0.5]),
#               family=binomial(logit),
#               control = glmerControl(optimizer = c("bobyqa"), optCtrl=list(maxfun=100000)));
# print(summary(model));
# print("#######################")
# 
# model = glmer(choice_left[prob_lot_draw>0.5] ~ 1 + awRelLot[prob_lot_draw>0.5] +
#                 cwRelLot[prob_lot_draw>0.5] + awRelQ[prob_lot_draw>0.5] +
#                 cwRelQ[prob_lot_draw>0.5] +
#                 (1 + awRelLot[prob_lot_draw>0.5] + cwRelLot[prob_lot_draw>0.5] +
#                  awRelQ[prob_lot_draw>0.5] + cwRelQ[prob_lot_draw>0.5] | subject[prob_lot_draw>0.5]),
#               family=binomial(logit),
#               control = glmerControl(optimizer = c("bobyqa"), optCtrl=list(maxfun=100000)));
# print(summary(model));
# print("#######################")
# 
# model = glmer(choice_left[prob_lot_draw<=0.3] ~ 1 + awRelLot[prob_lot_draw<=0.3] +
#                 cwRelLot[prob_lot_draw<=0.3] + awRelQ[prob_lot_draw<=0.3] +
#                 cwRelQ[prob_lot_draw<=0.3] +
#                 (1 + awRelLot[prob_lot_draw<=0.3] + cwRelLot[prob_lot_draw<=0.3] +
#                    awRelQ[prob_lot_draw<=0.3] + cwRelQ[prob_lot_draw<=0.3] | subject[prob_lot_draw<=0.3]),
#               family=binomial(logit),
#               control = glmerControl(optimizer = c("bobyqa"), optCtrl=list(maxfun=100000)));
# print(summary(model));
# print("#######################")
# 
# model = glmer(choice_left[prob_lot_draw>0.3 & prob_lot_draw<=0.7] ~ 1 + awRelLot[prob_lot_draw>0.3 & prob_lot_draw<=0.7] +
#                 cwRelLot[prob_lot_draw>0.3 & prob_lot_draw<=0.7] + awRelQ[prob_lot_draw>0.3 & prob_lot_draw<=0.7] +
#                 cwRelQ[prob_lot_draw>0.3 & prob_lot_draw<=0.7] +
#                 (1 + awRelLot[prob_lot_draw>0.3 & prob_lot_draw<=0.7] + cwRelLot[prob_lot_draw>0.3 & prob_lot_draw<=0.7] +
#                    awRelQ[prob_lot_draw>0.3 & prob_lot_draw<=0.7] + cwRelQ[prob_lot_draw>0.3 & prob_lot_draw<=0.7] | subject[prob_lot_draw>0.3 & prob_lot_draw<=0.7]),
#               family=binomial(logit),
#               control = glmerControl(optimizer = c("bobyqa"), optCtrl=list(maxfun=100000)));
# print(summary(model));
# print("#######################")
# 
# model = glmer(choice_left[prob_lot_draw>0.7] ~ 1 + awRelLot[prob_lot_draw>0.7] +
#                 cwRelLot[prob_lot_draw>0.7] + awRelQ[prob_lot_draw>0.7] +
#                 cwRelQ[prob_lot_draw>0.7] +
#                 (1 + awRelLot[prob_lot_draw>0.7] + cwRelLot[prob_lot_draw>0.7] +
#                    awRelQ[prob_lot_draw>0.7] + cwRelQ[prob_lot_draw>0.7] | subject[prob_lot_draw>0.7]),
#               family=binomial(logit),
#               control = glmerControl(optimizer = c("bobyqa"), optCtrl=list(maxfun=100000)));
# print(summary(model));
# print("#######################")


# aggregate RT analyses
model = lmer(rt ~ 1 + abs(awRelLot) + abs(cwRelLot) + abs(awRelQ) + abs(cwRelQ) +
                (1 + abs(awRelLot) + abs(cwRelLot) + abs(awRelQ) + abs(cwRelQ) | subject));
print(summary(model));
print("#######################")

model = lmer(choice_left ~ 1 + abs(awRelLot) + abs(cwRelLot) + abs(awRelQ) + abs(cwRelQ) +
               (conflictSize * abs(cwRelLot)) + (conflictSize * abs(cwRelQ)) +
               (1 + abs(awRelLot) + abs(cwRelLot) + abs(awRelQ) + abs(cwRelQ) +
                  (conflictSize * abs(cwRelLot)) + (conflictSize * abs(cwRelQ)) | subject));
print(summary(model));
print("#######################")

model = lmer(choice_left[prob_lot_draw<0.5] ~ 1 + abs(awRelLot[prob_lot_draw<0.5]) +
               abs(cwRelLot[prob_lot_draw<0.5]) + abs(awRelQ[prob_lot_draw<0.5]) +
               abs(cwRelQ[prob_lot_draw<0.5]) +
                (1 + abs(awRelLot[prob_lot_draw<0.5]) + abs(cwRelLot[prob_lot_draw<0.5]) +
                   abs(awRelQ[prob_lot_draw<0.5]) + abs(cwRelQ[prob_lot_draw<0.5]) | subject[prob_lot_draw<0.5]));
print(summary(model));
print("#######################")

model = lmer(choice_left[prob_lot_draw>0.5] ~ 1 + abs(awRelLot[prob_lot_draw>0.5]) +
               abs(cwRelLot[prob_lot_draw>0.5]) + abs(awRelQ[prob_lot_draw>0.5]) +
               abs(cwRelQ[prob_lot_draw>0.5]) +
                (1 + abs(awRelLot[prob_lot_draw>0.5]) + abs(cwRelLot[prob_lot_draw>0.5]) +
                   abs(awRelQ[prob_lot_draw>0.5]) + abs(cwRelQ[prob_lot_draw>0.5]) | subject[prob_lot_draw>0.5]));
print(summary(model));
print("#######################")

model = lmer(choice_left[prob_lot_draw<=0.3] ~ 1 + abs(awRelLot[prob_lot_draw<=0.3]) +
               abs(cwRelLot[prob_lot_draw<=0.3]) + abs(awRelQ[prob_lot_draw<=0.3]) +
               abs(cwRelQ[prob_lot_draw<=0.3]) +
                (1 + abs(awRelLot[prob_lot_draw<=0.3]) + abs(cwRelLot[prob_lot_draw<=0.3]) +
                   abs(awRelQ[prob_lot_draw<=0.3]) + abs(cwRelQ[prob_lot_draw<=0.3]) | subject[prob_lot_draw<=0.3]));
print(summary(model));
print("#######################")

model = lmer(choice_left[prob_lot_draw>0.3 & prob_lot_draw<=0.7] ~ 1 + abs(awRelLot[prob_lot_draw>0.3 & prob_lot_draw<=0.7]) +
               abs(cwRelLot[prob_lot_draw>0.3 & prob_lot_draw<=0.7]) + abs(awRelQ[prob_lot_draw>0.3 & prob_lot_draw<=0.7]) +
               abs(cwRelQ[prob_lot_draw>0.3 & prob_lot_draw<=0.7]) +
                (1 + abs(awRelLot[prob_lot_draw>0.3 & prob_lot_draw<=0.7]) + abs(cwRelLot[prob_lot_draw>0.3 & prob_lot_draw<=0.7]) +
                   abs(awRelQ[prob_lot_draw>0.3 & prob_lot_draw<=0.7]) + abs(cwRelQ[prob_lot_draw>0.3 & prob_lot_draw<=0.7]) |
                   subject[prob_lot_draw>0.3 & prob_lot_draw<=0.7]));
print(summary(model));
print("#######################")

model = lmer(choice_left[prob_lot_draw>0.7] ~ 1 + abs(awRelLot[prob_lot_draw>0.7]) +
               abs(cwRelLot[prob_lot_draw>0.7]) + abs(awRelQ[prob_lot_draw>0.7]) +
               abs(cwRelQ[prob_lot_draw>0.7]) +
                (1 + abs(awRelLot[prob_lot_draw>0.7]) + abs(cwRelLot[prob_lot_draw>0.7]) +
                   abs(awRelQ[prob_lot_draw>0.7]) + abs(cwRelQ[prob_lot_draw>0.7]) | subject[prob_lot_draw>0.7]));
print(summary(model));
print("#######################")

# ease = abs(wRelLot) + abs(wRelQ)
# model = lm( rt ~ ease)
# print(xtable(model), type="latex")
# 
# model = lm( rt[conflict==0] ~ ease[conflict==0])
# print(xtable(model), type="latex")
# 
# model = lm( rt[conflict==1] ~ ease[conflict==1])
# print(xtable(model), type="latex")

detach(data)

