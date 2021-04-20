rm(list=ls(all=TRUE))

library(xtable)

# setwd("~/Documents/Caltech/Fall_2016/fmri_bundles/behavioral_analysis/v2.1/R_data_analysis/")
data_path = '/Users/zeynepenkavi/Dropbox/RangelLab/DescribedVsLearned/v3.1/R_data_analysis'
setwd(data_path)

# load data
#########
# data = read.csv("~/Documents/Caltech/Fall_2016/fmri_bundles/behavioral_analysis/v2.1/R_data_analysis/bundles_data.csv")
data = read.csv(paste0(data_path, '/bundles_data.csv'))attach(data)

# prelims
#######
subjects <- c(1, 3:8, 11:20, 22:25, 27)
evLeft = prob_lottery * value_lottery
evDiff = evLeft - 0.5
qValDiff = q_left - q_right
difficultyLottery = - abs(evDiff)
difficultyFractal = - abs(qValDiff)
c = rep(0,length(subject))
c[evDiff * qValDiff < 0] = 1
c1 = rep(0,length(subject))
c1[evDiff>0 & qValDiff<0] = 1
c2 = rep(0,length(subject))
c2[evDiff<0 & qValDiff>0] = 1
prob_lottery_draw = 1 - prob_fractal_draw

choice = choice_left

# individual choice logits with 2 different conflicts
estCons = rep(0,length(subjects))
tstatCons = rep(0,length(subjects))
estEvDiff = rep(0,length(subjects))
tstatEvDiff = rep(0,length(subjects))
estQDiff = rep(0,length(subjects))
tstatQDiff = rep(0,length(subjects))
estConflict1 = rep(0,length(subjects))
tstatConflict1 = rep(0,length(subjects))
estConflict2 = rep(0,length(subjects))
tstatConflict2 = rep(0,length(subjects))

for (s in 1:length(subjects)) {
  name = subjects[s]
  model = glm(choice[subject == name] ~ evDiff[subject == name] +
                qValDiff[subject == name] + c1[subject == name] +
                c2[subject == name], family=binomial)
  estCons[s] = model$coefficients[1]
  estEvDiff[s] = model$coefficients[2]
  estQDiff[s] = model$coefficients[3]
  estConflict1[s] = model$coefficients[4]
  estConflict2[s] = model$coefficients[5]
  tstatCons[s] = coef(summary(model))[1,3]
  tstatEvDiff[s] = coef(summary(model))[2,3]
  tstatQDiff[s] = coef(summary(model))[3,3]
  tstatConflict1[s] = coef(summary(model))[4,3]
  tstatConflict2[s] = coef(summary(model))[5,3]
}

df = data.frame(subjects, estCons,tstatCons,estEvDiff,tstatEvDiff,
                estQDiff,tstatQDiff,estConflict1,tstatConflict1,estConflict2,tstatConflict2)
df = rbind(df, c("mean:", mean(estCons), NA, mean(estEvDiff), NA, mean(estQDiff), NA, mean(estConflict1), NA, mean(estConflict2), NA))
df = rbind(df, c("sd:", sd(estCons), NA, sd(estEvDiff), NA, sd(estQDiff), NA, sd(estConflict1), NA, sd(estConflict2), NA))
df[2:11] <- lapply(df[2:11], function(x) as.numeric(as.character(x)))
colnames(df) = c("subject","constant", "t-stat","evDiff", "t-stat",
                 "qDiff", "t-stat", "c1", "t-stat", "c2", "t-stat")
logit.table = xtable(df,digits=c(0,0,2,2,2,2,2,2,2,2,2,2))
print(logit.table, type="latex", include.rownames=FALSE,
      hline.after=c(-1,0,nrow(logit.table),nrow(logit.table)-2))


# individual choice logits with 2 different conflicts, plus iteractions
estCons = rep(0,length(subjects))
tstatCons = rep(0,length(subjects))
estEvDiff = rep(0,length(subjects))
tstatEvDiff = rep(0,length(subjects))
estC1EvDiff = rep(0,length(subjects))
tstatC1EvDiff = rep(0,length(subjects))
estC2EvDiff = rep(0,length(subjects))
tstatC2EvDiff = rep(0,length(subjects))
estQDiff = rep(0,length(subjects))
tstatQDiff = rep(0,length(subjects))
estC1QDiff = rep(0,length(subjects))
tstatC1QDiff = rep(0,length(subjects))
estC2QDiff = rep(0,length(subjects))
tstatC2QDiff = rep(0,length(subjects))

for (s in 1:length(subjects)) {
  name = subjects[s]
  model = glm(choice[subject == name] ~
                evDiff[subject == name] + (c1[subject == name] * evDiff[subject == name]) + 
                (c2[subject == name] * evDiff[subject == name]) +
                qValDiff[subject == name] + (c1[subject == name] * qValDiff[subject == name]) +
                (c2[subject == name] * qValDiff[subject == name]), family=binomial)
  estCons[s] = model$coefficients[1]
  estEvDiff[s] = model$coefficients[2]
  estC1EvDiff[s] = model$coefficients[3]
  estC2EvDiff[s] = model$coefficients[4]
  estQDiff[s] = model$coefficients[5]
  estC1QDiff[s] = model$coefficients[6]
  estC2QDiff[s] = model$coefficients[7]

  tstatCons[s] = coef(summary(model))[1,3]
  tstatEvDiff[s] = coef(summary(model))[2,3]
  tstatC1EvDiff[s] = coef(summary(model))[3,3]
  tstatC2EvDiff[s] = coef(summary(model))[4,3]
  tstatQDiff[s] = coef(summary(model))[5,3]
  tstatC1QDiff[s] = coef(summary(model))[6,3]
  tstatC2QDiff[s] = coef(summary(model))[7,3]
}

df = data.frame(subjects, estCons,tstatCons,estEvDiff,tstatEvDiff,
                estC1EvDiff,tstatC1EvDiff,estC2EvDiff,tstatC2EvDiff,
                estQDiff,tstatQDiff,estC1QDiff,tstatC1QDiff,estC2QDiff,tstatC2QDiff)
df = rbind(df, c("mean:", mean(estCons), NA, mean(estEvDiff), NA, mean(estC1EvDiff), NA,
                 mean(estC2EvDiff), NA, mean(estQDiff), NA, mean(estC1QDiff), NA, mean(estC2QDiff), NA))
df = rbind(df, c("sd:", sd(estCons), NA, sd(estEvDiff), NA, sd(estC1EvDiff), NA, sd(estC2EvDiff), NA,
                 sd(estQDiff), NA, sd(estC1QDiff), NA, sd(estC2QDiff), NA))
df[2:15] <- lapply(df[2:15], function(x) as.numeric(as.character(x)))
colnames(df) = c("subject","constant", "t-stat","evDiff", "t-stat",
                 "c1*evDiff", "t-stat","c2*evDiff", "t-stat",
                 "qDiff", "t-stat", "c1*qDiff", "t-stat", "c2*qDiff", "t-stat")
logit.table = xtable(df,digits=c(0,0,2,2,2,2,2,2,2,2,2,2,2,2,2,2))
print(logit.table, type="latex", include.rownames=FALSE,
      hline.after=c(-1,0,nrow(logit.table),nrow(logit.table)-2))


# individual choice logits, multiplying value diffs by their weights, with conflict
estCons = rep(0,length(subjects))
tstatCons = rep(0,length(subjects))
estEvDiff = rep(0,length(subjects))
tstatEvDiff = rep(0,length(subjects))
estQDiff = rep(0,length(subjects))
tstatQDiff = rep(0,length(subjects))
estConflict1 = rep(0,length(subjects))
tstatConflict1 = rep(0,length(subjects))
estConflict2 = rep(0,length(subjects))
tstatConflict2 = rep(0,length(subjects))

for (s in 1:length(subjects)) {
  name = subjects[s]
  wEvDiff = prob_lottery_draw[subject == name] * evDiff[subject == name]
  wQDiff = prob_fractal_draw[subject == name] * qValDiff[subject == name]
  model = glm(choice[subject == name] ~
                wEvDiff + wQDiff + c1[subject == name] + c2[subject == name], family=binomial)
  estCons[s] = model$coefficients[1]
  estEvDiff[s] = model$coefficients[2]
  estQDiff[s] = model$coefficients[3]
  estConflict1[s] = model$coefficients[4]
  estConflict2[s] = model$coefficients[5]
  tstatCons[s] = coef(summary(model))[1,3]
  tstatEvDiff[s] = coef(summary(model))[2,3]
  tstatQDiff[s] = coef(summary(model))[3,3]
  tstatConflict1[s] = coef(summary(model))[4,3]
  tstatConflict2[s] = coef(summary(model))[5,3]
}

df = data.frame(subjects,estCons,tstatCons,estEvDiff,tstatEvDiff,
                estQDiff,tstatQDiff,estConflict1,tstatConflict1,estConflict2,tstatConflict2)
df = rbind(df, c("mean:", mean(estCons), NA, mean(estEvDiff), NA, mean(estQDiff), NA, mean(estConflict1), NA, mean(estConflict2), NA))
df = rbind(df, c("sd:", sd(estCons), NA, sd(estEvDiff), NA, sd(estQDiff), NA, sd(estConflict1), NA, sd(estConflict2), NA))
df[2:11] <- lapply(df[2:11], function(x) as.numeric(as.character(x)))
colnames(df) = c("subject","constant", "t-stat","wRelLot", "t-stat",
                 "wRelQ", "t-stat","c1", "t-stat","c2", "t-stat")
logit.table = xtable(df,digits=c(0,0,2,2,2,2,2,2,2,2,2,2))
print(logit.table, type="latex", include.rownames=FALSE,
      hline.after=c(-1,0,nrow(logit.table),nrow(logit.table)-2))


# individual choice logits, multiplying value diffs by their weights, with conflict plus iteractions
estCons = rep(0,length(subjects))
tstatCons = rep(0,length(subjects))
estEvDiff = rep(0,length(subjects))
tstatEvDiff = rep(0,length(subjects))
estC1EvDiff = rep(0,length(subjects))
tstatC1EvDiff = rep(0,length(subjects))
estC2EvDiff = rep(0,length(subjects))
tstatC2EvDiff = rep(0,length(subjects))
estQDiff = rep(0,length(subjects))
tstatQDiff = rep(0,length(subjects))
estC1QDiff = rep(0,length(subjects))
tstatC1QDiff = rep(0,length(subjects))
estC2QDiff = rep(0,length(subjects))
tstatC2QDiff = rep(0,length(subjects))

for (s in 1:length(subjects)) {
  name = subjects[s]
  wEvDiff = prob_lottery_draw[subject == name] * evDiff[subject == name]
  wQDiff = prob_fractal_draw[subject == name] * qValDiff[subject == name]
  
  model = glm(choice[subject == name] ~
                wEvDiff + (c1[subject == name] * wEvDiff) + (c2[subject == name] * wEvDiff) +
                wQDiff + (c1[subject == name] * wQDiff) + (c2[subject == name] * wQDiff), family=binomial)
  estCons[s] = model$coefficients[1]
  estEvDiff[s] = model$coefficients[2]
  estC1EvDiff[s] = model$coefficients[3]
  estC2EvDiff[s] = model$coefficients[4]
  estQDiff[s] = model$coefficients[5]
  estC1QDiff[s] = model$coefficients[6]
  estC2QDiff[s] = model$coefficients[7]
  
  tstatCons[s] = coef(summary(model))[1,3]
  tstatEvDiff[s] = coef(summary(model))[2,3]
  tstatC1EvDiff[s] = coef(summary(model))[3,3]
  tstatC2EvDiff[s] = coef(summary(model))[4,3]
  tstatQDiff[s] = coef(summary(model))[5,3]
  tstatC1QDiff[s] = coef(summary(model))[6,3]
  tstatC2QDiff[s] = coef(summary(model))[7,3]
}

df = data.frame(subjects, estCons,tstatCons,estEvDiff,tstatEvDiff,
                estC1EvDiff,tstatC1EvDiff,estC2EvDiff,tstatC2EvDiff,
                estQDiff,tstatQDiff,estC1QDiff,tstatC1QDiff,estC2QDiff,tstatC2QDiff)
df = rbind(df, c("mean:", mean(estCons), NA, mean(estEvDiff), NA, mean(estC1EvDiff), NA,
                 mean(estC2EvDiff), NA, mean(estQDiff), NA, mean(estC1QDiff), NA, mean(estC2QDiff), NA))
df = rbind(df, c("sd:", sd(estCons), NA, sd(estEvDiff), NA, sd(estC1EvDiff), NA, sd(estC2EvDiff), NA,
                 sd(estQDiff), NA, sd(estC1QDiff), NA, sd(estC2QDiff), NA))
df[2:15] <- lapply(df[2:15], function(x) as.numeric(as.character(x)))
colnames(df) = c("subj","cons", "t-stat","wRelLot", "t-stat",
                 "c1*wRelLot", "t-stat","c2*wRelLot", "t-stat",
                 "wRelQ", "t-stat", "c1*wRelQ", "t-stat", "c2*wRelQ", "t-stat")
logit.table = xtable(df,digits=c(0,0,2,2,2,2,2,2,2,2,2,2,2,2,2,2))
print(logit.table, type="latex", include.rownames=FALSE,
      hline.after=c(-1,0,nrow(logit.table),nrow(logit.table)-2))


# individual choice logits, multiplying value diffs by their weights, with simple conflict plus iteractions
estCons = rep(0,length(subjects))
tstatCons = rep(0,length(subjects))
estEvDiff = rep(0,length(subjects))
tstatEvDiff = rep(0,length(subjects))
estCEvDiff = rep(0,length(subjects))
tstatCEvDiff = rep(0,length(subjects))
estQDiff = rep(0,length(subjects))
tstatQDiff = rep(0,length(subjects))
estCQDiff = rep(0,length(subjects))
tstatCQDiff = rep(0,length(subjects))
estC = rep(0,length(subjects))
tstatC = rep(0,length(subjects))

for (s in 1:length(subjects)) {
  name = subjects[s]
  wEvDiff = prob_lottery_draw[subject == name] * evDiff[subject == name]
  wQDiff = prob_fractal_draw[subject == name] * qValDiff[subject == name]
  
  model = glm(choice[subject == name] ~ wEvDiff + (c[subject == name] * wEvDiff) + wQDiff +
                (c[subject == name] * wQDiff) + c[subject == name], family=binomial)
  estCons[s] = model$coefficients[1]
  estEvDiff[s] = model$coefficients[2]
  estCEvDiff[s] = model$coefficients[3]
  estQDiff[s] = model$coefficients[4]
  estCQDiff[s] = model$coefficients[5]
  estC[s] = model$coefficients[6]
  
  tstatCons[s] = coef(summary(model))[1,3]
  tstatEvDiff[s] = coef(summary(model))[2,3]
  tstatCEvDiff[s] = coef(summary(model))[3,3]
  tstatQDiff[s] = coef(summary(model))[4,3]
  tstatCQDiff[s] = coef(summary(model))[5,3]
  tstatC[s] = coef(summary(model))[6,3]
}

df = data.frame(subjects, estCons,tstatCons,estEvDiff,tstatEvDiff,
                estCEvDiff,tstatCEvDiff,estQDiff,tstatQDiff,estCQDiff,tstatCQDiff,estC,tstatC)
df = rbind(df, c("mean:", mean(estCons), NA, mean(estEvDiff), NA, mean(estCEvDiff), NA,
                 mean(estQDiff), NA, mean(estCQDiff), NA, mean(estC), NA))
df = rbind(df, c("sd:", sd(estCons), NA, sd(estEvDiff), NA, sd(estCEvDiff), NA,
                 sd(estQDiff), NA, sd(estCQDiff), NA, sd(estC), NA))
df[2:13] <- lapply(df[2:13], function(x) as.numeric(as.character(x)))
colnames(df) = c("subj","cons", "t-stat","wRelEV", "t-stat", "cwRelEV", "t-stat",
                 "wRelQ", "t-stat", "cwRelQ", "t-stat", "c", "t-stat")
logit.table = xtable(df,digits=c(0,0,2,2,2,2,2,2,2,2,2,2,2,2))
print(logit.table, type="latex", include.rownames=FALSE,
      hline.after=c(-1,0,nrow(logit.table),nrow(logit.table)-2))


# individual RT, multiplying value diffs by their weights, with simple conflict plus iteractions
estCons = rep(0,length(subjects))
tstatCons = rep(0,length(subjects))
estEvDiff = rep(0,length(subjects))
tstatEvDiff = rep(0,length(subjects))
estCEvDiff = rep(0,length(subjects))
tstatCEvDiff = rep(0,length(subjects))
estQDiff = rep(0,length(subjects))
tstatQDiff = rep(0,length(subjects))
estCQDiff = rep(0,length(subjects))
tstatCQDiff = rep(0,length(subjects))
estC = rep(0,length(subjects))
tstatC = rep(0,length(subjects))

for (s in 1:length(subjects)) {
  name = subjects[s]
  wEvDiff = prob_lottery_draw[subject == name] * evDiff[subject == name]
  wQDiff = prob_fractal_draw[subject == name] * qValDiff[subject == name]
  
  model = lm(rt[subject == name] ~ abs(wEvDiff) + (c[subject == name] * abs(wEvDiff)) + abs(wQDiff) +
               (c[subject == name] * abs(wQDiff)) + c[subject == name])
  estCons[s] = model$coefficients[1]
  estEvDiff[s] = model$coefficients[2]
  estCEvDiff[s] = model$coefficients[3]
  estQDiff[s] = model$coefficients[4]
  estCQDiff[s] = model$coefficients[5]
  estC[s] = model$coefficients[6]
  
  tstatCons[s] = coef(summary(model))[1,3]
  tstatEvDiff[s] = coef(summary(model))[2,3]
  tstatCEvDiff[s] = coef(summary(model))[3,3]
  tstatQDiff[s] = coef(summary(model))[4,3]
  tstatCQDiff[s] = coef(summary(model))[5,3]
  tstatC[s] = coef(summary(model))[6,3]
}

df = data.frame(subjects, estCons,tstatCons,estEvDiff,tstatEvDiff,
                estCEvDiff,tstatCEvDiff,estQDiff,tstatQDiff,estCQDiff,tstatCQDiff,estC,tstatC)
df = rbind(df, c("mean:", mean(estCons), NA, mean(estEvDiff), NA, mean(estCEvDiff), NA,
                 mean(estQDiff), NA, mean(estCQDiff), NA, mean(estC), NA))
df = rbind(df, c("sd:", sd(estCons), NA, sd(estEvDiff), NA, sd(estCEvDiff), NA,
                 sd(estQDiff), NA, sd(estCQDiff), NA, sd(estC), NA))
df[2:13] <- lapply(df[2:13], function(x) as.numeric(as.character(x)))
colnames(df) = c("subj","cons", "t-stat","abs(wRelEV)", "t-stat", "abs(cwRelEV)", "t-stat",
                 "abs(wRelQ)", "t-stat", "abs(cwRelQ)", "t-stat", "c", "t-stat")
logit.table = xtable(df,digits=c(0,0,2,2,2,2,2,2,2,2,2,2,2,2))
print(logit.table, type="latex", include.rownames=FALSE,
      hline.after=c(-1,0,nrow(logit.table),nrow(logit.table)-2))

detach(data)