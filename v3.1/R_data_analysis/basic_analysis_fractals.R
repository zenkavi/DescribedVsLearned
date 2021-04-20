rm(list=ls(all=TRUE))
setwd("~/Documents/Caltech/Fall_2016/fmri_bundles/behavioral_analysis/v2.1/R_data_analysis/")

# load data
#########
data = read.csv("~/Documents/Caltech/Fall_2016/fmri_bundles/behavioral_analysis/v2.1/R_data_analysis/bundles_data.csv")
attach(data)

# prelims
#######
subjects <- c(1, 3:8, 11:20, 22:25, 27)
qValDiff = q_left - q_right
Diff = - abs(qValDiff)
choice = choice_left

# individual choice logits when prob fractal >= 70%
estCons = rep(0,length(subjects))
tstatCons = rep(0,length(subjects))
estSlope = rep(0,length(subjects))
tstatSlope = rep(0,length(subjects))

for (s in 1:length(subjects)) {
  model = glm(choice[subject == subjects[s] & prob_fractal_draw >= 0.7] ~ qValDiff[subject == subjects[s] & prob_fractal_draw >= 0.7], family=binomial)
  estCons[s] = model$coefficients[1]
  estSlope[s] = model$coefficients[2]
  tstatCons[s] = coef(summary(model))[1,3]
  tstatSlope[s] = coef(summary(model))[2,3]
}

library(xtable)
df = data.frame(subjects,estCons,tstatCons,estSlope,tstatSlope)
df = rbind(df, c("mean:", mean(estCons), NA, mean(estSlope), NA))
df = rbind(df, c("sd:", sd(estCons), NA, sd(estSlope), NA))
df[2:5] <- lapply(df[2:5], function(x) as.numeric(as.character(x)))
colnames(df) = c("subject","constant", "t-stat","qDiff", "t-stat")
habit_logit_table = xtable(df,digits=c(0,0,2,2,2,2))
print(habit_logit_table, type="latex", include.rownames=FALSE,
      hline.after=c(-1,0,nrow(habit_logit_table),nrow(habit_logit_table)-2))

# basic individual choice logits
estCons = rep(0,length(subjects))
tstatCons = rep(0,length(subjects))
estSlope = rep(0,length(subjects))
tstatSlope = rep(0,length(subjects))

for (s in 1:length(subjects)) {
  model = glm(choice[subject == subjects[s]] ~ qValDiff[subject == subjects[s]], family=binomial)
  estCons[s] = model$coefficients[1]
  estSlope[s] = model$coefficients[2]
  tstatCons[s] = coef(summary(model))[1,3]
  tstatSlope[s] = coef(summary(model))[2,3]
}

library(xtable)
df = data.frame(subjects,estCons,tstatCons,estSlope,tstatSlope)
df = rbind(df, c("mean:", mean(estCons), NA, mean(estSlope), NA))
df = rbind(df, c("sd:", sd(estCons), NA, sd(estSlope), NA))
df[2:5] <- lapply(df[2:5], function(x) as.numeric(as.character(x)))
colnames(df) = c("subject","constant", "t-stat","qDiff", "t-stat")
habit_logit_table = xtable(df,digits=c(0,0,2,2,2,2))
print(habit_logit_table, type="latex", include.rownames=FALSE,
      hline.after=c(-1,0,nrow(habit_logit_table),nrow(habit_logit_table)-2))

# basic individual rt curves
estCons = rep(0,length(subjects))
tstatCons = rep(0,length(subjects))
estSlope = rep(0,length(subjects))
tstatSlope = rep(0,length(subjects))

for (s in 1:length(subjects)) {
  model = lm(rt[subject == subjects[s]] ~ Diff[subject == subjects[s]])
  estCons[s] = model$coefficients[1]
  estSlope[s] = model$coefficients[2]
  tstatCons[s] = coef(summary(model))[1,3]
  tstatSlope[s] = coef(summary(model))[2,3]
}

df = data.frame(subjects,estCons,tstatCons,estSlope,tstatSlope)
df = rbind(df, c("mean:", mean(estCons), NA, mean(estSlope), NA))
df = rbind(df, c("sd:", sd(estCons), NA, sd(estSlope), NA))
df[2:5] <- lapply(df[2:5], function(x) as.numeric(as.character(x)))
colnames(df) = c("subject","constant", "t-stat","difficulty", "t-stat")
habit_rt_table = xtable(df, digits=c(0,0,2,2,2,2))
print(habit_rt_table, type="latex", include.rownames=FALSE,
      hline.after=c(-1,0,nrow(habit_rt_table),nrow(habit_rt_table)-2))

# choice logits grouped by probability of fractal draw
estCons = rep(0,11)
tstatCons = rep(0,11)
estSlope = rep(0,11)
tstatSlope = rep(0,11)

i=1
for (p in c(0,0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9,1)) {
  model = glm(choice[prob_fractal_draw == p] ~ qValDiff[prob_fractal_draw == p], family=binomial)
  estCons[i] = model$coefficients[1]
  estSlope[i] = model$coefficients[2]
  tstatCons[i] = coef(summary(model))[1,3]
  tstatSlope[i] = coef(summary(model))[2,3]
  i = i + 1
}

library(xtable)
df = data.frame(seq(0,1,by=0.1),estCons,tstatCons,estSlope,tstatSlope)
df = rbind(df, c("mean:", mean(estCons), NA, mean(estSlope), NA))
df = rbind(df, c("sd:", sd(estCons), NA, sd(estSlope), NA))
df[2:5] <- lapply(df[2:5], function(x) as.numeric(as.character(x)))
colnames(df) = c("prob_fractal_draw","constant", "t-stat","qDiff", "t-stat")
goal_logit_table = xtable(df)
print(goal_logit_table, type="latex", include.rownames=FALSE,
      hline.after=c(-1,0,nrow(goal_logit_table),nrow(goal_logit_table)-2))

detach(data)