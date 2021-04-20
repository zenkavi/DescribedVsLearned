rm(list=ls(all=TRUE))

library(xtable)

# setwd("~/Documents/Caltech/Fall_2016/fmri_bundles/behavioral_analysis/v2.1/R_data_analysis/")
data_path = '/Users/zeynepenkavi/Dropbox/RangelLab/DescribedVsLearned/v3.1/R_data_analysis'
setwd(data_path)

# load data
#########
# data = read.csv("~/Documents/Caltech/Fall_2016/fmri_bundles/behavioral_analysis/v2.1/R_data_analysis/bundles_data.csv")
data = read.csv(paste0(data_path, '/bundles_data.csv'))
attach(data)

# prelims
#######
subjects <- c(1, 3:8, 11:20, 22:25, 27)
evLeft = prob_lottery * value_lottery
evDiff = evLeft - 0.5
qValDiff = q_left - q_right
prob_lottery_draw = 1 - prob_fractal_draw
wRelLot = prob_lottery_draw * evDiff
wRelQ   = prob_fractal_draw * qValDiff
choice = choice_left
totWValue = prob_lottery_draw * evDiff + prob_fractal_draw * qValDiff
difficulty = - abs(totWValue)

# choice vs. total weighted value difference logit
estCons = rep(0,length(subjects))
tstatCons = rep(0,length(subjects))
estSlope = rep(0,length(subjects))
tstatSlope = rep(0,length(subjects))

for (s in 1:length(subjects)) {
  model = glm(choice[subject == subjects[s]] ~ totWValue[subject == subjects[s]], family=binomial)
  estCons[s] = model$coefficients[1]
  estSlope[s] = model$coefficients[2]
  tstatCons[s] = coef(summary(model))[1,3]
  tstatSlope[s] = coef(summary(model))[2,3]
}
df = data.frame(subjects,estCons,tstatCons,estSlope,tstatSlope)
df = rbind(df, c("mean:", mean(estCons), NA, mean(estSlope), NA))
df = rbind(df, c("sd:", sd(estCons), NA, sd(estSlope), NA))
df[2:5] <- lapply(df[2:5], function(x) as.numeric(as.character(x)))
colnames(df) = c("subject","constant", "t-stat","totWeightedValDiff", "t-stat")
t = xtable(df,digits=c(0,0,2,2,2,2))
print(t, type="latex", include.rownames=FALSE, hline.after=c(-1,0,nrow(t),nrow(t)-2))


# RT vs. total weighted value difference
estCons = rep(0,length(subjects))
tstatCons = rep(0,length(subjects))
estSlope = rep(0,length(subjects))
tstatSlope = rep(0,length(subjects))

for (s in 1:length(subjects)) {
  model = glm(rt[subject == subjects[s]] ~ totWValue[subject == subjects[s]])
  estCons[s] = model$coefficients[1]
  estSlope[s] = model$coefficients[2]
  tstatCons[s] = coef(summary(model))[1,3]
  tstatSlope[s] = coef(summary(model))[2,3]
}

df = data.frame(subjects,estCons,tstatCons,estSlope,tstatSlope)
df = rbind(df, c("mean:", mean(estCons), NA, mean(estSlope), NA))
df = rbind(df, c("sd:", sd(estCons), NA, sd(estSlope), NA))
df[2:5] <- lapply(df[2:5], function(x) as.numeric(as.character(x)))
colnames(df) = c("subject","constant", "t-stat","totWeightedValDiff", "t-stat")
t = xtable(df,digits=c(0,0,2,2,2,2))
print(t, type="latex", include.rownames=FALSE, hline.after=c(-1,0,nrow(t),nrow(t)-2))


# rt. vs. difficulty (total weighted value)
estCons = rep(0,length(subjects))
tstatCons = rep(0,length(subjects))
estSlope = rep(0,length(subjects))
tstatSlope = rep(0,length(subjects))

for (s in 1:length(subjects)) {
  model = lm(rt[subject == subjects[s]] ~ difficulty[subject == subjects[s]])
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
t = xtable(df, digits=c(0,0,2,2,2,2))
print(t, type="latex", include.rownames=FALSE, hline.after=c(-1,0,nrow(t),nrow(t)-2))

# basic individual choice logits
estCons = rep(0,length(subjects))
tstatCons = rep(0,length(subjects))
estEvDiff = rep(0,length(subjects))
tstatEvDiff = rep(0,length(subjects))
estQDiff = rep(0,length(subjects))
tstatQDiff = rep(0,length(subjects))

for (s in 1:length(subjects)) {
  name = subjects[s]
  model = glm(choice[subject == name] ~ evDiff[subject == name] +
                qValDiff[subject == name], family=binomial)
  estCons[s] = model$coefficients[1]
  estEvDiff[s] = model$coefficients[2]
  estQDiff[s] = model$coefficients[3]
  tstatCons[s] = coef(summary(model))[1,3]
  tstatEvDiff[s] = coef(summary(model))[2,3]
  tstatQDiff[s] = coef(summary(model))[3,3]
}

par(pty="s")
plot(estQDiff, estEvDiff, main="Choice logits per subject", 
     xlab="Q value diff coeff", ylab="Expected value diff coeff",
     xlim=c(0,7), ylim=c(0,7), pch=19)
abline(0,1)


# weighted individual choice logits
estCons = rep(0,length(subjects))
tstatCons = rep(0,length(subjects))
estWRelLot = rep(0,length(subjects))
tstatWRelLot = rep(0,length(subjects))
estWRelQ = rep(0,length(subjects))
tstatWRelQ = rep(0,length(subjects))

for (s in 1:length(subjects)) {
  name = subjects[s]
  model = glm(choice[subject == name] ~ wRelLot[subject == name] +
                wRelQ[subject == name], family=binomial)
  estCons[s] = model$coefficients[1]
  estWRelLot[s] = model$coefficients[2]
  estWRelQ[s] = model$coefficients[3]
  tstatCons[s] = coef(summary(model))[1,3]
  tstatWRelLot[s] = coef(summary(model))[2,3]
  tstatWRelQ[s] = coef(summary(model))[3,3]
}

par(pty="s")
plot(estWRelQ, estWRelLot, main="Choice logits per subject", 
     xlab="Weighted Q value diff coeff", ylab="Weighted expected value diff coeff",
     xlim=c(0,14), ylim=c(0,14), pch=19)
abline(0,1)

detach(data)
