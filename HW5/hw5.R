1-pf(129.15, 1, 96)
1-pf(25.0, 1, 96)
1-pf(4.0, 1, 96)
library(Stat2Data)
data(Alfalfa)
aggregate(Ht4~Row, data = Alfalfa,FUN= "mean" )
aggregate(Ht4~Acid, data = Alfalfa,FUN= "mean" )
mean(Alfalfa$Ht4)
sd(Alfalfa$Ht4)
lm1 = aov(Ht4~Acid+Row, data=Alfalfa)
summary(lm1)
par(mfrow = c(2, 2))
plot(lm1)

data(Swahili)
par(mfrow = c(1, 1))
interaction.plot(Swahili$Sex, Swahili$Province, Swahili$Attitude.Score)
lm2=aov(Attitude.Score~as.factor(Province) * as.factor(Sex), data = Swahili)
summary(lm2)
par(mfrow = c(2, 2))
plot(lm2)
par(mfrow = c(1, 1))
NM = Swahili[Swahili$Province == "NAIROBI"&Swahili$Sex == "male",]
sd(NM$Attitude.Score)
NF = Swahili[Swahili$Province == "NAIROBI"&Swahili$Sex == "female",]
sd(NF$Attitude.Score)
PF = Swahili[Swahili$Province == "PWANI"&Swahili$Sex == "female",]
sd(PF$Attitude.Score)

data(Blood1)
Blood1$Overwt = as.factor(Blood1$Overwt)
Blood1$Smoke = as.factor(Blood1$Smoke)
interaction.plot(Blood1$Overwt, Blood1$Smoke, Blood1$SystolicBP)
lm3 = lm(SystolicBP ~ Smoke * Overwt, data = Blood1)
anova(lm3)
source("LSDtest.R")
lout = LSDtest(Blood1$SystolicBP, Blood1$Overwt, alpha = 0.05)
lout
subset(lout, sign(lwr)==sign(upr))
ncomp = choose(nlevels(Blood1$Overwt), 2)
bout = LSDtest(Blood1$Overwt, Blood1$SystolicBP, alpha = 0.05/ncomp)
bout
subset(bout, sign(lwr)==sign(upr))
lm4 = aov(SystolicBP ~ Overwt, data = Blood1)
summary(lm4)
TukeyHSD(lm4, "Overwt", data = Blood1)


