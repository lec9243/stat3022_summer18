library(Stat2Data)
## Problem 2.25
data(Pines)
head(Pines)
lm1=lm(Hgt97~Hgt90, data=Pines)
summary(lm1)
summary(lm1)$adj.r.squared
summary(lm1)$r.squared
anova(lm1)
138344/(5010010+138344)

## Problem 3.13
data(MathEnrollment)
head(MathEnrollment)
mathenroll = data.frame(MathEnrollment)
newMathenroll = subset(mathenroll, Ayear!=2003)
lm2=lm(formula = Spring~Fall+Ayear, data=newMathenroll)
summary(lm2)
par(mfrow=c(2,2))
plot(lm2)
## Problem 3.14
summary(lm2)$adj.r.squared
summary(lm2)$r.squared
summary(lm2)$sigma 
anova(lm2)

## Problem 3.19
data(Speed)
head(Speed)
lm3=lm(FatalityRate~Year, data=Speed)
summary(lm3)
anova(lm3)
par(mfrow=c(2,2))
plot(lm3)
lm4=lm(formula = FatalityRate~Year+StateControl+Year*StateControl, data=Speed)
summary(lm4)

## Problem 3.21
data(BritishUnions)
head(BritishUnions)
BritishUnions$Late = as.factor(BritishUnions$Late)
lm5=lm(formula = NetSupport~Months+Late+Months*Late, data=BritishUnions)
summary(lm5)
anova(lm5)
lm6=lm(formula = NetSupport~Months, data=BritishUnions)
anova(lm6,lm5)

##Problem 3.22
par(mfrow=c(1,1))
plot(NetSupport~Unemployment, data=BritishUnions, main="NetSupport vs Unemployment")
lm7=lm(formula = NetSupport~Unemployment, data=BritishUnions)
par(mfrow=c(2,2))
plot(lm7)
summary(lm7)
lm8=lm(formula = NetSupport~Unemployment+Months, data=BritishUnions)
plot(lm8)
summary(lm8)

## Problem 3.30
data(Pollster08)
head(Pollster08)
lm9=lm(formula = Margin~Days+I(Days^2),data=Pollster08)
summary(lm9)
anova(lm9)
Pollster08$Charlie = as.factor(Pollster08$Charlie)
lm10=lm(formula = Margin~Days+Charlie, data=Pollster08)
summary(lm10)
anova(lm10)
Pollster08$Meltdown = as.factor(Pollster08$Meltdown)
lm11=lm(formula = Margin~Days+Meltdown, data=Pollster08)
summary(lm11)
anova(lm11)









