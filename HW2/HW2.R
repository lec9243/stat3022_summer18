library(Stat2Data)
data(USstamps)
help(USstamps)
head(USstamps)
plot(Price~Year, data=USstamps, main="Price (in cent) vs Year")
rm4=USstamps[c(-1,-2,-3,-4),]
lm1=lm(Price~Year, data=rm4)
abline(lm1,col="blue")
summary(lm1)
par(mfrow=c(2,2))
plot(lm1)

par(mfrow=c(1,1))
data(Pines)
help(Pines)
head(Pines)
plot(Hgt97~Hgt90, data=Pines, main="Height90 vs Height97")
lm2=lm(Hgt97~Hgt90, data=Pines)
abline(lm2, col="blue")
summary(lm2)
par(mfrow=c(2,2))
plot(lm2)

par(mfrow=c(1,1))
data(Caterpillars)
help(Caterpillars)
head(Caterpillars)
plot(Cassim~Intake, data=Caterpillars, main="Cassim vs Intake")
lm3=lm(Cassim~Intake, data=Caterpillars)
abline(lm3,col="blue")
summary(lm3)
par(mfrow=c(2,2))
plot(lm3)

par(mfrow=c(1,1))
data(TextPrices)
help(TextPrices)
head(TextPrices)
plot(Price~Pages, data=TextPrices, main="Pages vs Price")
lm4=lm(Price~Pages, data=TextPrices)
abline(lm4,col="blue")
summary(lm4)
anova(lm4)
7.653^2
#p value<0.05, reject anova.
#hypothesis: H0: beta1 = 0;H1: beta1 != 0 
par(mfrow=c(2,2))
plot(lm4)
confint(lm4, level=0.95)

#summary(lm4)$coefficients
#betahat1 =0.14
#se.betahat1 = 0.019
#n = nrow(TextPrices)
#quantile = (1-0.95)/2
#t.critical = qt(quantile, n-2, lower = F)
#c(betahat1 - t.critical*se.betahat1,
#  betahat1+t.critical*se.betahat1)
#the true slope of  price is a measure of change in price lies between .10 and .19 with 0.95 confidence.
#0 not in CI, supports reject null

par(mfrow=c(1,1))
data(Sparrows)
lm5 = lm(Weight ~ WingLength, data = Sparrows)
summary(lm5)
confint(lm5, level=0.95)


data(MathEnrollment)
help(MathEnrollment)
head(MathEnrollment)
mathenroll = data.frame(MathEnrollment)
newMathenroll = subset(mathenroll, Ayear!=2003)
lm6 = lm(Spring ~ Fall, data = newMathenroll)
summary(lm6)
new = data.frame(Fall = 290)
predict(lm6, newdata = new)
predict(lm6, newdata = new, interval = "confidence", level = 0.95)
predict(lm6, newdata = new, interval = "predict", level = 0.95)

