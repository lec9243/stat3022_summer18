library(Stat2Data)
### 7.18
data(AutoPollution)
AutoPollution[2:4] = lapply(AutoPollution[2:4],as.factor)
aggregate(Noise~Size, data = AutoPollution, FUN = "mean")
lm1 = aov(Noise~Size, data = AutoPollution)
summary(lm1)

### 7.24
data(Pedometer)
t.test(Pedometer[Pedometer$DayType == "Weekday",]$Moderate,
       Pedometer[Pedometer$DayType == "Weekend",]$Moderate)
wilcox.test(Moderate~DayType, data = Pedometer)


### 7.27
data(CloudSeeding2)
lm2 = aov(TE~Season, data = CloudSeeding2)
summary(lm2)
par(mfrow = c(2, 2))
plot(lm2)
par(mfrow = c(1, 1))
kruskal.test(TE~Season, data = CloudSeeding2)
 

### 8.21


### 9.14
data(MedGPA)
lm3 = glm(Acceptance~MCAT, family = binomial, data = MedGPA)
summary(lm3)


### 9.27
data(ChemoTHC)
lm4 = glm(cbind(Effective, NotEffective)~Drug, family="binomial",data=ChemoTHC)
summary(lm4)
