#Descriptive Statistics 2

wgt<-read.csv('D:/data/data_store/R_practice/08.wgt.csv', header = TRUE, na.strings = '.')
str(wgt)
wgt$sex <-factor (wgt$sex, levels = c(1,2), labels = c("남자","여자"))
str(wgt$sex)
attach(wgt)

mean(weight, na.rm=T)
median(weight)
mean(weight)
summary(wgt)

#Trim mean
mean(weight, trim = 0.1)

detach(wgt)

#Weighted mean
avg<-c(90,85,80)
wt<-c(40,27,33)
mean(avg)
weighted.mean(avg,wt)

#Distribution
attach(wgt)
min(weight)
max(weight)
diff(range(weight))
quantile(weight,c(0.25,0.5,0.75,1.0))
var(weight)
sd(weight)
skew(weight)
kurtosi(weight)
detach(wgt)

library(dplyr)
wgt<-transform(wgt,weight.z=scale(weight))

#Descriptive Statistics
attach(wgt)
summary(weight)
install.packages("psych")
library(psych)
describe(weight)
boxplot(weight)
wgt<-wgt[!(weight>80),]
describe(weight)

#Comparison
tapply(weight, sex, mean)
tapply(weight, sex, summary)
describeBy(wgt[c("weight")],sex, mat=TRUE)

