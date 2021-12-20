# t-test (Q1)
satisfaction<-read.csv('D:/data/data_store/R_practice/Ch09.satisfaction.csv', header = TRUE, na.strings = '.')
library(psych)
str(satisfaction)
describe(satisfaction)

#Draw Graphs
rpar<-par(no.readonly = TRUE)
par(mfrow=c(1,2))
boxplot(satisfaction$satis)
hist(satisfaction$satis, breaks=10, col='red',xlab="score",ylab="count", ylim=c(0,25),main="satisfaction score")
par(rpar)

#Analysis
options("scipen" = 20)
t.test(satisfaction$satis, alternative = c("two.sided"),mu=50,conf.level=0.95)

#result
mu=50
se=1.15
inter=qt(p=0.025, df=199)
data<-rnorm(1000,mu, se)
data<-sort(data)
plot(data, dnorm(data, mu,se),type='l', main = 'satisfaction score(mu=50) test',xlim=c(45,55))
abline(v-mu, col='green',lty=5)
abline(v=mu+inter*se, col = 'blue', lty=5)
abline(v=mu-inter*se, col = "blue", lty=5)
abline(v=49.54, col="red",lty=5)


#Q2
calorie.df<-read.csv('D:/data/data_store/R_practice/Ch09.calorie.csv', header = TRUE, na.strings = '.')
str(calorie.df)
library(psych)
describe(calorie.df)

#Draw Graphs
rpar<-par(no.readonly = TRUE)
par(mfrow=c(1,2))
boxplot(calorie.df$cal)
hist(calorie.df$cal, breaks = 10, col='red',xlab="kcal",ylab="count",ylim=c(0,25),main="Calorie")
par(rpar)

#Analysis
options("scipen" = 20)
t.test(calorie.df$cal, alternative = c("less"),mu=500,conf.level=0.95)

#result
mu=500
se=0.82
inter=qt(p=0.05, df=39, lower.tail = T)
data<-rnorm(1000,mu, se)
data<-sort(data)
plot(data, dnorm(data, mu,se),type='l', main = '칼로리(mu<=500)검정')
abline(v-mu, col='green',lty=5)
abline(v=mu+inter*se, col = 'blue', lty=5)
abline(v=498.48, col="red",lty=5)


#Q3
effect<-read.csv('D:/data/data_store/R_practice/Ch09.effect.csv', header = TRUE, na.strings = '.')
str(effect)
library(psych)
describe(effect)

#Draw Graph
rpar<-par(no.readonly = TRUE)
par(mfrow=c(1,2))
boxplot(effect$time)
hist(effect$time, breaks = 10, col='red',xlab="time duration",ylab="개수",ylim=c(0,25),main="진통제 지속시간")
par(rpar)

#Analysis
options("scipen" = 20)
t.test(effect$time, alternative = c("greater"),mu=5,conf.level=0.95)

#result
mu=5
se=0.05
inter=qt(p=0.05, df=39, lower.tail = F)
data<-rnorm(1000,mu, se)
data<-sort(data)
plot(data, dnorm(data, mu,se),type='l', main = '진통제지속시간(mu>=5)검정')
abline(v-mu, col='green',lty=5)
abline(v=mu+inter*se, col = 'blue', lty=5)
abline(v=5.07, col="red",lty=5)

#independent sample t-test
hospital<-read.csv('D:/data/data_store/R_practice/Ch10.hospital.csv', header = TRUE, na.strings = '.')
hospital$group<-factor(hospital$group, levels=c(1,2), labels=c("님자","여자"))
str(hospital)
library(psych)
describeBy(hospital$total, hospital$group, mat=T)


#Draw Graph
rpar<-par(no.readonly = TRUE)
layout(matrix(c(1,1,2,3),2,2,byrow=TRUE))
boxplot(hospital$total ~ hospital$group)
hist(hospital$total[hospital$group=="남자"])
hist(hospital$total[hospital$group=="여자"])
par(rpar)

#Homoscedasticity test
var.test(hospital$total~hospital$group,data=hospital)

#Analysis
options("scipen" = 20)
t.test(hospital$total~hospital$group, data=hospital, alternative = c("two.sided"),var.equal=TRUE,conf.level=0.95)

#Men
x=67.82
se=13.99018/(sqrt(50))
data<-rnorm(1000,x,se)
data<-sort(data)
plot(data,dnorm(data,x,se),col="blue",type="l",main="남자와 여자 간의 만족도 차이",xlim=c(55,90),ylim=c(0,0.20))
abline(v=x, col="blue", lty=3)
par(new=T)

#Women
x=78.546
se=13.02626/(sqrt(50))
data<-rnorm(1000,x,se)
data<-sort(data)
plot(data, dnorm(data,x,se),col="blue",type="l",xlim=c(55,90),ylim=c(0,0.20))
abline(v=x,col="blue",lty=3)
