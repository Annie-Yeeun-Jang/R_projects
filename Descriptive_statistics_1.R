#frequency
freq<-read.csv('D:/data/data_store/R_practice/07.grade.csv', header=TRUE, na.strings = '.')
str(freq)
freq$grade<-factor(freq$grade, levels=c(1:5),labels = c("F","D","C","B","A"))
str(freq)
attach(freq)
grade.n<-table(grade)
grade.n

grade.p<-prop.table(grade.n)
grade.p
grade.t<-cbind(grade.n,grade.p)
grade.t
grade.a<-addmargins(grade.t,margin = 1)
grade.a
detach(freq)

#pre
pre<-read.csv('D:/data/data_store/R_practice/07.pre.csv', header=TRUE, na.strings = '.')
str(pre)
pre$treat<-factor(pre$treat, levels=c(1,2),labels=c("Vitamin","Placebo"))
pre$cold<-factor(pre$cold, levels=c(1,2),labels = c("Cold","noCold"))
str(pre)

attach(pre)
pre.n<-table(treat,cold)
pre.n
pre.p<-prop.table(pre.n)
pre.p
pre.t<-cbind(pre.n,pre.p)
pre.a<-addmargins(pre.n)
pre.a

install.packages("gmodels")
library(gmodels)
pre<-CrossTable(treat,cold)
detach(pre)

#post
post<-read.csv('D:/data/data_store/R_practice/07.post.csv', header=TRUE, na.strings = '.')
str(post)
post$smoking<-factor(post$smoking, levels=c(1,2),labels=c("smok","nsmok"))
post$cancer<-factor(post$cancer, levels=c(1,2),labels=c("cancer","health"))
str(post)

attach(post)
post.n <-xtabs(observation~ cancer + smoking, data=post)
post.n
CrossTable(post.n)

#Q2
organic<-read.csv('D:/data/data_store/R_practice/07.organic.csv', header=TRUE, na.strings = '.')
str(organic)
organic$sex<-factor (organic$sex, levels = c(1,2), labels = c("남자","여자"))
organic$school<-factor (organic$school, levels = c(1:4), labels = c("고졸이하","대학재학","대학졸업","대학원이상"))
str(organic)

attach(organic)
#sex column
sex.n<-table(sex)
sex.n
sex.p<-prop.table(sex.n)
sex.p    
sex.t<-cbind(sex.n,sex.p)
sex.t   
sex.a<-addmargins(sex.t, margin =1)  
sex.a 

#organic column
organic.n<-table (school, sex)
organic.n
organic.p<-prop.table(organic.n)
organic.p
organic.t<-cbind(organic.n, organic.p)
organic.t
organic.a<-addmargins(organic.n)
organic.a
detach(organic)
