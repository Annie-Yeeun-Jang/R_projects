install.packages("foreign")
install.packages("dplyr")
install.packages("ggplot2")
install.packages("readxl")
library(foreign)
library(dplyr)
library(ggplot2)
library(readxl)

setwd("D:/data/data_store")

raw_welfare<-read.spss("Koweps_hpc10_2015_beta1.sav", to.data.frame =T)

#make a copy
welfare<-raw_welfare

#revising data
head(welfare)
tail(welfare)
view(welfare)
dim(welfare)
str(welfare)

summary(welfare)

#change col names
welfare<-rename(welfare, sex=h10_g3,birth = h10_g4, marriage=h10_g10, religion = h10_g11, income=p1002_8aq1, code_job=h10_eco9, code_region = h10_reg7)

#Check outliers
table(welfare$sex)

#Trimming out the outliers
welfare$sex<-ifelse(welfare$sex == 9, NA, welfare$sex)

#NA check
table(is.na(welfare$sex))

#labeling sex
welfare$sex <-ifelse(welfare$sex == 1, "male", "female")
table(welfare$sex)

#Visualization
qplot(welfare$sex)

#2
class(welfare$income)
summary(welfare$income)
qplot(welfare$income)
qplot(welfare$income) + xlim(0,1000)

summary(welfare$income)
welfare$income<-ifelse(welfare$income %in% c(0,9999), NA,welfare$income)

table(is.na(welfare$income))

#average income by sex
sex_income<-welfare %>%
  filter(!is.na(income)) %>%
  group_by(sex) %>%
  summarise(mean_income = mean(income))

ggplot(data=sex_income, aes(x=sex, y=mean_income)) +geom_col()



#3
class(welfare$birth)
summary(welfare$birth)
qplot(welfare$birth)

#preprocessing
summary(welfare$birth)
table(is.na(welfare$birth))
welfare$birth <-ifelse(welfare$birth == 9999, NA, welfare$birth)
table(is.na(welfare$birth))
welfare$age<-2015-welfare$birth +1
qplot(welfare$age)

#relationship
age_income<-welfare%>%
  filter(!is.na(income)) %>%
  group_by(age)%>%
  summarise(mean_income = mean(income))
head(age_income)

#graph
ggplot(data = age_income, aes(x=age, y=mean_income)) + geom_line()
