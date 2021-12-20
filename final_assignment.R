#Q1
#(1)
company_17101983<-read.csv('D:/data/data_store/R_practice/company_4.csv', header = TRUE, na.strings = '.')
str(company_17101983)
#(2)
company_17101983$ROE_17101983<-company_17101983$earning/company_17101983$equity*100
mean_ROE<-mean(company_17101983$ROE_17101983)
company_17101983$GR_17101983<-ifelse(company_17101983$ROE_17101983>=mean_ROE,"high","low")

#(3)
library("dplyr")
mean_ROE #-2.209357
low<-filter(company_17101983, GR_17101983=='low')
str(low) #3094
high<-filter(company_17101983, GR_17101983=='high')
str(high) #11888

#(4) 
comp2008_17101983<-filter(company_17101983, EY==2008)  
comp2008_17101983$ROA_17101983 <- comp2008_17101983$earning / comp2008_17101983$asset *100

comp2008_17101983 %>%
  group_by(market)%>%
  summarise(Mean=mean(ROA_17101983), Num=n()) %>%
  arrange(desc(Mean)) %>%
  head(5)



#Q2

c_17101983 <- function(x) { 
  c<- (x-32)*(5/9)
  print(c)
  result=ifelse(c<0, 'cold','notcold!')
  print(result)
}
c_17101983(100)


#Q3

#(1)
esb_17101983<-read.csv('D:/data/data_store/R_practice/ESB_E_2012.csv', header = TRUE, na.strings = '.')
str(esb_17101983)
esb_17101983$market<-factor(esb_17101983$market)

#(2)
library("psych")
describeBy(esb_17101983$TOTAL,esb_17101983$market, mat=T)


#(3)
rpar<-par(no.readonly = TRUE)
boxplot(esb_17101983$TOTAL~esb_17101983$market) #boxplot => no outliers

#(4)
var.test(esb_17101983$TOTAL~esb_17101983$market, data=esb_17101983)  #not homoscedastic
t.test(esb_17101983$TOTAL~esb_17101983$market, data=esb_17101983, alternative = c("two.sided"), var.equal = FALSE, conf.level = 0.95)
#p value가 is lower than 0.05, thus reject H0=> those two are different




