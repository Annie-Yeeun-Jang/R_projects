#hcluster
library("stats")
library(dplyr)
data2017<-read.csv("C:\\data_store\\데이터최종2017_2.csv",fileEncoding="EUC-KR")
data2017
data2017<-data2017[-1]
data2017[1]

rownames(data2017)<-c("종로구","중구", "용산구","성동구","광진구","동대문구","중랑구","성북구","강북구","도봉구","노원구","은평구","서대문구","마포구","양천구","강서구","구로구","금천구","영등포구","동작구","관악구","서초구","강남구","송파구","강동구")
data2017
s_data2017<-scale(data2017)

d_s<-cbind(data2017[,1],data2017[,4],data2017[,5])
rownames(d_s)<-c("종로구","중구", "용산구","성동구","광진구","동대문구","중랑구","성북구","강북구","도봉구","노원구","은평구","서대문구","마포구","양천구","강서구","구로구","금천구","영등포구","동작구","관악구","서초구","강남구","송파구","강동구")

c_dust<-read.csv("C:\\data_store\\2017_dust_MHL.csv",fileEncoding="EUC-KR")

mid_dust<-c_dust[10:18,]

rownames(mid_dust)<-c("양천구","관악구","동대문구","도봉구","은평구","강남구","중랑구","구로구","송파구")

high_dust<-c_dust[19:25,]
rownames(high_dust)<-c("성동구","성북구","서대문구","강서구","영등포구","서초구","강동구")

#mid clust
mid_hc<-hclust(dist(mid_dust[-1]), method="average")
str(mid_hc)

plot(mid_hc, hang=1)
str(mid_hc)
mid_hc$height

groups<-cutree(mid_hc,k=5)
groups
rect.hclust(mid_hc, k=5)

c_plot<-read.csv("C:\\data_store\\2017_dust_MHL_2.csv",fileEncoding="EUC-KR")
m_plot<-c_plot[10:18,]
m_numeric<-m_plot[,3:4]
plot(m_numeric[,1],m_numeric[,2], xlab="가로녹시율", ylab="녹지면적",type='n')
text(m_numeric[,1],m_numeric[,2], m_plot[,1]) 


#high dust
s_high_dust<-scale(high_dust)
s_high_dust
rownames(s_high_dust)<-c("성동구","성북구","서대문구","강서구","영등포구","서초구","강동구")
s_high_dust
high_hc<-hclust(dist(s_high_dust[-1]), method="average")
str(high_hc)

plot(high_hc, hang=1)
str(high_hc)
high_hc$height

groups<-cutree(high_hc,k=5)
groups
rect.hclust(high_hc, k=4)

#high plot draw
h_plot<-c_plot[19:25,]
h_numeric<-h_plot[,3:4]
plot(h_numeric[,1],h_numeric[,2], xlab="가로녹시율", ylab="녹지면적",type='n')
text(h_numeric[,1],h_numeric[,2], h_plot[,1]) 


