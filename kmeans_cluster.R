#Kmeans clustering

library("stats")
data2017<-read.csv("C:\\data_store\\데이터최종2017_2.csv",fileEncoding="EUC-KR")
data2017
data2017<-data2017[-1]
is.na(data2017)
data2017[1]

rownames(data2017)<-c("종로구","중구", "용산구","성동구","광진구","동대문구","중랑구","성북구","강북구","도봉구","노원구","은평구","서대문구","마포구","양천구","강서구","구로구","금천구","영등포구","동작구","관악구","서초구","강남구","송파구","강동구")
data2017
s_data2017<-scale(data2017)
clustering<-kmeans(s_data2017,2)
clustering$size
clustering$centers
clustering$cluster



library(NbClust)
nc<-NbClust(data2017,min.nc = 2,max.nc = 6, method = "kmeans")

d_s<-cbind(data2017[,1],data2017[,4],data2017[,5])
rownames(d_s)<-c("종로구","중구", "용산구","성동구","광진구","동대문구","중랑구","성북구","강북구","도봉구","노원구","은평구","서대문구","마포구","양천구","강서구","구로구","금천구","영등포구","동작구","관악구","서초구","강남구","송파구","강동구")

c_dust<-read.csv("C:\\data_store\\2017_dust_MHL.csv",fileEncoding="EUC-KR")
mid_dust<-c_dust[10:18,]
rownames(mid_dust)<-c("양천구","관악구","동대문구","도봉구","은평구","강남구","중랑구","구로구","송파구")

high_dust<-c_dust[19:25,]
rownames(high_dust)<-c("성동구","성북구","서대문구","강서구","영등포구","서초구","강동구")

mid_hc<-hclust(dist(mid_dust), method="average")
str(mid_hc)

plot(mid_hc, hang=1)
str(hclust)
hclust$height

groups<-cutree(hclust,k=3)
groups
rect.hclust(hclust, k=3)

data5yrs<-read.csv("C:\\data_store\\data5yrs.csv", fileEncoding = "EUC-KR")
data5yrs

library(dplyr)
data__<-select(data5yrs, one_of("region", "durt","tree","park"))
data__

y_value<-data__[,1]
y_value
s_data__<-scale(data__[-1])
s_data<-cbind(s_data__,y_value)
hclust<-hclust(dist(s_data__), method="centroid")
hclust
summary(hclust)
str(hclust)
plot(hclust, hang = 1)




