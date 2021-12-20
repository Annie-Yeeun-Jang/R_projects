#Terms_documents_naive_bayes

setwd("c:/data_store")
sms_raw <- read.csv("sms_spam.csv", stringsAsFactors = FALSE)
str(sms_raw)

install.packages("tm")
library(tm)
sms_corpus<- Corpus(VectorSource(sms_raw$text))
sms_corpus<-Corpus(VectorSource(sms_raw$text))

#corpus preprocessing
inspect(sms_corpus[1:5])
corpus_clean<-tm_map(sms_corpus,content_transformer(tolower))
corpus_clean<-tm_map(corpus_clean,removeNumbers)
corpus_clean<-tm_map(corpus_clean,removeWords,stopwords())                     
corpus_clean<-tm_map(corpus_clean,removePunctuation) 

inspect(sms_corpus[1:5])
inspect(corpus_clean[1:5])

#Document_Term_Matrix
sms_dtm<-DocumentTermMatrix(corpus_clean)
sms_raw_train<-sms_raw[1:4169,]  #every row
sms_raw_test<-sms_raw[4170:5559,]
sms_dtm_train<-sms_dtm[1:4169,]
sms_dtm_test<-sms_dtm[4170:5559,]

sms_corpus_train<-corpus_clean[1:4169]
sms_corpus_test<-corpus_clean[4170:5559]

table(sms_raw_train$type)
prop.table(table(sms_raw_train$type))

sms_dict<-findFreqTerms(sms_dtm_test,5) 
sms_train<-DocumentTermMatrix(sms_corpus_train,list(dictionary=sms_dict))
sms_test<-DocumentTermMatrix(sms_corpus_test,list(dictionary=sms_dict))
convert_count<-function(x){x<-ifelse(x>0,1,0)
x<-factor(x,levels = c(0,1),labels = c("No","Yes"))
return(x)}
sms_train<-apply(sms_train,MARGIN=2,convert_count)
sms_test<-apply(sms_test,MARGIN=2,convert_count)

install.packages("e1071")
library(e1071)
#Naive Bayes
sms_classifier<-naiveBayes(sms_train,sms_raw_train$type)
sms_test_pred<-predict(sms_classifier,sms_test)

install.packages("gmodels")
library("gmodels")
CrossTable(sms_test_pred,sms_raw_test$type)

m<-naiveBayes(sms_train,sms_raw_train$type,laplace = 1)
p<-predict(m,sms_test)

CrossTable(sms_)