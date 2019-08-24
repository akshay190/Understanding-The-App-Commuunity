#Load Requried Libraries
library(tidyverse)
library(shiny)
library(shinydashboard)
library(ggplot2)
library(dplyr)
library(tm)
library(stringr)
library(scales)
library(wordcloud)
library(DT)

#Set Working Directory
setwd(choose.dir())
getwd()

#Load "googleplaystore.csv" and "googleplaystore_user_reviews.csv" Files
app<-read.csv("googleplaystore.csv")
appreview<-read.csv("googleplaystore_user_reviews.csv",stringsAsFactors =F)
#View(app)
#View(appreview)
str(app)
#No.Of Observation
paste("No of ObserVation Is",nrow(app))
#No.Of Variable
paste("No of Variable Is",ncol(app))

#Data Cleaning
#Remove Those  Row Which Contain NaN 
app<-app %>% drop_na()
#Data Cleaning On appreview  
appreview$Translated_Review<-gsub("nan",NA,appreview$Translated_Review)
appreview<-appreview %>% drop_na()

#No.Of Observation
paste("No of ObserVation Is",nrow(app))
#No.Of Variable
paste("No of Variable Is",ncol(app))

#Replace Raw Data From Dataset
#Now Remove ',' or '+' or 'M' from Size Column
app$Size<-gsub(",","",app$Size)
app$Size<-gsub("+","",app$Size)
app$Size<-gsub("M","",app$Size)
app$Size<-gsub("k","",app$Size)
app$Size<-gsub("Varies with device",NA,app$Size)

#Now Remove '+' or ',' from Installs Column
app$Installs<-gsub(",","",app$Installs)
app$Installs<-gsub('Free',NA,app$Installs)
app$Installs<-substr(app$Installs,1,nchar(app$Installs)-1)
str(app)
#View(app)
dataset<-app
setdat <- app
playstore<-dataset

#Now Convert Data Into Numerical type
dataset$Installs <- as.character(dataset$Installs)
dataset$Installs <- as.numeric(dataset$Installs)
dataset$Reviews<-as.numeric(dataset$Reviews)
dataset$Size<-as.numeric(dataset$Size)

#View(dataset)
str(dataset)

data2<-dataset
data2<-data2 %>% select(c(Category,Rating,Genres))
indx<-sapply(data2,is.factor)
data2[indx]<-lapply(data2[indx],function(x) as.numeric(x))

set.seed(100)
trIndex<-sample(1:nrow(data2),0.7*nrow(data2))
trData<-data2[trIndex,]
tsData<-data2[-trIndex,]
lnmod<-lm(Rating~Category+Genres,data=trData)
pred<-predict(lnmod,tsData)
cf<-data.frame(Category=unique(dataset$Category),value=unique(as.numeric(dataset$Category)))
gf<-data.frame(Genres=unique(dataset$Genres),value=unique(as.numeric(dataset$Genres)))


#Find Frequency Of Each Word Accroding To Sentiment
plus<-appreview %>% filter(Sentiment=="Positive")
text_for_mining<-VectorSource(plus$Translated_Review)
tweets_corpus<-VCorpus(text_for_mining)
v<-tm_map(tweets_corpus,removePunctuation)
w<-tm_map(v,tolower)
w1<-tm_map(w,removeNumbers)
w2<-tm_map(w1,stripWhitespace)
w3<-tm_map(w2,removeWords,stopwords("english"))
v1<-DocumentTermMatrix(VCorpus(VectorSource(as.character(w3))))
v2<-as.matrix(v1)
DF22<-data.frame(word=names(v2[1,]),freq=v2[1,]+v2[2,]+v2[3,])
plus<- DF22 %>% filter(freq>1) %>% arrange(desc(freq)) %>% head(7000)
#wordcloud(words=plus$word,freq=plus$freq,random.order = TRUE,random.color = TRUE,colors=c("red","green","blue"),min.freq = 5)

nuet<-appreview %>% filter(Sentiment=="Neutral")
text_for_mining<-VectorSource(nuet$Translated_Review)
tweets_corpus<-VCorpus(text_for_mining)
v<-tm_map(tweets_corpus,removePunctuation)
w<-tm_map(v,tolower)
w1<-tm_map(w,removeNumbers)
w2<-tm_map(w1,stripWhitespace)
w3<-tm_map(w2,removeWords,stopwords("english"))
v1<-DocumentTermMatrix(VCorpus(VectorSource(as.character(w3))))
v2<-as.matrix(v1)
DF22<-data.frame(word=names(v2[1,]),freq=v2[1,]+v2[2,]+v2[3,])
nuet<- DF22 %>% filter(freq>1) %>% arrange(desc(freq)) %>% head(7000)
#wordcloud(words=nuet$word,freq=nuet$freq,random.order = TRUE,random.color = TRUE,colors=c("red","green","blue"),min.freq = 5)

minus<-appreview %>% filter(Sentiment=="Negative")
text_for_mining<-VectorSource(minus$Translated_Review)
tweets_corpus<-VCorpus(text_for_mining)
v<-tm_map(tweets_corpus,removePunctuation)
w<-tm_map(v,tolower)
w1<-tm_map(w,removeNumbers)
w2<-tm_map(w1,stripWhitespace)
w3<-tm_map(w2,removeWords,stopwords("english"))
v1<-DocumentTermMatrix(VCorpus(VectorSource(as.character(w3))))
v2<-as.matrix(v1)
DF22<-data.frame(word=names(v2[1,]),freq=v2[1,]+v2[2,]+v2[3,])
minus<- DF22 %>% filter(freq>1) %>% arrange(desc(freq)) %>% head(7000)
#wordcloud(words=minus$word,freq=minus$freq,random.order = TRUE,random.color = TRUE,colors=c("red","green","blue"),min.freq = 5)

