library(caret)
library(rpart)
library(ranger)
library(foreach)
library(data.table)  #data loading
library(ggplot2)     #data EDA
library(gbm)

setwd("C:\\Users\\lifebloom\\Desktop\\loan payment")

data_set=fread("challenge_data\\Data_set.csv",stringsAsFactors = T)
#data_set<-cbind(data_set[,c(rep(1:16))],data_set[,c(rep(53:69))])

data_set$STRT_CRDT_GRAD<-as.factor(data_set$STRT_CRDT_GRAD)
data_set$LTST_CRDT_GRAD<-as.factor(data_set$LTST_CRDT_GRAD)

data_set$LT1Y_PEOD_RATE<-as.character(data_set$LT1Y_PEOD_RATE)
data_set$LT1Y_PEOD_RATE[data_set$LT1Y_PEOD_RATE == "0"] <- "0"
data_set$LT1Y_PEOD_RATE[data_set$LT1Y_PEOD_RATE == "10미만"] <- "1"
data_set$LT1Y_PEOD_RATE[data_set$LT1Y_PEOD_RATE == "20미만"] <- "2"
data_set$LT1Y_PEOD_RATE[data_set$LT1Y_PEOD_RATE == "30미만"] <- "3"
data_set$LT1Y_PEOD_RATE[data_set$LT1Y_PEOD_RATE == "40미만"] <- "4"
data_set$LT1Y_PEOD_RATE[data_set$LT1Y_PEOD_RATE == "50미만"] <- "5"
data_set$LT1Y_PEOD_RATE[data_set$LT1Y_PEOD_RATE == "60미만"] <- "6"
data_set$LT1Y_PEOD_RATE[data_set$LT1Y_PEOD_RATE == "70미만"] <- "7"
data_set$LT1Y_PEOD_RATE[data_set$LT1Y_PEOD_RATE == "80미만"] <- "8"
data_set$LT1Y_PEOD_RATE[data_set$LT1Y_PEOD_RATE == "90미만"] <- "9"
data_set$LT1Y_PEOD_RATE[data_set$LT1Y_PEOD_RATE == "90이상"] <- "10"
data_set$LT1Y_PEOD_RATE<-as.integer(data_set$LT1Y_PEOD_RATE)

data_set$TEL_CNTT_QTR<-as.integer(data_set$TEL_CNTT_QTR)
year<-(2016-as.numeric(substr(data_set$TEL_CNTT_QTR,0,4)))*4
section<-substr(data_set$TEL_CNTT_QTR,5,5)
TEL<-year+as.numeric(section)
data_set$TEL_CNTT_QTR<-TEL


set.seed(1000) #reproducability setting
intrain<-createDataPartition(y=data_set$TARGET, p=0.7, list=FALSE) 
#data_set$TARGET<-as.factor(data_set$TARGET)
train<-data_set[intrain,-c(1)]
valid<-data_set[-intrain,-c(1)]

gbm_train=gbm(TARGET~.,data=train,distribution = "bernoulli",n.trees=350,shrinkage = 0.05,interaction.depth = 10,verbose = T)
n.tree=gbm.perf(gbm_train,method="OOB")
n.tree
gbm.pred=predict(gbm_train,valid,n.trees=350,num.threads=2)
gbm_bool<-ifelse(gbm.pred>-1.6,0,1)
gbm_conf<-confusionMatrix(actual_target,gbm_bool)
gbm_conf$byClass