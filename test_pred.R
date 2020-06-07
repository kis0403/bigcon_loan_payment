library(caret)
library(rpart)
library(ranger)
library(foreach)
library(data.table)  #data loading
library(ggplot2)     #data EDA
library(gbm)

setwd("C:\\Users\\lifebloom\\Desktop\\loan payment")

data_set=fread("challenge_data\\Data_set.csv",stringsAsFactors = T)



##7:3 train/valid
set.seed(1000) #reproducability setting
intrain<-createDataPartition(y=data_set$TARGET, p=0.7, list=FALSE) 
#data_set$TARGET<-as.factor(data_set$TARGET)
train<-data_set[intrain,-c(1)]
valid<-data_set[-intrain,-c(1)]
          #train2<-cbind(train[,c(1,16,21,22,34,52,53,56,59,66,67)],scale(train[,-c(1,16,21,22,34,52,53,56,59,66,67)]))



##logistic reg
logis_train=glm(TARGET~.,data=train,family=binomial)
logis_pred=predict.glm(logis_train,newdata = valid,type="response")

logis_bool<-ifelse(logis_pred>.2,1,0)
actual_target=as.numeric(valid$TARGET)-1
logis_bool
actual_target

logis_conf<-confusionMatrix(actual_target,logis_bool)
logis_conf$byClass                                              #F1 0.40612468

##D tree
tree_train=rpart(TARGET~.,data=train)
tree_pred<-predict(tree_train,newdata=valid,type="vector")

tree_bool<-ifelse(tree_pred>.2,0,1)

tree_conf<-confusionMatrix(actual_target,tree_bool)
tree_conf$byClass                                               #F1 0.33678441


##rf
rf_train=ranger(TARGET~.,data=train,num.trees=300,mtry=5,verbose = T)
rf_pred=predict(rf_train,valid)

actual_target=1-as.numeric(valid$TARGET)
rf_bool<-ifelse(rf_pred$predictions>0.2,0,1)
str(train)

rf_conf<-confusionMatrix(actual_target,rf_bool)
rf_conf$byClass                                           #F1 0.43745907



head(rf_pred)






















####################SMOTE#####################################
intrain<-createDataPartition(y=data_set$TARGET, p=0.7, list=FALSE) 
data_set$TARGET<-as.factor(data_set$TARGET)
train<-data_set[intrain,-c(1)]
valid<-data_set[-intrain,-c(1)]
newData <- SMOTE(TARGET ~ ., train, perc.over = 550,perc.under=130)
table(newData$TARGET)

valid$TARGET<-as.integer(valid$TARGET)
newData$TARGET<-as.integer(newData$TARGET)

rf_train=ranger(TARGET~.,data=newData,num.trees=300,mtry=5)
rf_pred=predict(rf_train,valid,type="response")

rf_bool<-ifelse(rf_pred$predictions>1.53,0,1)
rf_conf<-confusionMatrix(actual_target,rf_bool)
rf_conf$byClass 


actual_target
rf_bool



##para(rf)
rf.grid=expand.grid(ntree=c(200,300,400,500),mtry=c(3,4,5))

result=foreach(g=1:NROW(rf.grid),.combine = rbind) %do% {
  #training model
  m<-ranger(TARGET ~.,data=train,num.trees=rf.grid[g,"ntree"],mtry=rf.grid[g,"mtry"],importance='impurity')
  #prediction
  pred<-predict(m,valid)
  #f1
  rf_bool<-ifelse(pred$predictions>0.2,0,1)
  rf_conf<-confusionMatrix(actual_target,rf_bool)
  f1<-rf_conf$byClass[7]
  return(data.frame(g=g,num.trees=rf.grid[g,"ntree"],mtry=rf.grid[g,"mtry"],f1_acc=f1))
  cat(g)
}

result                                                          # n.tree 300    mtry 5 0.4393101





############################

gbm_train=gbm(TARGET~.,data=train,distribution = "bernoulli",n.trees=300,shrinkage = 0.05,interaction.depth = 8,verbose = T)
n.tree=gbm.perf(gbm_train,method="OOB")
n.tree
gbm.pred=predict(gbm_train,valid,n.trees=300,num.threads=2)
gbm.pred
gbm_bool<-ifelse(gbm.pred>-1.5,0,1)
gbm_conf<-confusionMatrix(actual_target,gbm_bool)
gbm_conf$byClass

    





str(data_balanced_over)
install.packages("ROSE")
library(ROSE)
#over sampling
data_balanced_over <- ovun.sample(TARGET ~ ., data = train, method = "over",N = 140000)$data
table(data_balanced_over$TARGET)


#0.46~~





gbm.grid=expand.grid(n.trees=c(250,300,350,400),interaction.depth=c(7,8,9,10),cut.off=c(-1.4,-1.45,-1.5,-1.55,-1.6))
actual_target
result=foreach(g=1:NROW(gbm.grid),.combine = rbind) %do% {
  #training model
  m=gbm(TARGET~.,data=train,distribution = "bernoulli",n.trees=gbm.grid[g,"n.trees"],shrinkage = 0.05,interaction.depth = gbm.grid[g,"interaction.depth"],verbose = T)
  #prediction
  pred=predict(m,valid,n.trees=m$n.trees,num.threads=2)
  #f1
  gbm_bool<-ifelse(pred>gbm.grid[g,"cut.off"],0,1)
  gbm_conf<-confusionMatrix(actual_target,gbm_bool)
  f1<-gbm_conf$byClass[7]
  return(data.frame(g=g,num.trees=gbm.grid[g,"n.trees"],interaction.depth=gbm.grid[g,"interaction.depth"],cut.off=gbm.grid[g,"cut.off"],f1_acc=f1))
}

result
#350,10,-1.6               0.4709741
#400,10,-1.5               0.4709501
#300,9,-1.5                0.4707916


install.packages("unbalanced")
library(unbalanced)

train2<-train#[,-c(16,21,22,34,52,53,56,59,66,67)]
train2$TARGET<-as.factor(train2$TARGET)

smote_train<-ubSMOTE(train2,train2$TARGET, perc.over = 550, k = 5, perc.under = 120, verbose = TRUE)

smote_train$X$TARGET<-as.integer(smote_train$X$TARGET)

train_sm<-smote_train$X
train_sm$TARGET<-train_sm$TARGET-1





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

