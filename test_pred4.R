library(caret)       #preprocess,eval
library(rpart)       #rf
library(ranger)      #rf
library(foreach)     #parallel
library(data.table)  #data loading
library(ggplot2)     #data EDA
library(gbm)         #GBM
library(unbalanced)  #data sampling


setwd("C:\\Users\\lifebloom\\Desktop\\loan payment")
#setwd("C:\\Users\\kis\\Desktop\\lib")

data_set=fread("challenge_data\\Data_set.csv",stringsAsFactors = T)
#data_set<-cbind(data_set[,c(rep(1:2))],data_set[,c(rep(3:6))])
##############################
# SMOTE
# train$TARGET=as.factor(train$TARGET)
# train2=ubSMOTE(train,train$TARGET, perc.over = 300, k = 5, perc.under = 120, verbose = TRUE)
# train=train2$X
# train$TARGET=as.numeric(train$TARGET)-1
# table(train$TARGET)
##############################

#table(data_set$CRDT_CARD_CNT,data_set$TARGET)/apply(table(data_set$CRDT_CARD_CNT,data_set$TARGET),1,sum)
##############################
# factorizing
# for (i in c(1:69)) {
#   a=colnames(data_set)[i]
#   data_set$a<-as.factor(data_set$a)
# }
# a
#############################

#한화생명신용상환금액/한화생명신용대출금액
data_set$TOT_CRLN_ratio<-data_set$TOT_REPY_AMT/data_set$TOT_CRLN_AMT
data_set$TOT_CRLN_ratio <- ifelse(is.nan(data_set$TOT_CRLN_ratio),0,data_set$TOT_CRLN_ratio)

#가구총지급건수당보험금액
data_set$FYCM_PAID_ratio<-data_set$FYCM_PAID_AMT/data_set$FMLY_CLAM_CNT
data_set$FYCM_PAID_ratio <- ifelse(is.nan(data_set$FYCM_PAID_ratio),0,data_set$FYCM_PAID_ratio)

 
#최초와 최근 신용등급 차이
data_set$STRT_CRDT_GRAD[data_set$STRT_CRDT_GRAD==0]<-5
data_set$LTST_CRDT_GRAD[data_set$LTST_CRDT_GRAD==0]<-5
data_set$CRDT_DIFF<-data_set$LTST_CRDT_GRAD-data_set$STRT_CRDT_GRAD

######################################################################
# ratio_LNIF는 전체 금액을 전체 건수로 나눈 비율
data_set$ratio_LNIF =data_set$TOT_LNIF_AMT/(data_set$BNK_LNIF_CNT+data_set$CPT_LNIF_CNT+data_set$SPART_LNIF_CNT+data_set$ECT_LNIF_CNT)
data_set$ratio_LNIF = ifelse(is.infinite(data_set$ratio_LNIF),997001,data_set$ratio_LNIF)
######################################################################


######################################################################
# ratio_CLIF_d_LNIF
data_set$ratio_CLIF_d_LNIF =data_set$TOT_CLIF_AMT/data_set$TOT_LNIF_AMT
######################################################################

######################################################################
# ratio_BNK_d_LNIF
data_set$ratio_BNK_d_LNIF =data_set$BNK_LNIF_AMT/data_set$TOT_LNIF_AMT
######################################################################

######################################################################
# ratio_CPT_d_LNIF
data_set$ratio_CPT_d_LNIF =data_set$CPT_LNIF_AMT/data_set$TOT_LNIF_AMT
######################################################################

######################################################################
# ratio_BNK_d_N
data_set$ratio_BNK_d_N =data_set$BNK_LNIF_AMT/data_set$BNK_LNIF_CNT
data_set$ratio_BNK_d_N = ifelse(is.nan(data_set$ratio_BNK_d_N),0,data_set$ratio_BNK_d_N)
######################################################################

######################################################################
# ratio_CPT_d_N
data_set$ratio_CPT_d_N =data_set$CPT_LNIF_AMT/data_set$CPT_LNIF_CNT
data_set$ratio_CPT_d_N <- ifelse(is.nan(data_set$ratio_CPT_d_N),0,data_set$ratio_CPT_d_N)
######################################################################

######################################################################
# 전체 대출 횟수
data_set$all_loan <- data_set$BNK_LNIF_CNT+data_set$CPT_LNIF_CNT+data_set$SPART_LNIF_CNT+data_set$ECT_LNIF_CNT
######################################################################

######################################################################
# 은행대출 / 전체 대출 횟수
data_set$BNK_d_all_loan <- data_set$BNK_LNIF_CNT/data_set$all_loan
data_set$BNK_d_all_loan <- ifelse(is.nan(data_set$BNK_d_all_loan),0,data_set$BNK_d_all_loan)
######################################################################

######################################################################
# 카드사/할부사/캐피탈 / 전체 대출 횟수
data_set$CPT_d_all_loan <- data_set$CPT_LNIF_CNT/data_set$all_loan
data_set$CPT_d_all_loan <- ifelse(is.nan(data_set$CPT_d_all_loan),0,data_set$CPT_d_all_loan)
######################################################################

######################################################################
# [2산업분류]/ 전체 대출 횟수
data_set$SPART_d_all_loan <- data_set$SPART_LNIF_CNT/data_set$all_loan
data_set$SPART_d_all_loan <- ifelse(is.nan(data_set$SPART_d_all_loan),0,data_set$SPART_d_all_loan)
######################################################################

######################################################################
# 은행, 카드사/할부사/캐피탈 제외 대출금액
data_set$except_loan <-data_set$TOT_LNIF_AMT-data_set$BNK_LNIF_AMT-data_set$CPT_LNIF_AMT
data_set$except_loan <- ifelse(data_set$except_loan<0,0,data_set$except_loan)
######################################################################
# 
# ######################################################################
# # ETC_d_all_loan
# data_set$ETC_d_all_loan <- data_set$ECT_LNIF_CNT/ data_set$all_loan
# ######################################################################
# 
# ######################################################################
# # loan_type
# data_set$loan_type <- data_set$ratio_BNK_d_LNIF
# data_set$loan_type[which(data_set$ratio_BNK_d_LNIF>0.5 &  data_set$ratio_BNK_d_LNIF!=1)] <- 2
# data_set$loan_type[(data_set$ratio_BNK_d_LNIF<=0.5 & data_set$ratio_BNK_d_LNIF!=0)] <- 3
# ######################################################################
# 
# ######################################################################
# # ratio_except_loan_d_LNIF
# data_set$ratio_except_loan_d_LNIF <- data_set$except_loan / data_set$TOT_LNIF_AMT
# ######################################################################
# 
# ######################################################################
# # per_income
# data_set$perincome <- data_set$HSHD_INFR_INCM/data_set$ACTL_FMLY_NUM
# ######################################################################
# 
# 
# 
# ######################################################################
# # ratio_CNT_d_AMT
# data_set$ratio_CNT_d_AMT <- data_set$all_loan / data_set$TOT_LNIF_AMT



######2017.09.03 Precision79   -1.50 0.55520254 0.4205776 0.47860322  

#prop.table(table(data_set$CRDT_CARD_CNT,data_set$TARGET),1)
data_set$exp_CRDT_CARD_CNT=data_set$TOT_LNIF_AMT/data_set$CRDT_CARD_CNT^2
data_set$exp_CRDT_CARD_CNT[which(data_set$exp_CRDT_CARD_CNT=="Inf")]<-1000000 # max=994001



# data_set$CRMM_OVDU_AMT
# table(data_set$TARGET,data_set$CRMM_OVDU_AMT)
# prop.table(table(data_set$TARGET,data_set$CRMM_OVDU_AMT),2)
# plot(prop.table(table(data_set$TARGET,data_set$GDINS_MON_PREM),2))
# 
# data_set$CRMM_OVDU_AMT_bin = ifelse(data_set$CRMM_OVDU_AMT>0,1,0)

for (j in c(1:40)){
set.seed(j+999) #reproducability setting
#data_set$TARGET<-as.factor(data_set$TARGET)
data_set$TARGET<-as.numeric(data_set$TARGET)
intrain<-createDataPartition(y=data_set$TARGET, p=0.7, list=FALSE) 
train<-data_set[intrain,-c(1)]
valid<-data_set[-intrain,-c(1)]

##GBM

gbm_train=gbm(TARGET~.,data=train,distribution = "bernoulli",n.trees=350,shrinkage = 0.05,interaction.depth = 10,verbose = T)
#n.tree=gbm.perf(gbm_train,method="OOB")
#n.tree
gbm.pred=predict(gbm_train,valid,n.trees=350,num.threads=2)

outcome=data.frame()
for (i in c(1:100)) {
  gbm_boolean=ifelse(gbm.pred>2.5-i/20,0,1)
  actual_target=1-as.numeric(valid$TARGET)
  gbm_confus<-confusionMatrix(actual_target,gbm_boolean)
  res=data.frame(cut.off=2.5-i/20,Pre=gbm_confus$byClass[5],Rec=gbm_confus$byClass[6],F1=gbm_confus$byClass[7])
  outcome=rbind(outcome,res)
}
#rltt<-data.frame()
best.cut<-2.5-(which(outcome$F1==max(outcome$F1)))/20
resss2<-ifelse(gbm.pred>best.cut,1,0)
resss3<-prop.table(table(resss2))[2]
resss4<-cbind(j+999,best.cut,resss3)
rltt<-rbind(rltt,resss4)
}
rltt

summary(gbm_train)

gbm_boolean2=ifelse(gbm.pred>-1.5,1,0)
prop.table(table(gbm_boolean2))
table(gbm_boolean2)


gbm_boolean3=ifelse(gbm.pred>-1.2,1,0)
prop.table(table(gbm_boolean3))



gbm_boolean=ifelse(gbm.pred>-1.6,1,0)
gbm_boolean
valid2<-valid
valid2$TARGET2<-gbm_boolean
error_case<-valid2[which(valid2$TARGET!=valid2$TARGET2)]
table(error_case$CRDT_CARD_CNT,error_case$TARGET)
table(valid$CRDT_CARD_CNT,valid$TARGET)
table(error_case$TARGET)
write.csv(error_case,"rs1.csv")

error_case2<-error_case[,-84]
gbm.pred=predict(gbm_train,error_case2,n.trees=350,num.threads=2)

outcome2=data.frame()
for (i in c(1:100)) {
  gbm_boolean=ifelse(gbm.pred>-3-i/20,0,1)
  actual_target=1-as.numeric(error_case2$TARGET)
  gbm_confus<-confusionMatrix(actual_target,gbm_boolean)
  res=data.frame(cut.off=-3-i/20,Pre=gbm_confus$byClass[5],Rec=gbm_confus$byClass[6],F1=gbm_confus$byClass[7])
  outcome2=rbind(outcome2,res)
}
outcome2

