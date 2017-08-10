setwd("C:\\Users\\kis\\Desktop\\weather\\data")

###library###
library(plyr) #load files
library(data.table)
library(ggplot2)
library(dplyr)



##save img###
save.image(file='myEnvironment.RData')

###temp###
##
tfunc<-function(x){
if(x<100){
  temDF$sec <- x
} else if(x<10000){
  temDF$sec = x%%100
  temDF$min = x%/%100
} else {
  temDF$sec = x%%100
  temDF$min = (x%/%100)%%100
  temDF$hr = x%/%10000
}
}

##

######Getting data#######

filenames <- list.files(path="C:\\Users\\kis\\Desktop\\weather\\data\\bicycle")
dataMerge <- data.frame()
for(f in filenames){ 
  ReadInMerge <- fread(file=f, header=T, na.strings="NULL")
  dataMerge <- rbind(dataMerge, ReadInMerge)
  cat(f,"/24","\n")
}

weather_day<-fread("dailyweather2015_2016_day.csv",header = T)

terminal_info<-fread("terminal2016.csv",header=T)

##Missing value 없음

######Feature Engineering#######
names(dataMerge)<- c("binum", "st_ter", "st_date","st_time","en_ter","en_date","en_time")

names(weather_day)<-c("date","avg_temp","rain_hr","rain_amt","avg_wspd","avg_dew","avg_humid","sum_sol_hr","holiday")

names(terminal_info)<-c("ter_id","ter_name","gu","dong","addr","capacity","feas","lat","lng")

#날짜 형식 변환
dataMerge$st_date<-as.Date(as.character(dataMerge$st_date),"%Y%m%d")
dataMerge$en_date<-as.Date(as.character(dataMerge$en_date),"%Y%m%d")
weather_day$date<-as.Date(weather_day$date,"%Y-%m-%d")

#시간 형식 변환
hr<-list()
min<-list()
sec<-list()
for(i in 1:nrow(dataMerge)){
     hr[i] <- dataMerge$st_time[i]%/%10000
     min[i]<-(dataMerge$st_time[i]%/%100)%%100
     sec[i]<-dataMerge$st_time[i]%%100
  cat(i,"/",nrow(dataMerge),"\n")
}

#editing NA
sum(is.na(weather_day$rain_hr))
sum(is.na(weather_day$rain_amt))

for (i in 1:nrow(weather_day)){
  if (is.na(weather_day$rain_hr[i])==TRUE) {
   weather_day$rain_hr[i]<-0
  }
    cat(i,"/",nrow(weather_day),"\n")
}

for (i in 1:nrow(weather_day)){
  if (is.na(weather_day$rain_amt[i])==TRUE) {
    weather_day$rain_amt[i]<-0
  }
  cat(i,"/",nrow(weather_day),"\n")
}
##########EDA##########
head(dataMerge)
head(weather_day)

#daily weather component
plot(weather_day$date,weather_day$avg_temp,type="l",col="steelblue")
plot(weather_day$date,weather_day$rain_amt,type="l",col="steelblue")
plot(weather_day$date,weather_day$avg_wspd,type="l",col="steelblue")
plot(weather_day$date,weather_day$avg_dew,type="l",col="steelblue")
plot(weather_day$date,weather_day$avg_humid,type="l",col="steelblue")

ggplot(weather_day, aes(date, rain_amt)) + geom_col()
ggplot(weather_day, aes(date, rain_hr)) + geom_col()
ggplot(weather_day, aes(date, sum_sol_hr)) + geom_col()


#daily bicycle usage
dailyUsg<-dataMerge %>% count(st_date)
names(dailyUsg)<- c("date","n")
plot(dailyUsg$date,dailyUsg$n,type="l")
ggplot(dailyUsg, aes(date, n)) + geom_col()


#holiday bicycle usage ( 0: working day 1:holiday)
weather_count<-merge(weather_day,dailyUsg,by="date")
holidayUsg<-aggregate(n~holiday,weather_count,sum)
head(holidayUsg)
holidayCnt<-weather_count %>% count(holiday)
holidayRate<-holidayUsg[,2] / holidayCnt[,2]

#bicycle usage by season
usage_spr<-subset(dataMerge,(st_date>="2015-03-01"&st_date<"2015-06-01")|(st_date>="2016-03-01"&st_date<"2016-06-01"))
usage_smr<-subset(dataMerge,(st_date>="2015-06-01"&st_date<"2015-09-01")|(st_date>="2016-06-01"&st_date<"2016-09-01"))
usage_atm<-subset(dataMerge,(st_date>="2015-09-01"&st_date<"2015-12-01")|(st_date>="2016-09-01"&st_date<"2016-12-01"))
usage_wtr<-subset(dataMerge,(st_date<"2015-03-01")|(st_date>="2015-12-01"&st_date<"2016-03-01")|(st_date>="2016-12-01"))

usage_season<-c(nrow(usage_spr),nrow(usage_smr),nrow(usage_atm),nrow(usage_wtr))
usage_season

#bicycle numbers & usage per bicycle
binums<-sort(unique(dataMerge$binum))
usgpb<-dataMerge %>% count(binum)
head(usgpb)
str(binums)
plot(usgpb)

#frequent starting terminal and ending terminal
st_freq<-dataMerge %>% count(st_ter)
names(st_freq)<- c("ter_id","st_n")
en_freq<-dataMerge %>% count(en_ter)
names(en_freq)<- c("ter_id","en_n")
ter_freq<-merge(terminal_info,st_freq)
ter_freq2<-merge(ter_freq,en_freq)

write.csv(ter_freq2[,c(1,8,9,10,11)],"ter_freq.csv")

#frequent path
newPath<-c()
for (i in 1:nrow(dataMerge)){
  newPath[i]<-paste(dataMerge$st_ter[i],dataMerge$en_ter[i])
  cat(i,"/",nrow(dataMerge),"\n")
}


head(ter_freq2)
##newPath변수를 count해서 최빈 경로 탐색+제자리 반환을 하는 경우와 경로 이동을 하는 경우 비교
##경로 이동을 하는 경우 평균적인 이동거리 탐색


##평균, 최대, 최소 이용시간 추적
##경로 이동을 하는 경우 평균 이용 시간 추적
##경로 이동이 길고 이용시간/빈도가 큰 경우 추출


set.seed(4256)
datamer<-sample_n(dataMerge[,-8],1000000,replace=FALSE)

write.csv(datamer,"sampleby.csv")


head(dataMerge)
head(weather_day)
head(terminal_info)


