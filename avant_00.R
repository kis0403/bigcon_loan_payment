setwd("C:\\Users\\lifebloom\\Desktop\\loan payment")

#library
library(data.table)  #data loading

#data loading
data_set=fread("challenge_data\\Data_set.csv")

str(data_set)
head(data_set,1)
summary(data_set)
