library(readr)
library(ggplot2)
library(forecast)
library(fpp2)
library(TTR)
library(dplyr)
library(caTools)
library(lubridate)
library(TSstudio)
getwd()
setwd("C:/Users/ilker zeybek/Desktop/360 proje")
mape <- function(actual,pred){
  mape <- mean(abs((actual - pred)/actual))*100
  return (mape)
}
data32939029 = read.csv("32939029.csv")
glimpse(data32939029)
set.seed(360)
ts32939029 <- ts(data32939029[, 5], start = c(2019, 04, 30), end = c(2020, 05, 31), frequency = 365)
split_data <- ts_split(ts32939029, sample.out = 75)
training32939029 <- split_data$train
test32939029 <- split_data$test
nrow(train32939029)
nrow(test32939029)
ts32939029 <- ts(train32939029[, 5], start = c(2019, 11, 23), end = c(2020, 05, 13), frequency = 365)
se_model <- ses(ts32939029, h = 1)
summary(se_model)
df_fc = as.data.frame(se_model)
test32939029$simplexp = df_fc$`Point Forecast`
mape(test32939029$sold_count, test32939029$simplexp)
holt_model <- holt(ts32939029, h = 1)
summary(holt_model)
df_holt = as.data.frame(holt_model)
test32939029$holt = df_holt$`Point Forecast`
mape(test32939029$sold_count, test32939029$holt)

set.seed(360)
data31515569 = read.csv("31515569.csv")
ts331515569 <- ts(data31515569[, 5], start = c(2019, 04, 30), end = c(2020, 05, 31), frequency = 365)
split_data <- ts_split(ts31515569, sample.out = 75)
training31515569 <- split_data$train
test31515569 <- split_data$test
nrow(train31515569)
nrow(test31515569)
ts31515569 <- ts(train31515569[, 5], start = c(2019, 09, 27), end = c(2020, 05, 13), frequency = 365)
se_model2 <- ses(ts31515569, h = 1)
summary(se_model2)
df_fc2 = as.data.frame(se_model2)
test31515569$simplexp = df_fc2$`Point Forecast`
mape(test31515569$sold_count, test31515569$simplexp)
holt_model2 <- holt(ts31515569, h = 1)
summary(holt_model2)
df_holt2 = as.data.frame(holt_model2)
test31515569$holt2 = df_holt2$`Point Forecast`
mape(test31515569$sold_count, test31515569$holt)

set.seed(360)
data7061886 = read.csv("7061886.csv")
ts7061886 <- ts(data7061886[, 5], start = c(2019, 04, 30), end = c(2020, 05, 31), frequency = 365)
split_data <- ts_split(ts7061886, sample.out = 75)
training7061886 <- split_data$train
test7061886 <- split_data$test
nrow(train7061886)
nrow(test7061886)
ts7061886 <- ts(train7061886[, 5], start = c(2019, 07, 27), end = c(2020, 05, 13), frequency = 365)
se_model3 <- ses(ts7061886, h = 1)
summary(se_model3)
df_fc3 = as.data.frame(se_model3)
test7061886$simplexp = df_fc3$`Point Forecast`
mape(test7061886$sold_count, test7061886$simplexp)
holt_model3 <- holt(ts7061886, h = 1)
summary(holt_model3)
df_holt3 = as.data.frame(holt_model3)
test7061886$holt3 = df_holt3$`Point Forecast`
mape(test7061886$sold_count, test7061886$holt)

set.seed(360)
data6676673 = read.csv("6676673.csv")
ts6676673 <- ts(data6676673[, 5], start = c(2019, 04, 30), end = c(2020, 05, 31), frequency = 365)
split_data <- ts_split(ts6676673, sample.out = 75)
training6676673 <- split_data$train
test6676673 <- split_data$test
nrow(train6676673)
nrow(test6676673)
ts6676673 <- ts(train6676673[, 5], start = c(2019, 01, 23), end = c(2020, 05, 13), frequency = 365)
se_model4 <- ses(ts6676673, h = 1)
summary(se_model4)
df_fc4 = as.data.frame(se_model4)
test6676673$simplexp = df_fc4$`Point Forecast`
mape(test6676673$sold_count, test6676673$simplexp)
holt_model4 <- holt(ts6676673, h = 1)
summary(holt_model4)
df_holt4 = as.data.frame(holt_model4)
test6676673$holt4 = df_holt4$`Point Forecast`
mape(test6676673$sold_count, test6676673$holt)

set.seed(360)
data5926527 = read.csv("5926527.csv")
ts5926527 <- ts(data5926527[, 5], start = c(2019, 04, 30), end = c(2020, 05, 31), frequency = 365)
split_data <- ts_split(ts5926527, sample.out = 75)
training5926527 <- split_data$train
test5926527 <- split_data$test
nrow(train5926527)
nrow(test5926527)
ts5926527 <- ts(train5926527[, 5], start = c(2019, 04, 30), end = c(2020, 05, 13), frequency = 365)
se_model5 <- ses(ts5926527, h = 1)
summary(se_model5)
df_fc5 = as.data.frame(se_model5)
test5926527$simplexp = df_fc5$`Point Forecast`
mape(test5926527$sold_count, test5926527$simplexp)
holt_model5 <- holt(ts5926527, h = 1)
summary(holt_model5)
df_holt5 = as.data.frame(holt_model5)
test5926527$holt5 = df_holt5$`Point Forecast`
mape(test5926527$sold_count, test5926527$holt)

set.seed(360)
data4066298 = read.csv("4066298.csv")
ts4066298 <- ts(data4066298[, 5], start = c(2019, 04, 30), end = c(2020, 05, 31), frequency = 365)
split_data <- ts_split(ts4066298, sample.out = 75)
training4066298 <- split_data$train
test4066298 <- split_data$test
nrow(train4066298)
nrow(test4066298)
ts4066298 <- ts(train4066298[, 5], start = c(2019, 09, 09), end = c(2020, 05, 13), frequency = 365)
se_model6 <- ses(ts4066298, h = 1)
summary(se_model6)
df_fc6 = as.data.frame(se_model6)
test4066298$simplexp = df_fc6$`Point Forecast`
mape(test4066298$sold_count, test4066298$simplexp)
holt_model6 <- holt(ts4066298, h = 1)
summary(holt_model6)
df_holt6 = as.data.frame(holt_model6)
test4066298$holt6 = df_holt6$`Point Forecast`
mape(test4066298$sold_count, test4066298$holt)

set.seed(360)
data3904356 = read.csv("3904356.csv")
ts3904356  <- ts(data3904356 [, 5], start = c(2019, 04, 30), end = c(2020, 05, 31), frequency = 365)
split_data <- ts_split(ts3904356 , sample.out = 75)
training3904356  <- split_data$train
test3904356 <- split_data$test
nrow(train3904356)
nrow(test3904356)
ts3904356 <- ts(train3904356[, 5], start = c(2019, 04, 30), end = c(2020, 05, 13), frequency = 365)
se_model7 <- ses(ts3904356, h = 1)
summary(se_model7)
df_fc7 = as.data.frame(se_model7)
test3904356$simplexp = df_fc7$`Point Forecast`
mape(test3904356$sold_count, test3904356$simplexp)
holt_model7 <- holt(ts3904356, h = 1)
summary(holt_model7)
df_holt7 = as.data.frame(holt_model7)
test3904356$holt7 = df_holt7$`Point Forecast`
mape(test3904356$sold_count, test3904356$holt)

set.seed(360)
data85004 = read.csv("85004.csv")
ts85004  <- ts(data85004 [, 5], start = c(2019, 04, 30), end = c(2020, 05, 31), frequency = 365)
split_data <- ts_split(ts85004 , sample.out = 75)
training85004  <- split_data$train
test85004 <- split_data$test
nrow(train85004)
nrow(test85004)
ts85004 <- ts(train85004[, 5], start = c(2019, 04, 30), end = c(2020, 05, 13), frequency = 365)
se_model8 <- ses(ts85004, h = 1)
summary(se_model8)
df_fc8 = as.data.frame(se_model8)
test85004$simplexp = df_fc8$`Point Forecast`
mape(test85004$sold_count, test85004$simplexp)
holt_model8 <- holt(ts85004, h = 1)
summary(holt_model8)
df_holt8 = as.data.frame(holt_model8)
test85004$holt8 = df_holt8$`Point Forecast`
mape(test85004$sold_count, test85004$holt)





