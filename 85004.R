library(readr)
library(ggplot2)
library(forecast)
library(fpp2)
library(TTR)
library(dplyr)
library(data.table)
library(lubridate)
library(TSstudio)

getwd()
setwd("C:/Users/ilker zeybek/Desktop/sales data")
set.seed(360)

data85004 <- read.csv("85004.csv")
data85004 = data[visit_count > 0]
acf(data85004$sold_count)
decomposed_ts = decompose(ts(data85004$sold_count, freq=7))
plot(decomposed_ts)

data85004[,lag2_forecast:=shift(sold_count,2)]
data85004[,lag7_forecast:=shift(sold_count,7)]

#Linear Regression Model with Lags (somewhat autoregressive)
lr_model = lm(sold_count ~ lag2_forecast+lag7_forecast,data85004)
summary(lr_model)
data85004[,lr_forecast:=predict(lr_model,filtered_data)]

##
ts85004 <- ts(data85004[, 5], start = c(2019, 04, 30), end = c(2020, 05, 31), frequency = 365)
split_data <- ts_split(ts85004, sample.out = 75)
training85004 <- split_data$train
test85004 <- split_data$test

fit <- auto.arima(ts85004, D=1)
fit
fit2 <- auto.arima(training7061886)
fit2
mymodel <- Arima(training85004, order = c(1,0,0))
on_test_model <- Arima(test85004, model = mymodel )
myforecast <- forecast(on_test_model, level = 95, h = 1)

plot(myforecast)
myforecast
accuracy(myforecast)


