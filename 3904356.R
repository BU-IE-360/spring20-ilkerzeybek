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

data3904356 <- read.csv("3904356.csv")
data3904356 = data[visit_count > 0]
acf(data3904356$sold_count)
decomposed_ts = decompose(ts(data3904356$sold_count, freq=7))
plot(decomposed_ts)

data3904356[,lag2_forecast:=shift(sold_count,2)]
data3904356[,lag7_forecast:=shift(sold_count,7)]

#Linear Regression Model with Lags (somewhat autoregressive)
lr_model = lm(sold_count ~ lag2_forecast+lag7_forecast,data3904356)
summary(lr_model)
data3904356[,lr_forecast:=predict(lr_model,filtered_data)]

##
ts3904356 <- ts(data3904356[, 5], start = c(2019, 04, 30), end = c(2020, 05, 31), frequency = 365)

split_data <- ts_split(ts3904356, sample.out = 75)
training3904356 <- split_data$train
test3904356 <- split_data$test

fit <- auto.arima(ts3904356, D=1)
fit
fit2 <- auto.arima(training7061886)
fit2
mymodel <- Arima(training3904356, order = c(0,0,0))
on_test_model <- Arima(test3904356, model = mymodel )
myforecast <- forecast(on_test_model, level = 95, h = 1)

plot(myforecast)
myforecast
accuracy(myforecast)


