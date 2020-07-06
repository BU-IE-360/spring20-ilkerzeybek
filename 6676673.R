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

data6676673 <- read.csv("3904356.csv")
data6676673 = data[visit_count > 0]
acf(data6676673$sold_count)
decomposed_ts = decompose(ts(data6676673$sold_count, freq=7))
plot(decomposed_ts)

data6676673[,lag2_forecast:=shift(sold_count,2)]
data6676673[,lag7_forecast:=shift(sold_count,7)]

#Linear Regression Model with Lags (somewhat autoregressive)
lr_model = lm(sold_count ~ lag2_forecast+lag7_forecast,data6676673)
summary(lr_model)
data6676673[,lr_forecast:=predict(lr_model,filtered_data)]

##
ts6676673 <- ts(data6676673[, 5], start = c(2019, 04, 30), end = c(2020, 05, 31), frequency = 365)

split_data <- ts_split(ts6676673, sample.out = 75)
training6676673 <- split_data$train
test6676673 <- split_data$test

fit <- auto.arima(ts32939029, D=1)
fit
fit2 <- auto.arima(training7061886)
fit2
mymodel <- Arima(training6676673, order = c(1,0,0))
on_test_model <- Arima(test6676673, model = mymodel )
myforecast <- forecast(on_test_model, level = 95, h = 1)

plot(myforecast)
myforecast
accuracy(myforecast)


