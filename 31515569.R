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

data31515569 <- read.csv("3904356.csv")
data31515569 = data[visit_count > 0]
acf(data31515569$sold_count)
decomposed_ts = decompose(ts(data31515569$sold_count, freq=7))
plot(decomposed_ts)

data31515569[,lag2_forecast:=shift(sold_count,2)]
data31515569[,lag7_forecast:=shift(sold_count,7)]

#Linear Regression Model with Lags (somewhat autoregressive)
lr_model = lm(sold_count ~ lag2_forecast+lag7_forecast,data31515569)
summary(lr_model)
data31515569[,lr_forecast:=predict(lr_model,filtered_data)]

##
ts31515569 <- ts(data31515569[, 5], start = c(2019, 04, 30), end = c(2020, 05, 31), frequency = 365)

split_data <- ts_split(ts31515569, sample.out = 75)
training31515569 <- split_data$train
test31515569 <- split_data$test

fit <- auto.arima(ts7061886, D=1)
fit
fit2 <- auto.arima(training7061886)
fit2
mymodel <- Arima(training31515569, order = c(1,0,0))
on_test_model <- Arima(test31515569, model = mymodel )
myforecast <- forecast(on_test_model, level = 95, h = 1)

plot(myforecast)
myforecast
accuracy(myforecast)




