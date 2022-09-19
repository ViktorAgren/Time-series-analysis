library(forecast)
library(tidyverse)
library(lmtest)
library(tseries)
library(lubridate)
library(dplyr)

# Read data and arrange
df<-read.csv("C:/Users/99vik/Downloads/GOLDNEW.csv")
df<-df %>% arrange(desc(row_number()))

# Change to weekly data (max of each week)
df$week<-floor_date(as.Date(df$Date),"week")
df<-df %>%
  group_by(week) %>%
  summarize(Close = mean(Close.Last))


# Create and plot time series
gold_data<-ts(df$Close,frequency = 52.2,
              start=decimal_date(ymd("2012-04-30")))

# Log of time series
gold_log <- log(gold_data)
autoplot(gold_log)

# ACF and PACF log data
Acf(gold_log, lag.max = 522)
Pacf(gold_log, lag.max = 522)

# ADF and KPSS test log data
adf.test(gold_log, alternative = c("stationary", "explosive"),
         k = 0)
kpss.test(gold_log)

# Difference of lag 1
gold_diff <- diff(gold_log, lag = 1)
autoplot(gold_diff)

# ADF and KPSS test diff data
adf.test(gold_diff, alternative = c("stationary", "explosive"),
         k = 0)
kpss.test(gold_diff)
# ACF and PACF diff data
acf(gold_diff,lag.max = 520)
pacf(gold_diff,lag.max = 520)

# Train data set (90%)
train_data <- gold_log[1:470]

# Creating ARIMA model
arima <- Arima(train_data, order = c(2, 1, 2),include.drift = TRUE)
summary(arima)
checkresiduals(arima)
tsdiag(arima)

# Histogram with normal curve
normal <- seq(min(arima$residuals), max(arima$residuals), length = 600)
fun <- dnorm(normal, mean = mean(arima$residuals),
             sd = sd(arima$residuals))
hist(arima$residuals, prob = TRUE, col = "white",
     ylim = c(0, max(fun)+10),
     main = "Histogram with normal curve",
     breaks = 50)
lines(normal, fun, col = 2, lwd = 2)

# Normal Q-Q plot
qqnorm(arima$residuals)
qqline(arima$residuals, col = "red",
       distribution = qnorm)

# Final forecast
forecast_ori <- forecast(arima, h = 52)
actual <- ts(gold_log)
forecast_ori %>% autoplot() + autolayer(actual)





