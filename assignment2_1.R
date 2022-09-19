library(forecast)
library(lubridate)
library(tseries)

# read data and create time series
# y is the snow depth in meters and 
# x is the precipitation in mm
y <- read.table("storliensnow.dat")$V1
x <- read.table("storlienprec.dat")$V1
y <- ts(y,frequency = 365, start = decimal_date(ymd("2021-12-2")))
x <- ts(x,frequency = 365, start = decimal_date(ymd("2021-12-2")))

# first plot
autoplot(y,xlab="Time",ylab="(y) Snow depth in meters")
autoplot(x,xlab="Time",ylab="(x) Precipitation in mm")

# stationary test x
adf.test(x, alternative = c("stationary", "explosive"),
         k = 0)
kpss.test(x)

# stationary test y
adf.test(y, alternative = c("stationary", "explosive"),
         k = 0)
kpss.test(y)

# Create zero mean time series for x and y
x <- x - mean(x)
y <- diff(y)
# ACF and PACF 
par(mfrow=c(1,2));acf(x[1:length(x)], main="");pacf(x[1:length(x)],main="")

# ARIMA model
x_model <- arima(x, order=c(0,0,1), include.mean = FALSE)

# diagnostics
tsdiag(x_model)

# Prewhitening of input and output
theta1 <- as.numeric(x_model$coef[1])
ytilde <- filter(y, filter = c(1,-theta1),
                 method = "c", sides = 1)
w <- filter(x, filter = c(1,-theta1),
            method = "c", sides = 1)

#CCF
n <- length(y)
ytilde1 <- ytilde[seq(2,n)]
w1 <- w[seq(2,n)]
par(mfrow=c(1,1))
ccf(ytilde1,w1,ylab="CCF")

# Estimated regression
y0 <- y[seq(4,n)]
x0 <- x[seq(4,n)]
y2 <- y[seq(2,n-2)]
y3 <- y[seq(1,n-3)]

r <- lm(y0 ~ x0+y2+y3-1);summary(r)

u <- r$res
omega1 <- as.numeric(r$coef[2])
omega2 <- as.numeric(r$coef[3])
eta <- filter(u, filter = c(0,omega1,omega2), method = "recursive")
plot(eta, type = "l")
par(mfrow = c(1,2));acf(eta);pacf(eta)

# eta model
m <- arima(eta, order = c(2,0,0), include.mean=FALSE)

par(mfrow = c(1,2));acf(m$residuals);pacf(m$residuals)

hist(m$res)
qqnorm(m$res)

tsdiag(m)


library(MASS)
#Fit the full model
full.model <-lm(y0~y1+y2+y3+y4+y5+x0+x1+x2+x3+x4+x5-1)
# Stepwise regression model
#
step.model <- stepAIC(full.model , direction = "both",trace = FALSE)

summary(step.model)

