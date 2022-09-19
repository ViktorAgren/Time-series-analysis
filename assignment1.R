library(forecast)
library(lmtest)
library(tseries)
x<-read.table("bransle.dat")$V1 #read data
timeseries<-ts(x, frequency=4,start=c(2009,1)) #create a time series with quarterly data
ggtsdisplay(timeseries) #plot time series, ACF and PACF

lambda<- BoxCox.lambda(timeseries) #boxcox transformation


timeseries.sdiff<-diff(timeseries,lag=4,differences=1)
#remove seasonal trend
ggtsdisplay(timeseries.sdiff)

timeseries.rdiff<-diff(timeseries.sdiff)
#remove regular trend is insignificant
ggtsdisplay(timeseries.rdiff)
qqnorm(m$residuals)

fit1<-Arima(timeseries,order=c(1,0,0)
            ,seasonal=c(2,1,0),lambda=lambda)
ggtsdisplay(fit1$residuals)
summary(fit1)

fit2<-Arima(timeseries,order=c(0,1,1),seasonal=c(0,1,2),lambda=lambda)
ggtsdisplay(fit2$residuals)
summary(fit2)

fit5<-Arima(timeseries,order=c(0,1,1),seasonal=c(2,1,0),lambda=lambda)
ggtsdisplay(fit3$residuals)
summary(fit3$residuals)


autoplot(fit5)
coeftest(fit5)
checkresiduals(fit5)
qqnorm(fit5$residuals)

auto.arima(timeseries, trace=TRUE,approximation = FALSE)

autoplot(timeseries) + autolayer(fit3$fitted
                                 , series="Arima(011)(210)4")




















plot.ts(timeseries,ylab="trädbränsle")
par(mfrow=c(2,1))
acf(data,main="")
pacf(data,main="")
#decomp
timeseriescomponents<-decompose(timeseries)
plot(timeseriescomponents)
#remove seasonal
timeseriesseasonallyadjusted <- timeseries - timeseriescomponents$seasonal
plot(timeseriesseasonallyadjusted)


#auto arima
test=auto.arima(timeseriesseasonallyadjusted,ic="aic",trace=TRUE)
acf(ts(test$residuals))
pacf(ts(test$residuals))

#check residuals
m=arima(timeseriesseasonallyadjusted,order=c(0,0,1))
par(mfrow=c(2,1))
hist(m$residuals)
qqnorm(m$residuals)
tsdiag(m)


## TEST 2
data<-read.table("bransle.dat")$V1
timeseries<-ts(data, frequency=4,start=c(2009,1))
plot.ts(timeseries,ylab="trädbränsle")
#Normalize data (mean zero, sd one)
mu<-mean(timeseries)
sd<-sd(timeseries)
timeseries=(timeseries-mu)/sd
#remove any increasing/decreasing overtime (adf.test(timeseriesdiff))
timeseriesdiff<-diff(timeseries)
plot.ts(timeseriesdiff,ylab="trädbränsle")
