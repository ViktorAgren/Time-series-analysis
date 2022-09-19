library(forecast)
x<-read.table("carsmon752203.dat")$V1
timeseries<-ts(x, frequency=12,start=c(1975,1))

# (a)
autoplot(timeseries,ylab = "Number of cars in traffic in Sweden")

# (b)
spec.pgram(timeseries)
spec.pgram(timeseries,spans = 2)
spec.pgram(timeseries,spans = 8)
spec.pgram(timeseries,spans = 10)
spec.pgram(timeseries,spans = 12)
spec.pgram(timeseries,spans = 14)
spec.pgram(timeseries,spans = 24)

spec.ar(timeseries)

# (c)
timeseries_diff<-diff(timeseries)
autoplot(timeseries_diff,ylab = "Number of cars in traffic in Sweden")

spec.pgram(timeseries_diff)
spec.pgram(timeseries_diff,spans = 2)
spec.pgram(timeseries_diff,spans = 8)
spec.pgram(timeseries_diff,spans = 10)
spec.pgram(timeseries_diff,spans = 12)
spec.pgram(timeseries_diff,spans = 14)
spec.pgram(timeseries_diff,spans = 24)

spec.ar(timeseries_diff)
# (d)
moving_average<-ma(timeseries,order=12)
moving_average<-na.omit(moving_average)
autoplot(moving_average,ylab = "Number of cars in traffic in Sweden")

spec.pgram(moving_average)
spec.pgram(moving_average,spans = 2)
spec.pgram(moving_average,spans = 8)
spec.pgram(moving_average,spans = 10)
spec.pgram(moving_average,spans = 12)
spec.pgram(moving_average,spans = 14)
spec.pgram(moving_average,spans = 24)

spec.ar(moving_average)
