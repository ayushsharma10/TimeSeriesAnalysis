getwd()
setwd('A:/R Programming/Time Series Analysis/')
getwd()

install.packages('forecast')
install.packages('tseries')
install.packages('dygraph')


lynx
#dataset
head(lynx)

#Getting time stamp of data
time(lynx)

#Line graph for data
plot(lynx)

#quantiles
quantile(lynx)

#deciles
quantile(lynx, probs = seq(0,1,0.1), type = 5)

seq(0,1,length.out = 11)

#sort
sort(lynx)




#Creating own timeseries data

data <- runif(50,10,45)
plot(data,type = 'l')

#packing this vector is timeseries - quarterly

timeseries <- ts(data = data, start = c(1956,3), frequency = 4)
plot(timeseries)
timeseries
time(timeseries)
class(timeseries)

#how a multivar timeseries looks
class(EuStockMarkets) ; head(EuStockMarkets)


# DATA VIZUALISATION

#Using nottem dataset

plot(nottem)
plot(decompose(nottem))

# for unit vector using plot.ts()

plot.ts(cumsum(rnorm(500)))


#using forcast library for seasonality plot

library(forecast)

ggseasonplot(nottem)
ggmonthplot(nottem)
ggsubseriesplot(nottem)
ggtsdisplay(nottem)
            
            
#Stationarity)

test = ts(c(rnorm(100,2,1),rnorm(100,50,1)))
plot(test)

#We see that date has non-constant mean - non - stationary data

plot(diff(test)) #diff get difference between consecutive values
#we now observed that data is not stationary.
#there is one spike which more advance models can deal with. Very rarely second differencing is required.

plot(diff(diff(test))) #Second difference - This created two spikes which cancel outs each other hence const mean



#Unit root testing

x = rnorm(100)
library(tseries)
adf.test(x)
adf.test(diff(test))


#checking stationary test for nottem
adf.test(nottem)


## Auto - Correlation
#Lynx
plot(lynx)

acf(lynx, lag.max = 20)
#we can see there are several bars ranging outside 95% confidence interval

pacf(lynx, lag.max = 20)

#Nottem
pacf(nottem, lag.max = 20)

