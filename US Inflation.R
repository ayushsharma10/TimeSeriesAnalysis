library(forecast)
library(tseries)
library(ggplot2)
library(dygraphs)

#Source : Statbureau.org

inflation <- ts(inflation$x, start=2003, frequency = 12)
plot(inflation)

mean(inflation)
'We see downward spike in 2008-2009 which is due to 2008 financial crisis'

#Decomposing plot
plot(decompose(inflation))

#stationarity check
adf.test(inflation)

#STL decomposition - better alternative than above

plot(stl(inflation, s.window = 7))

#STL forcasting
plot(stlf(inflation, method = 'ets'))
'As we did mention h, R will use h as double the frequency of ts. here 2*12(months) = 24 periods'
'i.e. it forecast for 2 seasonal cycles'

#Comparision with a standard ets plot
autoplot(forecast(ets(inflation), h=24))


#AMRIMA for Seasonality

auto.arima(inflation, stepwise = F, approximation = F, trace = T)

#forecasting using ARIMA
plot(forecast(auto.arima(inflation, stepwise = F, approximation = F)))

'We see seasonality is not captured by arima and seasonal high and lows are conservatively estimated'

'In situations like this where forcast of seasonaluity is not good, we have a general backup plan '

#Using SARIMA CATCHALL method
catchall = arima(inflation, order = c(0,1,1), seasonal = c(0,1,1)) #we used arima and not auto arime
catchall$aic
#forcasting
plot(forecast(catchall))




#Exponential Smoothing


#holt-winters method
plot(forecast(hw(inflation), h=24))

#ETS
plot(forecast(ets(inflation), h=24))

'We see that we have 3 models giving approximately the same results- ETS, HW and SARIMA(0,1,1)(0,1,1)'

#monthplot
ggmonthplot(inflation)

#seasonplot
ggseasonplot(inflation)
