#### ARIMA MODEL

#Dataset

plot(lynx)

acf(lynx) ; pacf(lynx)

library(forecast)

myar_base = auto.arima(lynx)

#refining settings
auto.arima(lynx, trace = T)

myar = auto.arima(lynx, stepwise = F, approximation = F)
myar

plot(myar)

plot(forecast(myar, h=3))
plot(forecast(myar_base, h=3))

#how model looks compared to original values

plot(lynx, lwd = 3, col = 'red')
lines(myar$fitted, col = 'blue')

#### EXPONENTIAL SMOOTHING

#Dataset

plot(nottem)
etsmodel = ets(nottem); etsmodel

#first line of result tells model type itslef - here it is additive error, No trend and additive seasonality

#comparing how model looks compared to original data
plot(nottem, lwd = 3, col = 'red')
lines(etsmodel$fitted, col = 'blue')

plot(forecast(etsmodel, h=17)) #dark grey shade is 85% accuracy range and light grey is 95% accuracy range

plot(forecast(etsmodel, h=17, level = 95)) #now we get only one shade of 95% CI

# manually setting ets model
#changing erro and sesonality to multiplicative

etsmodmult = ets(y = nottem, model='MNM')
etsmodmult

plot(nottem, lwd = 3, col = 'red')
lines(etsmodmult$fitted, col = 'blue')


plot(forecast(etsmodel, h=17), lwd=2, col = 'red')
plot(forecast(etsmodmult, h=17))

