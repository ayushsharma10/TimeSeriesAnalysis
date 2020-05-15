getwd()
setwd('A:/R Programming/Time Series Analysis/')

library(forecast)
library(tseries)
library(dygraphs)


spain = ts(spain$x, start = 1980)


str(spain)
plot(spain, ylab = 'Labour Force Rate (Age-25-54')

#Exponential Smoothing

#Since dataset has trend with no seasonality, we go for Holt Linear Trend method

#HOLT EXPONENTIAL SMOOTHING

holtrend = holt(spain, h=10)
summary(holtrend) #gives result of model
plot(holtrend)

#Damped trend

#Auto phi generation
holtdamped = holt(spain, h=10, damped = T, phi = 0.8)
plot(holt(spain, h=15, damped = T))
summary(holt(spain, h=15, damped = T))
#when auto generated, phi was set to 0.98

#manual phi generation
plot(holt(spain, h=15, damped = T, phi = 0.87))
summary(holt(spain, h=15, damped = T, phi=0.8))


#ARIMA MODELLING

ggtsdisplay(spain) #taking look at acf anf pacf plots
#AR(1) model is also preffered solution using ACF and PACF - Statistically

spainarima = auto.arima(spain, stepwise = F, approximation = F ) 
summary(spainarima)
arimafore = forecast(spainarima, h=10)
#The fact that we have drift present means we have a constant to our model
 
plot(forecast(spainarima, h=5))

#This model will work fine for short span as it does not take damping in consideration 



#Comparing multiple models - vizualisation
library(ggplot2)


#install.packages("extrafont")
library(extrafont)
font_import() #addiming more fonts to database
loadfonts(device = "win") #initialising

autoplot(spain) +
  forecast::autolayer(holtrend$mean, series = 'Holt Linear Trend')+
  forecast::autolayer(holtdamped$mean, series = 'Holt Damped Trend')+
  forecast::autolayer(arimafore$mean, series = 'ARIMA')+
  xlab('Years')+
  ylab('Labour Force Rate (Age-25-54)')+
  guides(colour = guide_legend(title = 'Forecast Method'))+
  theme(legend.position = c(0.9,0.2))+
  ggtitle('Model Comparision')+
  theme_minimal()+
  theme(plot.title = element_text(family = 'Verdana', hjust = 0.5, color = 'dark blue',
                                  face = 'bold', size = 15))


#windowsFonts() - displays fonts in R database
