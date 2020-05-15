library(forecast) #model building
library(tseries) #timeseries 
library(ggplot2) #data vizualisation
library(dygraphs) #interactive graph
library(tidyr) #data cleaning


#Checking class of V2 vector
class(revenue$V2)

#data cleaning

revenue <- separate(revenue, col = V2,
                    sep= c(2,-3), into = c('rest','data','rest2'))
head(revenue)
class(revenue$data)
#still character


#Time series conversion
 myts = ts(as.numeric(revenue$data), start = 1998, frequency = 12)
 myts
 plot(myts) # we see some gaps in our trend as there are NAs. There are outliers as well
 
 summary(myts)
 
 #all in one cleaning tool in forecast package
 
 myts <- tsclean(myts)
 summary(myts)
 myts 
 'we can see our dataset has no outliers and NA values'
 
 plot(myts)
'we observe we see clear seaonality and upward trend'

#EDA
 plot(decompose(myts))

 ggmonthplot(myts)
 
 ggseasonplot(myts)
 
#setting neural network model
 model <- nnetar(myts)
 
 #forecasting
  nnetforecast <- forecast(model, h=36, PI = T) #PI = Prediction Intervals
  autoplot(nnetforecast)

#Interactive graphs
  
  #creatiing dataframe
  data <- nnetforecast$x
  lower <- nnetforecast$lower[,2] #95% lower confidence predicted values 
  upper <- nnetforecast$upper[,2] #95% upper confidence predicted values
  pforecast <- nnetforecast$mean #predicted values
  
  mydata <- cbind(data,lower,upper,pforecast)
  rbind(head(mydata),tail(mydata))
  mydata
  # mn = mean(mydata[,1], na.rm = T)
  # std = sd(mydata[,1], na.rm = T)
  
  
  #reet zoom plugin
  dyUnzoom <-function(dygraph) {
    dyPlugin(
      dygraph = dygraph,
      name = "Unzoom",
      path = system.file("plugins/unzoom.js", package = "dygraphs")
    )
  }
  
  
  dygraph(mydata, main = 'Beach Restaurant') %>% 
    dyRangeSelector() %>%  #allows user to zoom in for fraction of ts
    dyUnzoom() %>% 
    dySeries(name = 'data', label = 'Revenue Data') %>% #adding data
    dySeries(name = c('lower','pforecast','upper'), label = 'Revenue Forecast') %>% 
    dyLegend(show = 'always',
             hideOnMouseOut = FALSE) %>% #always show legend even if mouse is outside area
    dyAxis('y', label = 'Monthly Revenue USD', drawGrid = T) %>% 
    dyAxis('x', drawGrid = T) %>% 
    dyHighlight(highlightCircleSize = 5, #describes what happens on mouse hovering
                highlightSeriesOpts = list(strokeWidth =2)) %>% 
    # dyShading(from = mn - std, to = mn + std, axis = "y") %>% 
    dyOptions(axisLineColor = 'navy', gridLineColor = 'gray', includeZero = T) 
mn  
        