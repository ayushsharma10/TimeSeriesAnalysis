#Generating dataset

set.seed(5)

testts = ts(rnorm(300), start = c(1919,1), frequency = 4)
plot(testts)
testts


library(forecast)

meanmodel <- meanf(testts, h=20) #We aew forcasting 20 quarters. hence, argument h = 20
naivemodel <- naive(testts, h=20)
driftmodel <- rwf(testts, h=20, drift = T)

plot(meanmodel)

class(meanmodel['fitted'])

plot(meanmodel, main ='')
lines(naivemodel$mean,col=123,lwd=2) #lwd - line width
lines(driftmodel$mean,col=22,lwd=2)
legend('topleft', lty = 1, cex = 0.5, col = c(4,123,22),
       legend = c('Mean Method','Naive Method', 'Drift Method'))

# EXPLAINATION
'Our original data is from 1919 to 1993, but as we predicting for 20 quarters, that is for next 5 years,
we can observed how timeline has extended and grey portion of the plot gives forcasted range.'
'Blue line will explain mean of meanmodel - mean of all observation and it is a straight horizontal line
Note: since it is a rnorm data, mean has to be at zero. Thus blue line will always be at zero.
Also, since Rnorm has st. dev of 1, therfored dark gret shade telling the deviation in forcasting will most likely
be between +1 and -1.
Green line will show last observation value as forcasted value hence is is also a horizontal line
Pink line is fircasting value from first and last observation and shows trends of upwards from first to last observation'


#Acurracy

set.seed(95)

myts = ts(rnorm(400), start = c(1919,1), frequency = 4)

#Splitting in train test of 80% train

mytstrain <- window(myts, start = 1919, end = 1999 )

plot(mytstrain)

#model building

meanmodel <- meanf(mytstrain, h=80) #We are forcasting 80 quarters as 20%(test) of 400 is 80
naivemodel <- naive(mytstrain, h=80)
driftmodel <- rwf(mytstrain, h=80)

#extracting test data

mytstest <- window(myts, start = 2000)

accuracy(meanmodel, mytstest)
accuracy(naivemodel, mytstest)
accuracy(driftmodel, mytstest)



#Residuals

set.seed(95)

testts = ts(rnorm(200), start = c(1919,1))

library(forecast)

meanmodel <- meanf(testts, h=20) #We aew forcasting 20 quarters. hence, argument h = 20
naivemodel <- naive(testts, h=20)
driftmodel <- rwf(testts, h=20, drift = T)

# var and mean of mean model
var(meanmodel$residuals)
plot(meanmodel$residuals)
mean(meanmodel$residuals)

#we observe that mean is v.clase to 0 and var is constant
#hence in this case, mean method is probably the best method


# NAIVE AND DRIFT METHOD
#We need to remove first NA value in fitted as both of these models needs atleast one value to start with

naivwithoutNA <- naivemodel$residuals[2:200]

var(naivwithoutNA)
plot(naivwithoutNA, type = 'b')
mean(naivwithoutNA)

#variance is off from 1 as that of our original dataset 
#mean is slightly off than 0 value as this model depends on the last observation

driftwithoutNA <- driftmodel$residuals[2:200]

var(driftwithoutNA)
plot(driftwithoutNA, type = 'b')
mean(driftwithoutNA)

#We see variance is away from 1


#histogram of distribution
hist(meanmodel$residuals)
#residuals are almost normally distributed
shapiro.test(meanmodel$residuals)
#Shapiro test says residuals are not normal

acf(meanmodel$residuals)
#we have no autocorrelation 

#Overall mean model is quite a good fit for overall dataset of random numbers