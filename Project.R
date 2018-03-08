library(tidyverse)
library(ggplot2)
library(MASS)
library(qpcR)
library(forecast)
#For macs download https://www.xquartz.org/
library(tseries)

##Setup

#Reading in data
data <- read.csv("crypto.csv", header=TRUE)

#Choosing only Bitcoin
newdata <- subset(data, symbol=="BTC")

#Choosing the two variables
myvars <- c("date","close")
crypto <- newdata[myvars]

#Changing class of "date"
crypto$date <- as.Date(crypto$date)

#Cutting off dates before 4/1/17
bitcoin <- subset(crypto, date > "2017-03-29")

#Testing stuff
bitcoin2 <- ts(bitcoin, frequency =  7)

##Transformations

#BoxCox Transform
time <- 1:length(r.bitcoin[,2])
fit <- lm(r.bitcoin[,2] ~ time)
boxcoxtransform <- boxcox(r.bitcoin[,2] ~ time, plotit = T)
lamb <- boxcoxtransform$x[which(boxcoxtransform$y == max(boxcoxtransform$y))]
bitcoinboxcox <- (1/lamb)*(r.bitcoin[,2]^lamb-1)
ts.plot(bitcoinboxcox)

#Differencing
bitcoin.diff <- diff(bitcoinboxcox,1)
ts.plot(bitcoin.diff, main = "De-trended data")


#PLotting ACF/PACF
op <- par(mfrow=c(2,1)) 
acf(bitcoin.diff) 
pacf(bitcoin.diff)
par(op)

#PLotting ACF/PACF
op <- par(mfrow=c(2,1)) 
acf(bitcoinboxcox) 
pacf(bitcoinboxcox)
par(op)

# Calculate AICc for ARMA models with p and q running from 0 to 5 (Trying to find a model through brute force) 
aiccs <- matrix(NA, nr = 6, nc = 6)
dimnames(aiccs) = list(p=0:5, q=0:5)
for(p in 0:5)
{
  for(q in 0:5) {
    aiccs[p+1,q+1] = AICc(arima(bitcoin.diff, order = c(p,0,q), method="ML")) }
}

aiccs
(aiccs==min(aiccs))

#Check for stationarity
adf.test(bitcoin.diff)

##Diagnostics

#Possible Fits:
#MA(1) Differences = 1
fit = arima(bitcoinboxcox, order=c(0,1,1), method="ML")

#MA(1) Difference = 2
fit_MA1D2 = arima(bitcoinboxcox, order=c(1,2,0), method="ML")

#AR(1) Differences = 1
fit_AR1 = arima(bitcoinboxcox, order=c(1,1,0), method="ML")





#Testing for independence of residuals
Box.test(resid(fit), type="Ljung")

#Test for normality of residuals
shapiro.test(residuals(fit))

#Plotting Residuals of Fit
ts.plot(residuals(fit),main = "Fitted Residuals")


par(mfrow=c(1,2),oma=c(0,0,2,0)) 
# Plot diagnostics of residuals 
op <- par(mfrow=c(2,2))
# acf
acf(residuals(fit),main = "Autocorrelation")
# pacf
pacf(residuals(fit),main = "Partial Autocorrelation")
# Histogram
hist(residuals(fit),main = "Histogram") 
# q-q plot
qqnorm(residuals(fit)) 
qqline(residuals(fit),col ="blue")
# Add overall title
title("Fitted Residuals Diagnostics", outer=TRUE) 
par(op)


##Forecasting on bitcoinboxcox dataset
mypred = predict(fit, n.ahead=100)
ts.plot(bitcoinboxcox, xlim=c(0,429)) 
points(x = 329:428, y = mypred$pred) 
lines(329:428,mypred$pred+1.96*mypred$se,lty=2) 
lines(329:428,mypred$pred-1.96*mypred$se,lty=2)

##Forecasting on bitcoin dataset
mypred = predict(fit, n.ahead=10)
ts.plot(bitcoin, xlim=c(0,340)) 
points(x = 330:339, y = mypred$pred) 
lines(330:339,mypred$pred+1.96*mypred$se,lty=2) 
lines(330:339,mypred$pred-1.96*mypred$se,lty=2)

##Automated Forecast
fcast <- forecast(bitcoin[,2], h=10)
plot(fcast)




############################

##New code used in Office Hours


r.bitcoin = bitcoin[2:nrow(bitcoin),2] / bitcoin[1:(nrow(bitcoin)-1),2]-1

r.crypto = crypto[2:nrow(crypto),2] / crypto[1:(nrow(crypto)-1),2]-1

#BoxCox Transform
time <- 1:length(r.crypto)
fit <- lm(r.crypto ~ time)
boxcoxtransform <- boxcox(r.crypto ~ time, plotit = T)
lamb <- boxcoxtransform$x[which(boxcoxtransform$y == max(boxcoxtransform$y))]
bitcoinboxcox <- (1/lamb)*(r.crypto^lamb-1)
ts.plot(bitcoinboxcox)
