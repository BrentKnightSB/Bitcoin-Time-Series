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

plot(crypto)

#BoxCox Transform
time <- 1:length(crypto[,2])
fit <- lm(crypto[,2] ~ time)
boxcoxtransform <- boxcox(crypto[,2] ~ time, plotit = T)
lamb <- boxcoxtransform$x[which(boxcoxtransform$y == max(boxcoxtransform$y))]
bitcoinboxcox <- (1/lamb)*(crypto[,2]^lamb-1)
ts.plot(bitcoinboxcox)

#Differencing
bitcoin.diff <- diff(bitcoinboxcox,1)
ts.plot(bitcoin.diff, main = "De-trended data")


#PLotting ACF/PACF
op <- par(mfrow=c(2,1)) 
acf(bitcoin.diff) 
pacf(bitcoin.diff)
par(op)


#Possible Fits:
#MA(1) Differences = 1
fit = arima(bitcoin.diff, order=c(3,0,1), method="ML")

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


#Testing for independence of residuals
Box.test(resid(fit), type="Ljung")

#Test for normality of residuals
shapiro.test(residuals(fit))

