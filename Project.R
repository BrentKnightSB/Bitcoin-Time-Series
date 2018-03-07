library(tidyverse)
library(ggplot2)
library(MASS)
library(qpcR)
#For macs download https://www.xquartz.org/
library(tseries)

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


#BoxCox Transform
time <- 1:length(bitcoin[,2])
fit <- lm(bitcoin[,2] ~ time)
boxcoxtransform <- boxcox(bitcoin[,2] ~ time, plotit = T)
lamb <- boxcoxtransform$x[which(boxcoxtransform$y == max(boxcoxtransform$y))]
bitcoinboxcox <- (1/lamb)*(bitcoin[,2]^lamb-1)
ts.plot(bitcoinboxcox)

#Differencing
bitcoin.diff <- diff(bitcoinboxcox,1)
ts.plot(bitcoin.diff, main = "De-trended data")


#PLotting ACF/PACF
op <- par(mfrow=c(2,1)) 
acf(bitcoin.diff) 
pacf(bitcoin.diff)
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
  
