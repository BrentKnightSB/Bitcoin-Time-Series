library(tidyverse)
library(ggplot2)
library(MASS)


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



