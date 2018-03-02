
library(tidyverse)
library(ggplot2)


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
newcrypto <- subset(crypto, date > "2017-03-29")

#Plotting
ts.plot(data=newcrypto)
crypto2 = NULL



crypto3 <- newcrypto["close"]
n <- 7
cryptoFinal <- aggregate(newcrypto, list(rep(1:(nrow(newcrypto)%/%n+1),each=n, len=nrow(newcrypto))),mean)[-1]

cryptoFinal$Week <- seq.int(nrow(cryptoFinal))

myvars2 <- c("Week","close")
cryptoFinal2 <- cryptoFinal[myvars2]
ts.plot(data=cryptoFinal2)

