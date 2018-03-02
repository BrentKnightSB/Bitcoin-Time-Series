
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
newcrypto <- subset(crypto, date > "2017-04-01")

#Plotting
ts.plot(data=newcrypto)



