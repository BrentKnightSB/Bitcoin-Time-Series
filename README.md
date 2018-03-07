# PSTAT 174 FINAL PROJECT 

## Progress 3/1/17
Cleaned up data by removing all categories but date and closing price. Cut off all dates before March 30th, and converted set into weekly price rather than daily price. Weekly price consists of 7 days starting from the week of March 30th and is the mean of 7 days.

## Progress 3/6/17
Redid data set to count days starting from March 30th, 2017 to February 22nd, 2018. Applied box cox transformation and differenced the time series at 1 lag to create a stationary time series. Did some preemptive modeling and proposed either a MA(1) model or ARIMA(17,1,0) model 
# TO DO
Run data set through a TA to see whether our intuition is right. Needs to perform differencing and other diagnostic checks to make it stationary. Once that's done we select the model and forecast (Easier said than done) 
