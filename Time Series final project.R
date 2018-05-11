library(sandwich)
library(dynlm)
library(tseries)
library(ggplot2)
library(forecast)
library(car)
#Passenger Injuries and Injury Rates, 1995 through 2014,for U.S. Air Carriers Operating Under 14 CFR 121				

# Predicting the change in passenger serious injury from 2015 until 2020 using information through 2014. 

# Get data for passenger serious injuries convert data to time series 
pas_serious_in=airline_injuries$`Passenger Serious Injuries`[1:32]
pas_serious_in=ts(pas_serious_in,start = 1983, end = 2014, frequency = 1)
lpas_serious_in=log(pas_serious_in)
gpas_serious_in=diff(lpas_serious_in,lag = 1)
gpas_serious_in
#Initial plots and preliminarily analysis of pas_fat and pas_serious_in
ts.plot(pas_serious_in)
summary(pas_serious_in)
hist(pas_serious_in)

# Divide data into training and testing sets
# For training set, I am using data for passenger serious injury from 1983 through 2012
pas_ser_1=airline_injuries$`Passenger Serious Injuries`[1:30]

# convert data to time serious
pas_ser_1=ts(pas_ser_1, start = 1983, end = 2012, frequency = 1)

# Find the log, create a growth rate and plot the growth rate
lpas_ser_1=log(pas_ser_1)
cpas=diff(lpas_ser_1,lag = 1)
cpas_1=lag(cpas,-1)
cpas_2=lag(cpas,-2)
gcpas=diff(lpas_ser_1,lag = 1)*100
gcpas
ts.plot(gcpas)

# Interested in finding out if the lag1 and lag 2 of change in passenger serious are significant  see if the lag of cpas is important

# model
cpas_model=dynlm(cpas~cpas_1+cpas_2)

linearHypothesis(cpas_model,c("cpas_1=0","cpas_2=0"), test = "F")

#Generate a white noise process
set.seed(123)
y = 1 + rnorm(1000,mean=0,sd=2)

#Convert to a time series object
y = ts(y,start=1,end=1000,frequency=1)

# Plotting the ACF and PACF
Acf(gcpas,lag.max = 10)
acf(gcpas, lag.max = 10, type = "partial")


# using MA(1) process
gcpas.fit=arima(gcpas,order=c(0,0,1))
gcpas.fit

# 1 Step ahead forecast
grate.fcast = forecast(gcpas.fit,h=1)
summary(grate.fcast)
plot(grate.fcast)

# 2 Step ahead forecast
grate.fcast1 = forecast(gcpas.fit,h=2)
summary(grate.fcast1)
plot(grate.fcast1)

# 3 Step Ahead Forecast
grate.fcast1 = forecast(gcpas.fit,h=3)
summary(grate.fcast1)
plot(grate.fcast1)