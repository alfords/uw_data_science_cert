# #########################################################################
# Assignment 7
# Method for Data Analysis
# 
# Author: Josh Jensen
# #########################################################################



#
### Set up
#

# load libraries and set working drive
library(dplyr)
library(readr)
library(ggplot2)
library(forecast)
library(tseries)

setwd(choose.dir())

# read in data
dairy <- read_csv('CADairyProduction.csv') 

# inspect time range & check 12 months per year
dairy %>% group_by(Year) %>% summarise(n()) %>% arrange(Year)

# create ts object
icecream <- ts(dairy$Icecream.Prod, start = 1995, deltat = 1/12)
icecream



#
### Inspect stationarity
#

# plot the ts
plot(icecream)
abline(reg=lm(icecream ~ time(icecream)))
# seasonal components are readily apparent
# unsure about heteroskedadicity
# trend component may be mild enough or skewed to still consider stationary 

# now look side by side with a log transform
par(mfrow=c(2,1))
plot(icecream)
abline(reg=lm(icecream ~ time(icecream)))

plot(log(icecream))
abline(reg=lm(log(icecream) ~ time(icecream)))

par(mfrow=c(1,1))
# log transform appears not to make a huge difference

# lets inspect standard deviation by year
# if heteroskedasticity exists, then we should see a trend in the standard deviation
plot(aggregate(icecream, FUN = sd))
# anamoly exists in 2006-2009, but no overall trend

# use augmented dickey-fuller test for stationarity
adf.test(icecream, alternative = 'stationary')
# accept alt hypothesis that series is stationary


# plot acf & pacf
acf_plots <- function(ts) {
  par(mfrow=c(2,1))
  acf(ts, main = "ACF")
  pacf(ts, main = "PACF")
  par(mfrow=c(1,1))
}

acf_plots(icecream)
# large seasonal component 
#



#
### Run ARMA on remainder
#

# seasonal decomposition
icecream_decomp <- stl(icecream, s.window = 'periodic')

plot(icecream_decomp)

# AR(1) 
fit_arma10 <- arima(icecream_decomp$time.series[,'remainder'], order = c(1,0,0))
acf_plots(fit_arma10$residuals[-1])
# not a good fit

# MA(1)
fit_arma01 <- arima(icecream_decomp$time.series[,'remainder'], order = c(0,0,1))
acf_plots(fit_arma01$residuals[-1])
# not a good fit

# ARMA(1,1)
fit_arma11 <- arima(icecream_decomp$time.series[,'remainder'], order = c(1,0,1))
acf_plots(fit_arma11$residuals[-1])

# ARMA(1,2)
fit_arma12 <- arima(icecream_decomp$time.series[,'remainder'], order = c(1,0,2))
acf_plots(fit_arma12$residuals[-1])
# becoming tedious...

# bring in the big guns
fit_arma <- auto.arima(icecream_decomp$time.series[,'remainder'],
                         max.p = 5, max.d = 0, max.q = 5,
                         max.order = 5, seasonal = FALSE, stepwise = FALSE)

summary(fit_arma)
acf_plots(fit_arma$residuals[-1])



#
### Train ARIMA & forecast out
#

fit_arima <- auto.arima(icecream, max.p=3, max.q=3,
                         max.P=2, max.Q=2, max.order=5, 
                         max.d=2, max.D=1, start.p=0, 
                         start.q=0, start.P=0, start.Q=0)

summary(fit_arima)
acf_plots(fit_arima$residuals[-1])

icecream_forecast <- forecast(fit_arima, h=12)

summary(icecream_forecast)
plot(icecream_forecast)
