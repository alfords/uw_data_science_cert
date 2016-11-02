# #########################################################################
# Seattle 911 Call Logs
# Final Project
# Method for Data Analysis
# 
# Author: Josh Jensen
# #########################################################################



#
### Set up & cleaning
#

# set working directory
setwd('C:/Users/jjensen/Dropbox/UW - Data Science Cert/2 - Methods for Data Analysis/final_project')

# load libraries
library(dplyr)
library(readr)
library(lubridate)
library(reshape2)
library(xts)
library(tseries)
library(forecast)
library(ggplot2)


# read in data
# https://data.seattle.gov/api/views/kzjm-xkqj/rows.csv?accessType=DOWNLOAD
# calls911_log <- read_csv('https://data.seattle.gov/api/views/kzjm-xkqj/rows.csv?accessType=DOWNLOAD')
calls911_log <- read_csv('Seattle_Real_Time_Fire_911_Calls.csv')

# first glance inspection
head(calls911_log, n=15)
str(calls911_log)

# deal with first row
calls911_log <- calls911_log[-1,]
head(calls911_log)

# standardize column names
colnames(calls911_log) <- gsub(' ', '_', tolower(colnames(calls911_log)))

# check that datetime has no nas 
sum(is.na(calls911_log$datetime))
# all good

# convert datetime from a character to a POSIX date-time
calls911_log$dt <- mdy_hms(calls911_log$datetime)
calls911_log[is.na(calls911_log$dt),]$dt <- as.POSIXct(ymd(gsub('T.*','',calls911_log[is.na(calls911_log$dt),]$datetime)))

# final look at the structure
str(calls911_log)



#
### Transform to time series (month, day, & hour)
#

# check if incident_numbers repeat
length(unique(calls911_log$incident_number))
# they do

# create hour, date, day, month, & year dimensions
calls911_log$hour <- hour(calls911_log$dt)
calls911_log$date <- as.Date(calls911_log$dt)
calls911_log$day <- day(calls911_log$dt)
calls911_log$month <- month(calls911_log$dt)
calls911_log$year <- year(calls911_log$dt)

# group bys
calls911_month <- calls911_log %>%
                    group_by(year, month) %>%
                    summarise(date = min(date), days = n_distinct(day), count = n()) %>%
                    arrange(date)
                    
calls911_date <- calls911_log %>%
                    count(date) %>%
                    arrange(date)

calls911_hour <- calls911_log %>%
                    count(date, hour) %>%
                    arrange(date, hour) %>%
                    filter(!is.na(date))

# turn date into a time series
ts_date <- xts(calls911_date$n, order.by = calls911_date$date)

plot(ts_date)

# check out days
dcast(calls911_month, year ~ month, value.var = 'days')
# appears that time series is not represented consistently until 10/11

# create month ts
df <- filter(calls911_month, date >= '2011-11-01' & date <= '2016-04-01')

ts_month <- ts(df$count, start = (2011 + (11 - 1)/12), deltat = 1/12)

plot(ts_month)



#
### Predicting monthly volumes
#


# compare plots with different transformations
par(mfrow=c(3,1))
plot(ts_month)
plot(log(ts_month))
plot(diff(log(ts_month)))
par(mfrow=c(1,1))

# plot acf & pacf
acf_plots <- function(ts) {
  par(mfrow=c(2,1))
  acf(ts, main = "ACF")
  pacf(ts, main = "PACF")
  par(mfrow=c(1,1))
}

acf_plots(ts_month)

lag.plot(ts_month, lags = 9, do.lines = F)



acf_plots(log(ts_month))

lag.plot(log(ts_month), lags = 9, do.lines = F)



acf_plots(diff(log(ts_month)))

lag.plot(diff(log(ts_month)), lags = 9, do.lines = F)



# use log
ts_month_log <- log(ts_month)


# test for stationarity
adf.test(diff(ts_month_log), alternative = 'stationary')
# reject not stationary


# decompose
ts_decomp <- stl(ts_month_log, s.window = 'periodic')
plot(ts_decomp)



# run lm on the trend
trend <- ts_decomp$time.series[,2]

trend_lm <- lm(trend ~ time(trend))
summary(trend_lm)

# plot model
plot(trend, main = "Trend Component with Linear Fit")
abline(mC <- lm(trend ~ time(trend)), col = "red")
    
# linear trend implication
exp(trend_lm$coefficients[2])
# implies that each year has an average 5.25% growth rate in monthly call volume




# auto.arima this bitch
fit_arima <- auto.arima(ts_month_log,
          max.p=5, max.q=5, max.d=2,
          max.P=2, max.Q=2, max.D=2, 
          max.order=8,   start.p=0, 
          start.q=0, start.P=0, start.Q=0,
          stepwise = FALSE)

# inspect model
summary(fit_arima)

# eval fit of residuals
plot(fit_arima$residuals, main = "Plot of Residuals Over Time")

qqnorm(scale(fit_arima$residuals), main = "Q-Q Plot of Residuals")
abline(a = 0, b = 1, col = 'red')

acf_plots(fit_arima$residuals)



### MAPE (mean absolute percentage error)
df <- data.frame(exp(fit_arima$fitted), #fitted values
                 ts_month,             #actual values
                 as.Date(time(ts_month)))
colnames(df) <- c("Fitted", "Actual", "Date")

MAPE <- df %>% summarise(MAPE=mean(abs(Actual-Fitted)/Actual))

### Plot actual versus predicted
ggplot(data=df, aes(x=Date)) +
  geom_line(aes(y=Actual, colour = "Actual"), size=1) +
  geom_line(aes(y=Fitted, colour = "Fitted"), size=1, alpha=.5) +
  theme_bw() + theme(legend.title = element_blank()) + 
  ylab("Seattle Fire 911 call dispatches") + xlab("") +
  ggtitle(paste0("ARIMA --- MAPE = ", round(100*MAPE,2), "%")) + 
  theme(axis.text.x=element_text(angle = -90, hjust = 0))


# predict
pred_arima <- forecast(fit_arima, h = 12)

plot(pred_arima)


# show actual values
round(exp(pred_arima$mean), digits = 0)

# convert predicted values
pred_arima$lower <- exp(pred_arima$lower)
pred_arima$upper <- exp(pred_arima$upper)
pred_arima$mean <- exp(pred_arima$mean)
pred_arima$x <- exp(pred_arima$x)

# plot
plot(pred_arima)

ts_month











#
### PCA & Clustering
#


