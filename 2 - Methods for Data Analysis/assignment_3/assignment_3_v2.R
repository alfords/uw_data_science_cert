# #########################################################################
# Assignment 3
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

setwd(choose.dir())

# read in data
auto_prices <- read_csv('Automobile price data _Raw_.csv', na = c('','?')) 

# rename columns
colnames(auto_prices) <- gsub('-','_', colnames(auto_prices))



#
### test normality of price & log price
#

# create log_price
auto_prices$log_price <- log(auto_prices$price)

# z normalize prices
auto_prices$z_price <- (auto_prices$price - mean(auto_prices$price, na.rm = T)) / sd(auto_prices$price, na.rm = T)
auto_prices$z_log_price <- (auto_prices$log_price - mean(auto_prices$log_price, na.rm = T)) / sd(auto_prices$log_price, na.rm = T)

# inspect z_price distribution
hist(auto_prices$z_price)

qqnorm(auto_prices$z_price, main = 'Q-Q Plot of Price')
abline(a=0, b=1, col='red')
# seems pretty far off of being normally distributed

# inspect z_log_price distribution
hist(auto_prices$z_log_price)

qqnorm(auto_prices$z_log_price, main = 'Q-Q Plot of Log Price')
abline(a=0, b=1, col='red')
# looks to be a lot closer to being normally distributed


# K-S Tests (Kolmogorov-Smirnov test)
ks.test(auto_prices$z_price, pnorm)
ks.test(auto_prices$z_log_price, pnorm)
# reject the null hypothesis of both z_price & z_log_price being normally distributed


# Shapiro-Wilk Normality Test
shapiro.test(auto_prices$z_price)
shapiro.test(auto_prices$z_log_price)
# reject the null hypothesis of both z_price & z_log_price being normally distributed


#
### Hypothesis testing on difference of means of significance of price (log price)
#




# fuel type
table(auto_prices$fuel_type)

auto_prices %>% group_by(fuel_type) %>%
  summarise(mean_log_price = mean(log_price, na.rm=T), 
            sd_log_price = sd(log_price), 
            se_log_price = sd(log_price, na.rm=T)/n())

diesel <- auto_prices[auto_prices$fuel_type == 'diesel',]
gas <- auto_prices[auto_prices$fuel_type == 'gas',]

t.test(diesel$log_price, gas$log_price, alternative = 'two.sided')
# fail to reject null of means being equal

plot_df <- auto_prices %>%
          group_by(fuel_type) %>%
          summarise(mean_log_price = mean(log_price, na.rm=T), se_log_price = sd(log_price, na.rm=T)/n())

ggplot(plot_df, aes(x=fuel_type, y=mean_log_price)) +
  geom_bar(stat = 'identity', fill = 'sky blue') +
  geom_errorbar(aes(ymax=(mean_log_price+se_log_price), ymin=(mean_log_price-se_log_price)), width=.25) +
  ggtitle(paste('Comparison of means' ))

# aspiration
table(auto_prices$aspiration)

std <- auto_prices[auto_prices$aspiration == 'std',]
turbo <-  auto_prices[auto_prices$aspiration == 'turbo',]

t.test(std$log_price, turbo$log_price, alternative = 'less')
# reject null hypothesis of means being equal
# alternative hypothesis: turbo price > standard price


# drive wheels
table(auto_prices$drive_wheels)

fwd <- auto_prices[auto_prices$drive_wheels == 'fwd',]
rwd <- auto_prices[auto_prices$drive_wheels == 'rwd',]

t.test(fwd$log_price, rwd$log_price, alternative = 'two.sided')
# reject null hypothesis of means being equal
# alternative hypothesis: fwd price != rwd price