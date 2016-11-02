# #########################################################################
# Assignment 1
# Method for Data Analysis
# 
# Author: Josh Jensen
# 
# Tasks:
# - Explore auto price data set.
# - Write R program that shows/illustrates 3 key takeaways of your 
# choosing from exploring the data.
# #########################################################################



#
### Set up
#

# load libraries and set working drive
library(dplyr)
library(ggplot2)
library(readr)

setwd(choose.dir())

# read in data
auto_prices <- read_csv('Automobile price data _Raw_.csv', na = c("","?")) 



#
### Cleaning & initial inspection
#

# inspect schema
str(auto_prices)

View(auto_prices)

# replace - with _ to make columns easier to work with
colnames(auto_prices) <- gsub("-","_",colnames(auto_prices))

# inspect some character outputs
table(auto_prices$fuel_type)
table(auto_prices$make)

table(auto_prices$num_of_cylinders)
table(auto_prices$aspiration)
table(auto_prices$drive_wheels)

# create df with ints & nums only
auto_nums_only <- auto_prices[,(lapply(auto_prices, class) != 'character')]

# look at the pairs plot
pairs(auto_nums_only)



#
### Exploratory analysis
#


### 1. investigate mpg

# city mpg
ggplot(data = auto_prices, aes(x = city_mpg, y = price)) +
  geom_point() +
  geom_smooth(method = lm) +
  ylim(0,50000) +
  ggtitle('City MPG by Price')

# highway mpg
ggplot(data = auto_prices, aes(x = highway_mpg, y = price)) +
  geom_point() +
  geom_smooth(method = lm) +
  ylim(0,50000) +
  ggtitle('Highway MPG by Price')


### 2. investigate fuel type

auto_prices$fuel_type <- as.factor(auto_prices$fuel_type)

auto_by_fuel_type <- auto_prices %>% 
                        group_by(fuel_type) %>% 
                        summarise(mean_price = mean(price, na.rm = T), sd_price = (sd(price, na.rm = T)))

auto_by_fuel_type

# boxplot
ggplot(data = auto_prices, aes(x = fuel_type, y = price)) +
  geom_boxplot(fill="#D55E00") +
  ggtitle('Fuel type by Price Boxplot')

# faceted histogram
ggplot(data = auto_prices, aes(x = price)) +
  geom_histogram(aes(y = ..density..), alpha=.5) +
  facet_grid(. ~ fuel_type) +
  ggtitle('Price Distribution Density Histogram by Fuel type')

# scatter with mpg
ggplot(data = auto_prices, aes(x = city_mpg, y = price, color = fuel_type)) +
  geom_point() +
  geom_smooth(method = lm, alpha = 0) +
  ylim(0,50000) +
  ggtitle('City MPG by Price; split by fuel type')



### 3. investigate pricey cluster of vehicles (>$30K)

# add expensive boolean variable
auto_prices$expensive_30k <- (auto_prices$price > 30000)

auto_nums_only$expensive_30k <- (auto_prices$price > 30000)

# compute means of all numeric columns
auto_nums_only %>% group_by(expensive_30k) %>%
  summarise_each(funs(mean))
# expensive cars have almost double the engine size, lets investigate that

# boxplot
ggplot(data = auto_prices, aes(x = expensive_30k, y = engine_size)) +
  geom_boxplot(fill="#D55E00") +
  ggtitle('Engine Size by Cars Costings > $30K')
# huge difference!

# scatter
ggplot(data = auto_prices, aes(x = engine_size, y = price, color = expensive_30k)) +
  geom_point() +
  geom_smooth(method = lm, alpha = 0.2) +
  ggtitle('Engine Size by Price; Comparing Cars Costings > $30K')
