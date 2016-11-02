# #########################################################################
# Assignment 5
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
library(GGally)

setwd(choose.dir())

# read in data
auto_prices <- read_csv('Automobile price data _Raw_.csv', na = c('','?')) 



#
### Cleaning
#

# rename columns
colnames(auto_prices) <- gsub('-','_', colnames(auto_prices))

# subset down to columns of interest
# price, engine size, curb weight, and city mpg 
vars <- c("engine_size", "curb_weight", "city_mpg")
auto_prices <- select(auto_prices, price, one_of(vars))

# create log price
auto_prices$log_price <- log(auto_prices$price)

# inspection of structure
str(auto_prices)



#
### Exploratory
#

# look at pair plot
ggpairs(auto_prices)

# look at pair plot of logged variables
ggpairs(log(auto_prices))
# looks like relationships are more linear and have less of a heteroskedasisty concern
# but lets evaluate side by side and also evaluate square root and square transforms

# transform df to be in a friendly shape to facet plot
auto_prices <- mutate(auto_prices, id = row_number())

ap_raw <- auto_prices %>% select(one_of(vars))
ap_raw <- mutate(ap_raw, id = row_number(), transform = "raw")

ap_log <- log(auto_prices) %>% select(one_of(vars))
ap_log <- mutate(ap_log, id = row_number(), transform = "log")

ap_sqrt <- sqrt(auto_prices) %>% select(one_of(vars))
ap_sqrt <- mutate(ap_sqrt, id = row_number(), transform = "square root")

ap_square <- data.frame(auto_prices^2) %>% select(one_of(vars))
ap_square <- mutate(ap_square, id = row_number(), transform = "squared")

ap_transformed <- bind_rows(ap_raw, ap_log, ap_sqrt, ap_square)
ap_transformed <- left_join(ap_transformed,
                            select(auto_prices, id, log_price),
                            by = "id")

# assume outcome variable is log price 
# lets evaluate transforms by loop through all variables and transforms
# evaluating a simple OLS model for each transform
for (i in 1:length(vars)) {
  writeLines(paste("\n\n\n*****", vars[i], "*****"))
  
  temp <- select_(ap_transformed, vars[i], "log_price", "transform")
  temp <- na.omit(temp)
    
  p <- ggplot(temp, aes_string(y="log_price", x=vars[i])) +
    geom_point() +
    geom_smooth(method = lm) +
    facet_grid(. ~ transform, scales = "free_x") +
    ggtitle(paste("Transforms of",vars[i]))
  
  print(p)
  
  transforms <- unique(temp$transform)
  
  for (j in 1:length(transforms)) {
    writeLines(paste("\n\n\nSIMPLE OLS MODEL EVAL FOR:", transforms[j]))
    
    temp_filtered <- filter(temp, transform == transforms[j])
    
    m <- lm(as.formula(paste("log_price ~",vars[i])), temp_filtered)
    print(paste("R^2 =",summary(m)$r.squared))
    print("")
    print(summary(m)$coefficients)
  }
}

# based on results, the transform of each variable that explains the most variance in log price is:
# engine size -> log
# curb weight -> square root
# city mpg    -> log



#
### model price by engine size, curb weight, and city mpg 
#

# first as basic comparison lets run an untransformed model
ols_baseline <- lm(price ~ engine_size + curb_weight + city_mpg, auto_prices)
summary(ols_baseline)

# now vs log price
ols_baseline2 <- lm(log(price) ~ engine_size + curb_weight + city_mpg, auto_prices)
summary(ols_baseline2)

# finally full defined transforms
ols_transformed <- lm(log(price) ~ log(engine_size) + sqrt(curb_weight) + log(city_mpg), auto_prices)
summary(ols_transformed)

#
### evaluate residuals
#

# look at diagnostic plots
plot(ols_transformed)

# get residuals
residuals <- ols_transformed$residuals
# z normalize
residuals <- (residuals - mean(residuals)) / sd(residuals)

# Kolmogorov-Smirnov on residuals
ks.test(residuals, pnorm)
# fail to reject null hypothesis of residuals being normally distributed

# Shapiro Wilks test on residuals
shapiro.test(residuals)
# reject null hypothesis of residuals being normally distributed


#
### final model
#

# since the residuals did not pass both tests, heteroskedasticity may exist
# there may exist some interaction between terms
# lets try adding in a full set of interaction terms

# run model
ols_final <- lm(log(price) ~ log(engine_size) + sqrt(curb_weight) + log(city_mpg) 
                + engine_size:curb_weight 
#                + engine_size:city_mpg 
                + curb_weight:city_mpg 
#                + engine_size:curb_weight:city_mpg
                , auto_prices)

# evaluate
summary(ols_final)

plot(ols_final)

# tests on residuals
residuals <- ols_final$residuals
residuals <- (residuals - mean(residuals)) / sd(residuals)
ks.test(residuals, pnorm)
shapiro.test(residuals)
# tests fail to reject null hypothesis, indicating residuals are normally distributed
