# #########################################################################
# Assignment 6
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



#
### Cleaning
#

# rename columns
colnames(auto_prices) <- gsub('-','_', colnames(auto_prices))

# create log price
auto_prices$log_price <- log(auto_prices$price)

# subset down to columns of interest
# make, fuel_type, aspiration, body_style, drive_wheels, length, curb_weight, engine_type, num_of_cylinders, engine_size, city_mpg
vars <- c("make", "fuel_type", "aspiration", "body_style", "drive_wheels", "length", "curb_weight", "engine_type", "num_of_cylinders", "engine_size", "city_mpg")
auto_prices <- select(auto_prices, log_price, one_of(vars))

# note svd requires fully balanced data set, so lets remove na's
auto_prices <- na.omit(auto_prices)

# inspection of structure
str(auto_prices)



#
### create model matrix
#

# inspect number of dimensions in data frame
dim(auto_prices)
# note 11 features and 1 outcome measure

# create model matrix
ap_matrix <- model.matrix(log_price ~ . , auto_prices)
head(ap_matrix)

# inspect number of dimensions in matrix
dim(ap_matrix)
# note 45 orthogonal features and 1 outcome measure (not included in matrix)



#
### SVD Regression
#

# compute the svd
ap_svd <- svd(ap_matrix)

# compute inverse of the singular values
1/ap_svd$d

d_inverse <- diag(1/ap_svd$d)
head(d_inverse)

# compute psuedo inverse
psuedo_inverse <- ap_svd$v %*% t(d_inverse) %*% t(ap_svd$u)
dim(psuedo_inverse)

# verify correct calculation of puesdo matrix
psuedo_inverse %*% (ap_svd$u %*% diag(ap_svd$d) %*% t(ap_svd$v))
# approximately identity matrix

# find model coefficients
ap_coef <-  psuedo_inverse %*% as.matrix(auto_prices$log_price)
ap_coef

# now with reducing the dimensionality
# reduce dimensionality by removing the least stable singular vectors 
d_reduced <- ifelse(1/ap_svd$d > 0.5, 0, 1/ap_svd$d)
d_inverse2 <- diag(d_reduced)
d_inverse2

# number of orthogonal features used in model
length(d_reduced[d_reduced > 0])

# run regression
psuedo_inverse2 <- ap_svd$v %*% t(d_inverse2) %*% t(ap_svd$u)
dim(psuedo_inverse2)

ap_coef2 <- psuedo_inverse2 %*% as.matrix(auto_prices$log_price)
ap_coef2



# run diagnostics by looking at residuals

# calculate predicted and residual values
auto_prices$pred <- ap_matrix %*% ap_coef2
auto_prices$resid <- auto_prices$pred - auto_prices$log_price


# shamelessly lifting Prof. Steve Elston's diagnositic plots
# found here:
# https://github.com/StephenElston/DataScience350/blob/master/Lecture6/SVDReg.R
plot.diagnostic = function(df){
  ## Plot the histogram and Q-Q of the residuals
  par(mfrow = c(1,2))
  hist(df$resid,
       main = 'Histogram of residuals',
       xlab = 'Model residuals')
  qqnorm(df$resid)
  par(mfrow = c(1,1))
  
  ## Plot the residuals vs the predicted values
  require(ggplot2)
  ggplot(df, aes(pred, resid)) +
    geom_point(size = 2, alpha = 0.3) + 
    ggtitle('Residuals vs predicted value') + 
    xlab('Predicted values') + ylab('Residual')
}

# run diagnostic plots
plot.diagnostic(auto_prices)

# compute rmse
rmse <- sqrt(mean(auto_prices$resid^2))
rmse



#
### Stepwise Regression
#

# drop pred & resid values
auto_prices <- dplyr::select(auto_prices, -pred, -resid)

# fit full model
ap_lm <- lm(log_price ~ ., data = auto_prices)

summary(ap_lm)
plot(ap_lm)

# fit step wise
library(MASS)
ap_lmstep <- stepAIC(ap_lm, direction = 'both')

summary(ap_lmstep)
plot(ap_lmstep)

# compare with anova
anova(ap_lmstep, ap_lm, blah)
