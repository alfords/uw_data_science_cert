##--------------------------------------------
##
## Class: PCE 350 Data Science Methods Class
##
##---- Regression with R Part 2 ----
##
##----- Model matrix with categorical variables ---
##

auto.price <- read_csv('Automobile price data _Raw_.csv', na = c('','?')) 
colnames(auto.price) <- gsub('-','.', colnames(auto.price))

unique(auto.price$drive.wheels) # How many values?

## First attempt to build a simple model matrix
mod.mat = model.matrix(log(price) ~ drive.wheels, data = auto.price)
head(mod.mat)  # Note the use of contrast to intercept

## Or, code without the intercept
mod.mat = model.matrix(log(price) ~ 0 + drive.wheels, data = auto.price)
head(mod.mat)


mod.mat = model.matrix(log(price) ~ 0 + drive.wheels + aspiration, data = auto.price)
head(mod.mat)
