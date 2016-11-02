# #########################################################################
# Assignment 4
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
library(simpleboot)
library(combinat)

setwd(choose.dir())

# read in data
auto_prices <- read_csv('Automobile price data _Raw_.csv', na = c('','?')) 



#
### Cleaning
#

# rename columns
colnames(auto_prices) <- gsub('-','_', colnames(auto_prices))

# subset down to columns of interest
# price, number of doors, body style, drive wheels, number of cylinders, engine type
vars <- c("num_of_doors", "body_style", "drive_wheels", "num_of_cylinders", "engine_type")
auto_prices <- select(auto_prices, price, one_of(vars))

# set to factor
str(auto_prices)

auto_prices[vars] <- lapply(auto_prices[vars], as.factor)

# create log price
auto_prices$log_price <- log(auto_prices$price)

# final inspection of structure
str(auto_prices)



#
### Compare differences in log prices with mean and visually
#

# compare the means
for (i in 1:length(vars)) {
  temp <- auto_prices %>% 
              group_by_(vars[i]) %>%
              summarise(mean_log_price = mean(log_price, na.rm=T), n = n())
  print(temp)
}

# inspect boxplots
for (i in 1:length(vars)) {
  p <- ggplot(auto_prices, aes_string(x=vars[i], y="log_price", fill=vars[i])) +
    geom_boxplot() +
    ggtitle(paste("Boxplot of",vars[i]))
  print(p)
}



#
### Use ANOVA
#

log_price_anova <- function(factor_vector) {
  # run anova
  anova_test <- aov(auto_prices$log_price ~ factor_vector)
  # print results
  writeLines("ANOVA TEST:\n")
  print(summary(anova_test))
  
  # run tukey anova on anova output
  tukey_anova <- TukeyHSD(anova_test)
  # print results
  writeLines("\n\n\nTUKEY ANOVA TEST:\n")
  print(tukey_anova)
  
  # plot tukey anova difference in means plot for 95% confidence
  plot(tukey_anova)
}

# number of doors
log_price_anova(auto_prices$num_of_doors)

# body style
log_price_anova(auto_prices$body_style)

# drive wheels
log_price_anova(auto_prices$drive_wheels)

# number of cylinders
log_price_anova(auto_prices$num_of_cylinders)

# engine type
log_price_anova(auto_prices$engine_type)



#
### bootstrap distribution CIs of the (differences of) means 
#


boot_factor_diff_in_means_log_price <- function(factor_vector, replicates = 10000, alpha = .05) {
  
  # use combn to output all distinct combinations of n levels of factor, with choose 2
  
  if (length(levels(factor_vector)) > 2) {
    combinations <- combn(levels(factor_vector), 2)
  } else if (length(levels(factor_vector)) == 2) { 
    combinations <- matrix(c(levels(factor_vector)[1], levels(factor_vector)[2]), nrow = 2, ncol = 1)
  } else {
    stop()
  }
    
  # loop through all distinct combinations
  for (i in 1:ncol(combinations)) {
    
    # filter
    df1 <- auto_prices[factor_vector == combinations[1,i],]
    df2 <- auto_prices[factor_vector == combinations[2,i],]
    
    # skip bootstrapping if either has less than 10 observations
    if ((nrow(df1)>10) & (nrow(df2)>10)) {
      # run bootstrap
      boot <- two.boot(na.omit(df1$log_price), na.omit(df2$log_price), mean, R = replicates)
      
      # print results
      writeLines(paste("\n\n\nDifference between", combinations[1,i], "and", combinations[2,i]))
      print(paste0("with confidence interval of ", 100 * (1 - .05), "%"))
      print("")
      print(paste("LOWER BOUND: ", quantile(boot$t, alpha/2)))
      print(paste("MEAN DIFF:   ", mean(boot$t)))
      print(paste("UPPER BOUND: ", quantile(boot$t, 1-(alpha/2))))
    }
  }
}


# number of doors
boot_factor_diff_in_means_log_price(auto_prices$num_of_doors)

# body style
boot_factor_diff_in_means_log_price(auto_prices$body_style)

# drive wheels
boot_factor_diff_in_means_log_price(auto_prices$drive_wheels)

# number of cylinders
boot_factor_diff_in_means_log_price(auto_prices$num_of_cylinders)

# engine type
boot_factor_diff_in_means_log_price(auto_prices$engine_type)
