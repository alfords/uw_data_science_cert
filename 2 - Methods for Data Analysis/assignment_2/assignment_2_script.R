# #########################################################################
# Assignment 2: Monty Hall Simulation
# Method for Data Analysis
# 
# Author: Josh Jensen
# 
# Problem:
# Suppose you're on a game show, and you're given the choice of three doors: Behind one door is a car; behind the others, goats. You pick a door, say No. 1, and the host, who knows what's behind the doors, opens another door, say No. 3, which has a goat. He then says to you, "Do you want to pick door No. 2?" Is it to your advantage to switch your choice?
# #########################################################################



#
### Set up
#

# load libraries and set working drive
library(dplyr)
library(ggplot2)
library(reshape2)



#
### Run Monty Hall Simulation
#

# Generate data frame of n obs, with random initial door choices & doors with car behind
# note: runif will not generate either of the extreme values unless max = min or max-min is small compared to min

simMontyHall <- function(n = 10000) {
  df <- data.frame(door_choice = trunc(runif(n, min = 1, max = 4)),   # generates a uniform random number of 1, 2, or 3 
                   door_with_car = trunc(runif(n, min = 1, max = 4))  # generates a uniform random number of 1, 2, or 3 
                   )
  
  # create boolean columns for both switch & stay strategies
  df$stay_win <- ifelse(df$door_choice == df$door_with_car, T, F)
  
  # note: since Monty Hall will never reveal the car, a switching strategy will win as long as the car was not behind the original choice
  df$switch_win <- ifelse(df$door_choice != df$door_with_car, T, F)
  
  df
}

# run with 1M observations
df <- simMontyHall(1000000)



#
### Summarize Results
#

# create & print summary df
summary_df <- summarise(df, stay_prob = mean(stay_win),
                            stay_var = var(stay_win),
                            switch_prob = mean(switch_win),
                            switch_var = var(switch_win))
melt(summary_df)

# probability plot
prob_df <- melt(select(summary_df, ends_with("_prob")))

ggplot(prob_df, aes(x = variable, y = value)) +
  geom_bar(stat = "identity") +
  labs(title="Simulated Probability of Winning the Car by Strategy", x="Strategy", y="Probability")

# variance plot
var_df <- melt(select(summary_df, ends_with("_var")))

ggplot(var_df, aes(x = variable, y = value)) +
  geom_bar(stat = "identity") +
  labs(title="Simulated Variance of Winning the Car by Strategy", x="Strategy", y="Variance")