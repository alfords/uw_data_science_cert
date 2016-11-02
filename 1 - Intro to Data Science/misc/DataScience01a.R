# DataScience01a.R
# Copyright 2016 by Ernst Henle
####################################################

# Introduction to R
# This is a rapid introduction for Data Scientists

# What is R?
# From Wikipedia:
# R is an open source programming language and software environment for statistical
# computing and graphics. The R language is widely used among statisticians and data
# miners for developing statistical software and data analysis.
# (http://en.wikipedia.org/wiki/R_%28programming_language%29)

# Basic R resources:
# http://www.r-project.org/
# http://cran.r-project.org/doc/contrib/Verzani-SimpleR.pdf

# Download R from:
# http://cran.r-project.org/bin/

# Rstudio
# Rstudio integrates components into an IDE:
#    File Editor
#	   Console (command line interpreter)
#	   Plots
#    Help
#    Packages
#
# Download R studio from:  
#  http://www.rstudio.com/ide/download/desktop


# Clear Workspace
rm(list=ls())
# Clear Console:
cat("\014")

# Use the R console like a calculator
3 + 5
exp(-1^2/2)/sqrt(2*pi)

# assignment
x <- 2
y <- exp(-x^2/2)/sqrt(2*pi)

# Get values of variables
x
y
# Hello World
# Most language introductions start off by demonstrating a Hello World program
# The simplest Hello World is the string "Hello World" typed into the console:
"Hello World" # The console will respond with "Hello World" if you run this string
hw <- "Hi World"
hw

# How is code persisted?
# Scripts and Functions are persisted in R files like this one.
# These files have an R suffix (*.R)

# The most common data structure in R:  the vector
# here x is a scalar:
x <- -1
y <- exp(-x^2/2)/sqrt(2*pi)
x
y

# Ask R:  What is x?
is(x)
length(x)

# Some math with vectors
x <- -1:2
y <- 1:4
x
y
2*x^3
x+y
x*y

# here x is a vector:
x <- c(-2, -1.5, -1.0, -0.5, 0, 0.5, 1.0, 1.5, 2)
length(x)
# y is a vector because x is a vector
y <- exp(-x^2/2)/sqrt(2*pi)
length(y)
x
y
plot(x, y); lines(x, y) # plot points and add a line to the plot

# Use seq to create a sequence in a vector.
# Note how function arguments (from, to, by) can be specified by their name.
x <- seq(from = -2.5, to = 2.5, by = 0.1)
y <- exp(-x^2/2)/sqrt(2*pi)
plot(x, y); lines(x, y) # plot points and add a line to the plot

###################################################################
# Clear Workspace
rm(list=ls())
# Clear Console:
cat("\014")

# Sharing Data

# assign a url to variable "url"
url <- "http://archive.ics.uci.edu/ml/machine-learning-databases/blood-transfusion/transfusion.data"
# Download a rectangular dataset
DATAFRAME.obj<-read.csv(url, header=TRUE)

# present an overview of these data

# How many rows and columns are in the dataframe?
nrow(DATAFRAME.obj)
ncol(DATAFRAME.obj)

# See all the data
DATAFRAME.obj

# view the first few rows of the data
head(DATAFRAME.obj)

# view the column names
names(DATAFRAME.obj)

# What do these names mean?

# What are the stanrad deviation, mean, and median of each vector?
sd(DATAFRAME.obj)

is(DATAFRAME.obj)

# determine the standard deviation, mean, and median for the 1st vector
sd(DATAFRAME.obj[               , 1])
mean(DATAFRAME.obj[, 1])
median(DATAFRAME.obj[, 1])

is(DATAFRAME.obj)
sapply(DATAFRAME.obj, is)

sapply(DATAFRAME.obj, length)

summary(DATAFRAME.obj)

# determine the standard deviation, mean, and median for each vector
sapply(DATAFRAME.obj, sd)
sapply(DATAFRAME.obj, mean)
sapply(DATAFRAME.obj, median)

# Get a profile of each column
hist(DATAFRAME.obj$Recency, col=rgb(0,1,0,.5)) # hist(DATAFRAME.obj[, 1])
hist(DATAFRAME.obj$Frequency, col=rgb(0,1,0,.5)) # hist(DATAFRAME.obj[, 2])
hist(DATAFRAME.obj$Monetary, col=rgb(0,1,0,.5)) # hist(DATAFRAME.obj[, 3])
hist(DATAFRAME.obj$Time, col=rgb(0,1,0,.5)) # hist(DATAFRAME.obj[, 4])
hist(DATAFRAME.obj$whether, col=rgb(0,1,0,.5)) # hist(DATAFRAME.obj[, 5])

# Correlate columns
plot(DATAFRAME.obj)

############################################################################
# A glimpse into what we will do in future lessons
# Predictive Analytics:

# Names are difficult.  They are too long.  
names(DATAFRAME.obj) <- c("Recency", "Frequency", "Monetary", "Time", "whether")
head(DATAFRAME.obj)
# Create a schema
formula <- whether ~ Recency + Frequency + Monetary + Time
# Create a model using the data and an algorithm (logistic regression)
model <- glm(formula=formula, data=DATAFRAME.obj, family="binomial")
# Predict probabilities based on model and data
predictedProbabilities <- predict(model, newdata=DATAFRAME.obj[-5], type="response")

# Compare predictions to actual outcomes
threshold <- 0.5
predictedValues <- as.numeric(predictedProbabilities > threshold)
"Confusion Matrix "
table(predictedValues, DATAFRAME.obj$whether)
############################################################################
