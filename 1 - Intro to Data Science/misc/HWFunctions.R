# HWFunctions.R
####################################################

# Clear Workspace
rm(list=ls())
# Clear Console:
cat("\014")

# Hello World
# Most language introductions start off by demonstrating a Hello World program
# The simplest Hello World is the string "Hello World" typed into the console:
"Hello World" # The console will respond with "Hello World" if you run this string
hw <- "Hi World"
hw

# User-defined functions
# Hello world as a user-defined function
# Use the function keyword to assign code to a function
hw.f1 <- function()
{
  hw <- "Hello World"
  return(hw)
  hw <- "Bye"
  hw
}

zzzz <- hw.f1()

# A fancier user-defined function for Hello World
hw.f2 <- function(message = "Hello \nWorlds")
{
  x <- 1
  y <- 1
  for (iter in 1:10)
  {
    plot(1,col='white',frame.plot=F,axes=F,xlab="",ylab="")
    x <- x + runif(1)*0.25 - 0.125
    y <- y + runif(1)*0.25 - 0.125
    text(x,y,labels=message, col=runif(1)*8 + 1, cex=runif(1)*6 + 1, font = runif(1)*4 + 1)
    Sys.sleep(.3)
  }
  plot(1,col='white',frame.plot=F,axes=F,xlab="",ylab="")
  text(1,1,labels=message, col=runif(1)*8 + 1, cex=runif(1)*6 + 1, font = runif(1)*4 + 1)
}

# Try out hw.f2
hw.f2()
hw.f2("Bon Voyage")

# How is code persisted?
# Functions are persisted in R files like this one (DataScience02b.R).
# These files have an R suffix (*.R)

############################################################################

# Functional Programmining
# In R, a function can be used as an input to another function

# Clear Workspace
rm(list=ls())
# Clear Console:
cat("\014")

# functional language
vec_line <- seq(-7, 7)
vec_line
plot(c(-6, 15), c(-8, 8), col="white", bg="white")
points(vec_line, vec_line, col="blue", pch=21, bg="yellow")

# anonymous function:  function(x){exp(-x*x/10)}
vec_gaus <- sapply(vec_line, function(x){exp(-x*x/10)})
vec_gaus <- vec_gaus*(max(vec_line) - min(vec_line)) + min(vec_line)
lines(vec_line, vec_gaus, col="red")

# create a function that requires a function
myFunction <- function(x, y, inputFunction)
{
  inputFunction(x) - inputFunction(y)
}

anotherFunction <- function(x)
{
  10*exp(-x*x/(1 + (x %% 3)))
}

input1 <- seq(-7, 7)
input2 <- seq(-8, 6)
output <- myFunction(input1, input2, anotherFunction)

points(output, col='blue', lty=4, lwd=4)
lines(1:length(output), output, col="green")

############################################################################



x <- c(0, 0, 1, 2, 3, 4, 5, 5, 5, 5, 5, 5, 5, 6, 7, 80)
x

#   0  0  1  2  3  4  5  5  5  5  5  5  5  6  7 80
# 2 bins:           |
#   0  0  1  2  3  4  5  5  5  5  5  5  5  6  7 80
# 3 bins:         |                       |
#   0  0  1  2  3  4  5  5  5  5  5  5  5  6  7 80
# 4 bins:   |       |                        |
#   0  0  1  2  3  4  5  5  5  5  5  5  5  6  7 80

