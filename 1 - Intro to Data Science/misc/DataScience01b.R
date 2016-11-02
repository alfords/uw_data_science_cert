# DataScience01b.R
# Copyright 2016 by Ernst Henle
############################################################################
#
# Clear Workspace
rm(list=ls())
# Clear Console:
cat("\014")

# Remove Outlier:
x <- c(1, 2, 2, 3, 3, 3, 4, 4, 5, 99)
highLimit <- mean(x) + 2*sd(x)
lowLimit <- mean(x) - 2*sd(x)
goodFlag <- (x < highLimit) & (x > lowLimit)
x[goodFlag]
x
x <- x[goodFlag]
x

# Replace Outlier
x <- c(1, 2, 2, 3, 3, 3, 4, 4, 5, 99)
badFlag <- (x > highLimit) | (x <  lowLimit) # !goodFlag
x[badFlag] <- mean(x)
x

# Better:
x <- c(1, 2, 2, 3, 3, 3, 4, 4, 5, 99)
x
x[badFlag] <- mean(x[goodFlag])
x

############################################################################
# Digression on NA

# Remove outlier
x <- c(1, 2, 2, 3, 3, 3, NA, 4, 5, 99, NA)
x
x <- x[(x < mean(x) + 2*sd(x)) & (x >  mean(x) - 2*sd(x))]
x

# Remove outlier
x <- c(1, 2, 2, 3, 3, 3, NA, 4, 5, 99, NA)
x <- x[(x < mean(x, na.rm=T) + 2*sd(x, na.rm=T)) & (x >  mean(x, na.rm=T) - 2*sd(x, na.rm=T))]
x

1 == 1
1 == 2
NA == NA
NA == 1
1 == NA
is.na(NA)
is.na(1)

# how many NA?
x <- c(1, 2, 2, 3, 3, 3, NA, 4, 5, 99, NA)
is.na(x)
sum(is.na(x))
summary(x) # Does not show NA from Character
############################################################################

# Clear Workspace
rm(list=ls())
# Clear Console:
cat("\014")

# Get Data
# URL of data
url <- "http://archive.ics.uci.edu/ml/machine-learning-databases/mammographic-masses/mammographic_masses.data"
# Read Data into a data frame
# A data frame is a common data structure in R
# A data frame is list of equal sized vectors
# The vectors in a data frame can be of different types
mamm <- read.csv(url, header=F, stringsAsFactors=FALSE)
head(mamm)

sapply(mamm, is) # note that vectors are not numeric
mamm <- data.frame(sapply(mamm, as.numeric)) # make vectors numeric
sapply(mamm, is) # vectors are now numeric
head(mamm) # But the drawback is that vectors contain NA

# Number of rows with one or more NA
nrow(mamm) - sum(complete.cases(mamm))
nrow(mamm) - nrow(na.omit(mamm))

# Number of NAs
sum(sapply(mamm, is.na))

nrow(mamm)
mamm <- na.omit(mamm)
nrow(mamm)

sum(sapply(mamm, is.na))

############################################################################
# Clear Workspace
rm(list=ls())
# Clear Console:
cat("\014")

# Relabel this:  
x <- c("Car", "Automobile", "Bike", "Truck", "Bicycle", "Sedan", "Coupe", "Cycle", "Truck", "Velo", "Automobile", "Bike")
# All small cars, like "Car", "Automobile", "Sedan", and "Coupe" should be "Car"
x[x == "Automobile"] <- "Car"
x
x[x == "Sedan"] <- "Car"
x[x == "Coupe"] <- "Car"
x
# All Bicycles, like "Bike", "Bicycle", and "Velo" should be "Bike"
x[x == "Bicycle"] <- "Bike"
x[x == "Velo"] <- "Bike"
x[x == "Cycle"] <- "Bike"
x
# All Big Vehicles like "Truck" and "Lorry" should be "Truck"
x[x == "Lorry"] <- "Truck" # Note:  There weren't any Lorries!
x

# Recode this:
x <- c(1, 1, 1, 2, 1, 3, 3, 3)
x[x == "1"] <- "USA"
x[x == "2"] <- "Japan"
x[x == "3"] <- "Europe"
x

# Binarize this:
x <- c('R', 'G', 'B', 'B', 'B', 'B', 'B', 'R', 'G', 'B', 'G', 'G', 'G')
isRed <- x == 'R'
isGreen <- x == 'G'
isBlue <- x == 'B'

isRed
isGreen
isBlue

# You can cast T/F into 1/0
isRed <- as.numeric(isRed)
isGreen <- as.numeric(isGreen)
isBlue <- as.numeric(isBlue)

# Better Presentation:
isRed; isGreen; isBlue

# Or, as a data frame
data.frame(isRed, isGreen, isBlue)

###########################################################
# Normalization
x <- c(-3, 3, 15, 3, 15, 18, -9, -27, 13, 15, 6)
x
y <- 1000*x # For explanation only.  Do not do this in homework
y

# Trivial Normalization
a <- 0
b <- 1
normalized <- (x - a) / b
normalized

a <- 0
b <- 1
normalized <- (y - a) / b
normalized

# min max Normalization
a <- min(x)
b <- max(x) - min(x)
normalized <- (x - a) / b
normalized

a <- min(y)
b <- max(y) - min(y)
normalized <- (y - a) / b
normalized

# z-Score Normalization
a <- mean(x)
b <- sd(x)
normalized <- (x - a) / b
normalized

a <- mean(y)
b <- sd(y)
normalized <- (y - a) / b
normalized

# Discretization into 4 bins
x <- c(-3, 3, 15, 3, 15, 18, -9, -27, 13, 15, 6)
range <- max(x) - min(x)
binWidth <- range / 4
bin1Min <- -Inf
bin1Max <- min(x) + binWidth
bin2Max <- min(x) + 2*binWidth
bin3Max <- min(x) + 3*binWidth
bin4Max <- Inf
xDiscretized <- rep(NA, length(x))
xDiscretized
xDiscretized[bin1Min < x & x <= bin1Max] <- "Very Low"
xDiscretized[bin1Max < x & x <= bin2Max] <- "Low"
xDiscretized[bin2Max < x & x <= bin3Max] <- "High"
xDiscretized[bin3Max < x & x <= bin4Max] <- "Very High"
xDiscretized

# Equalization
x <- c(-3, 3, 15, 3, 15, 18, -9, -27, 13, 15, 6)

#binLimits <- quantileBinMax(x, 4)
sort(x)
# [1] -27  -9  -3   3   3   6  13  15  15  15  18
#    <--------|-----------|------|--------------->
# Very Low:  -27, -9
veryLowMin <- -Inf
VeryLowMax <- -9
# Low:  -3, 3, 3
LowMax <- 3
# High:  6, 13
HighMax <- 13
# Very High:  15,  15,  15,  18
VeryHighMax <- Inf
xDiscretized <- x
xDiscretized[veryLowMin < x & x <=  VeryLowMax] <- "Very Low"
xDiscretized[VeryLowMax < x & x <=      LowMax] <- "Low"
xDiscretized[LowMax     < x & x <=     HighMax] <- "High"
xDiscretized[HighMax    < x & x <= VeryHighMax] <- "Very High"
#x <- c("Low", "Low", "Very High", "Low", "Very High", "Very High", "Very Low", "Very Low", "High", "Very High", "High")
xDiscretized
############################################################################
