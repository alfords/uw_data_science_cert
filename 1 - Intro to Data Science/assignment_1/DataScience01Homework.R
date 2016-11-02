# DataScience01Homework.R
# Copyright 2016 by Ernst Henle

# Assignment Item 1
# Download and Install R. Then download and install R studio. Calculate 2 + 3 in R studio .
# Type your name into the console.  Take a screenshot of R-studio (not just the console) and
# name the screenshot file:  RStudio.jpg or RStudio.png or RStudio.pdf.  The format should
# be jpg, png, or pdf.

# Assignment Item 2
# Join the LinkedIn group for this course. Introduce yourself, start a discussion, or make a
# comment on an existing discussion.  Write the topic of that discussion in a txt file
# called discussion.txt

# Clear Workspace
rm(list=ls())
# Clear Plot
dev.off()
# Clear Console:
cat("\014")

# Assignment Item 3 / Solution 3
url <- "http://archive.ics.uci.edu/ml/machine-learning-databases/00225/Indian Liver Patient Dataset (ILPD).csv"
ILPD <- read.csv(url, header=FALSE, stringsAsFactors=FALSE)

# Assignment Item 4
# Manually construct a vector of column using
# http://archive.ics.uci.edu/ml/datasets/ILPD+(Indian+Liver+Patient+Dataset)#
# Attribute Information:
# 1. Age Age of the patient
# 2. Gender Gender of the patient
# 3. TB Total Bilirubin
# 4. DB Direct Bilirubin
# 5. Alkphos Alkaline Phosphotase
# 6. Sgpt Alamine Aminotransferase
# 7. Sgot Aspartate Aminotransferase
# 8. TP Total Protiens
# 9. ALB Albumin
# 10. A/G Ratio Albumin and Globulin Ratio
# 11. Selector field used to split the data into two sets (labeled by the experts) 

# Solution 4
headers <- c("Age","Gender","DB","TB","Alkphos","Sgpt","Sgot","TPr","ALB","AGRatio","Selector") 

# Associate names with the dataframe
names(ILPD) <- headers

# Assignment Item 5 / Solution 5
head(ILPD)

# Assignment Item 6
# Note that "A/G Ratio" has NA results with sapply(ILPD, mean)
# na.rm = TRUE removes NAs from the calculation in medain(x, na.rm = TRUE); mean(x, na.rm = TRUE); sd(x, na.rm = TRUE); 

# Solution 6
?sapply
sapply(ILPD, mean, na.rm=TRUE)
sapply(ILPD, sd, na.rm=T)
sapply(ILPD, median, na.rm=T)

# Another way to determine mean and median
summary(ILPD)

# Assignment Item 7
# Plot all histograms.  Note that Gender doesn't plot because it isn't numeric
# Solution 7
hist(ILPD$Age, col="cyan")
hist(ILPD$Gender, col="cyan")
hist(ILPD$TB, col="cyan")
hist(ILPD$DB, col="cyan")
hist(ILPD$Alkphos, col="cyan")
hist(ILPD$Sgpt, col="cyan")
hist(ILPD$Sgot, col="cyan")
hist(ILPD$TP, col="cyan")
hist(ILPD$ALB, col="cyan")
hist(ILPD$AGRatio, col="cyan")
hist(ILPD$Selector, col="cyan")

# Assignment Item 8 /Solution 8
# plot(ILPD) will not work if gender is not numeric
ILPD_noGender <- ILPD[-2] # remove gender because it isn't numeric
plot(ILPD_noGender) # plot the new ILPD

# An even better solution is to make Gender numeric
ILPD$Gender <- (ILPD$Gender == "Female")
head(ILPD)
hist(ILPD$Gender, col="blue")
plot(ILPD, col="blue") # Works because gender is  numeric
#
# Assignment Item 9
# This matrix of plots allows a data scientist to get a quick overview of distributions between attributes.
# Solution 9 (Example)
# When points are only found at two values on one of the dimensions, then that dimension is binary.
# Gender and Selector are binary.
# When the scatter plots form recognizable curves or lines, then two attributes are correlated.  
# Direct Billirubin and Total Billirubin are strongly correlated
# Albumin and Total Protein are correlated

####################################
# Clean up workspace
rm(list=ls())
# Clear Console:
cat("\014")

# Assignment Item 10
# Remove Outliers:
v <- c(-1, 1, -1, 1, 1, 17, -3, 1, 1, 3)
highLimit <- mean(v) + 2*sd(v)
goodFlag <- v < highLimit
v <- v[goodFlag]
v
# Solution 10
#  [1] -1  1 -1  1  1 -3  1  1  3

# Assignment Item 11
# Relabel:
v <- c("BS", "MS", "PhD", "HS", "Bachelors", "Masters", "High School", "BS", "MS", "MS")
vRelabeled <- v

vRelabeled[vRelabeled == "Bachelors"] <- "BS"

vRelabeled["Bachelors" == vRelabeled] <- "BS"
vRelabeled["Masters" == vRelabeled] <- "MS"
vRelabeled["High School" == vRelabeled] <- "HS"
vRelabeled
# Solution 11
#  [1] "BS"  "MS"  "PhD" "HS"  "BS"  "MS"  "HS"  "BS"  "MS"  "MS"

# Assignment Item 12
# Min-Max normalization:
v <-  c(-1, 1, -1, 1, 1, 17, -3, 1, 1, 3)
vMinMax <- (v - min(v))/(max(v) - min(v))
vMinMax
# Solution 12
#  [1] 0.1 0.2 0.1 0.2 0.2 1.0 0.0 0.2 0.2 0.3

# Assignment Item 13
# z-score normalization
v <- c(-1, 1, -1, 1, 1, 17, -3, 1, 1, 3)
vZScore <- (v - mean(v))/sd(v)
vZScore
# Solution 13
#  [1] -0.5437099 -0.1812366 -0.5437099 -0.1812366 -0.1812366  2.7185494 -0.9061831 -0.1812366 -0.1812366  0.1812366

# Assignment Item 14
# Binarize:
Colors <- c('Red', 'Green', 'Blue', 'Green', 'Blue', 'Blue', 'Blue', 'Red', 'Green', 'Blue')
Red <- Colors == "Red"
Green <- Colors == "Green"
Blue <- Colors == "Blue"
Red; Green; Blue
# Solution 14
# [1]  TRUE FALSE FALSE FALSE FALSE FALSE FALSE  TRUE FALSE FALSE
# [1] FALSE  TRUE FALSE  TRUE FALSE FALSE FALSE FALSE  TRUE FALSE
# [1] FALSE FALSE  TRUE FALSE  TRUE  TRUE  TRUE FALSE FALSE  TRUE

Red <- as.numeric(Red)
Green <- as.numeric(Green)
Blue <- as.numeric(Blue)
Red; Green; Blue
# Solution 14 (Easier to read)
# [1] 1 0 0 0 0 0 0 1 0 0
# [1] 0 1 0 1 0 0 0 0 1 0
# [1] 0 0 1 0 1 1 1 0 0 1

# Assignment Item 15
# Discretization
v <- c(81, 3, 3, 4, 4, 5, 5, 5, 5, 5, 5, 5, 6, 6, 6, 6, 6, 7, 7, 7, 7, 8, 8, 9, 12, 23, 24, 25)
numberOfBins <- 3
vMax <- max(v)
vMin <- min(v)
vRange <- vMax - vMin
binRange <- vRange / numberOfBins
bin1Min <- -Inf
bin1Max <- vMin + binRange
bin2Max <- bin1Max + binRange
bin3Max <- +Inf
bin1Min
bin1Max
bin2Max
bin3Max
discretizedV <- v
discretizedV[bin1Min < v & v <= bin1Max] <- "L" # Low
discretizedV[bin1Max < v & v <= bin2Max] <- "M" # Med
discretizedV[bin2Max < v & v  < bin3Max] <- "H" # High
discretizedV
# Solution 15
#  [1] "H" "L" "L" "L" "L" "L" "L" "L" "L" "L" "L" "L" "L" "L" "L" "L" "L" "L" "L" "L" "L" "L" "L" "L" "L" "L" "L" "L"

# Assignment Item 16
# One way to do equal area dicretization
v <- c(81, 3, 3, 4, 4, 5, 5, 5, 5, 5, 5, 5, 6, 6, 6, 6, 6, 7, 7, 7, 7, 8, 8, 9, 12, 23, 24, 25)
length(v) # 28 / 3 is approx 9; Therefore aim for approx 9 items per bucket
sort(v)
#   3  3  4  4  5  5  5  5  5  5  5  6  6  6  6  6  7  7  7  7  8  8  9 12 23 24 25 81
# |--------------------------------|--------------------------|------------------------|
# bin1Max is 5, bin2Max is 7
numberOfBins <- 3
vSorted <- sort(v)
binRange <- length(vSorted) / numberOfBins
bin1Min <- -Inf
bin1Max <- vSorted[round(binRange)]
bin2Max <- vSorted[round(2*binRange)]
bin3Max <- +Inf
discretizedV <- v
discretizedV[bin1Min < v & v <= bin1Max] <- "L"
discretizedV[bin1Max < v & v <= bin2Max] <- "M"
discretizedV[bin2Max < v & v <  bin3Max] <- "H"
discretizedV
# Solution 16
#  [1] "H" "L" "L" "L" "L" "L" "L" "L" "L" "L" "L" "L" "M" "M" "M" "M" "M" "M" "M" "M" "M" "H" "H" "H" "H" "H" "H" "H"
