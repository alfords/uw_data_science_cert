# 1	Download and Install R. Then download and install R studio. Calculate 2 + 3 in R studio . Type your name into the console.  Take a screenshot of R-studio (not just the console) and name the screenshot file:  RStudio.jpg or RStudio.png or RStudio.pdf.  The format should be jpg, png, or pdf.
'Done'

# 2	Join the LinkedIn group for this course. Introduce yourself, start a discussion, or make a comment on an existing discussion.  Write the topic of that discussion in a txt file called discussion.txt
"Favorite R packages?"

# 3	Review the patterns described in DataScience01a.R and use R to get the Indian Liver Patient Dataset from the UCI machine learning repository.
# url <- http://archive.ics.uci.edu/ml/machine-learning-databases/00225/Indian%20Liver%20Patient%20Dataset%20(ILPD).csv # Copy this url carefully
# ILPD <- read.csv(url, header=FALSE, stringsAsFactors=FALSE)


url <- "http://archive.ics.uci.edu/ml/machine-learning-databases/00225/Indian%20Liver%20Patient%20Dataset%20(ILPD).csv"
ILPD <- read.csv(url, header=FALSE, stringsAsFactors=FALSE)


# 4	The following was not covered in class.  Get the 11 column headers from this page: http://archive.ics.uci.edu/ml/datasets/ILPD+(Indian+Liver+Patient+Dataset)#
# Manually construct a vector of column headers using this pattern:
#   a.headers <- c(<name1>, <name2>, .) # Each column has a name
# Associate names with the dataframe using this pattern:
#   a.names(<dataframe>) <- headers

# 1. Age	Age of the patient 
# 2. Gender	Gender of the patient 
# 3. TB	Total Bilirubin 
# 4. DB	Direct Bilirubin 
# 5. Alkphos Alkaline Phosphotase 
# 6. Sgpt Alamine Aminotransferase 
# 7. Sgot Aspartate Aminotransferase 
# 8. TP	Total Protiens 
# 9. ALB	Albumin 
# 10. A/G Ratio	Albumin and Globulin Ratio 
# 11. Selector field used to split the data into two sets (labeled by the experts) 
headers <- c("age","gender","tb","db","alkphos","sgpt","sgot","tp","alb","a_g_ratio","test_set")
names(ILPD) <- headers


# 5	Use head(ILPD) to view the first 6 rows.
head(ILPD)


# 6	Review the patterns described in DataScience01a.R.  Write code to determine the mean, median, and standard deviation (sd) of each column and present their values in the console. Some calculations may fail.  Where applicable, fix the failures by using na.rm = TRUE. Type ?median to see how.
lapply(ILPD, mean)
lapply(ILPD, median)
lapply(ILPD, sd)


# 7	Review the patterns described in DataScience01a.R Create Histograms (hist) for each column where possible.

i = 1
for (i in 1:ncol(ILPD)) {
  
  if (class(ILPD[,i]) == "integer" |  class(ILPD[,i]) == "numeric") {
    hist(ILPD[,i], xlab = colnames(ILPD)[i], main = paste("Histogram of", colnames(ILPD)[i]))
  }
  i = i + 1
}
  

# 8	Review the patterns described in DataScience01a.R Use the plot(ILPD) function on this data frame to present a general overview of the data. You want to see a matrix of many plots. You may have some problems because the Gender column is not numeric. You can skip the Gender column, or you can turn the gender column into a numeric column. You might need help from a fellow student, the LinkedIn group, or me.
library(dplyr)
plot(select(ILPD, -gender))

# 9	Look at the plots from plot(ILPD) and answer: 
# 9a	What can you say about the data?
'
Total Bilirubin (tb) & Direct Bilirubin (db) seem to be strongly positively correlated with each other. 
This also appears to be the case for Total Protiens (tp) and albumin (alb).
Age appears uncorrelated with most features.

'

# 9b	How can you tell if a vector contains continuous numbers or binary data?
'Binary number features only appear in the plot with 2 distinct values on the relevant axis'

# 9c	How can you tell if two vectors are correlated?
'They are tightly packed and follow a general trend.'



# 10	Review the patterns described in DataScience01b.R  Write code to remove outliers from the following vector and present the result in the console: c(-1, 1, -1, 1, 1, 17, -3, 1, 1, 3)
x <- c(-1, 1, -1, 1, 1, 17, -3, 1, 1, 3)
highLimit <- mean(x) + 2*sd(x)
lowLimit <- mean(x) - 2*sd(x)
noOutliers <- (x < highLimit) & (x > lowLimit)
x[noOutliers]

# 11	Review the patterns described in DataScience01b.R  Write code to relabel the following vector.  Use the shortest strings for each category in the relabeled version. Present the result in the console: c('BS', 'MS', 'PhD', 'HS', 'Bachelors', 'Masters', 'High School', 'BS', 'MS', 'MS') 
x <- c('BS', 'MS', 'PhD', 'HS', 'Bachelors', 'Masters', 'High School', 'BS', 'MS', 'MS') 

x[x == "Bachelors"] <- "BS"
x[x == "Masters"] <- "MS"
x[x == "High School"] <- "HS"
x

# 12	Review the patterns described in DataScience01b.R  Write code to normalize the following vector using a Min-Max normalization and present the result in the console:  c(-1, 1, -1, 1, 1, 17, -3, 1, 1, 3)
x <- c(-1, 1, -1, 1, 1, 17, -3, 1, 1, 3)
a <- min(x)
b <- max(x) - min(x)
normalized <- (x - a) / b
normalized

# 13	Review the patterns described in DataScience01b.R  Write code to normalize the following vector using a Z-score normalization and present the result in the console:  c(-1, 1, -1, 1, 1, 17, -3, 1, 1, 3) 
x <- c(-1, 1, -1, 1, 1, 17, -3, 1, 1, 3)
a <- mean(x)
b <- sd(x)
normalized <- (x - a) / b
normalized

# 14	Review the patterns described in DataScience01b.R  Write code to binarize: c('Red', 'Green', 'Blue', 'Green', 'Blue', 'Blue', 'Blue', 'Red', 'Green', 'Blue')  and present the result in the console
x <- c('Red', 'Green', 'Blue', 'Green', 'Blue', 'Blue', 'Blue', 'Red', 'Green', 'Blue')
isRed <- x == 'Red'
isGreen <- x == 'Green'
isBlue <- x == 'Blue'
cbind(isRed,isGreen,isBlue)

# 15	Review the patterns described in DataScience01b.R  Write code to discretize the following vector into 3 bins of equal range and present the result in the console: c(81, 3, 3, 4, 4, 5, 5, 5, 5, 5, 5, 5, 6, 6, 6, 6, 6, 7, 7, 7, 7, 8, 8, 9, 12, 23, 24, 25)
x <- c(81, 3, 3, 4, 4, 5, 5, 5, 5, 5, 5, 5, 6, 6, 6, 6, 6, 7, 7, 7, 7, 8, 8, 9, 12, 23, 24, 25)
range <- max(x) - min(x)
binWidth <- range / 3
bin1Min <- -Inf
bin1Max <- min(x) + binWidth
bin2Max <- min(x) + 2*binWidth
bin3Max <- Inf
xDiscretized <- rep(NA, length(x))
xDiscretized
xDiscretized[bin1Min < x & x <= bin1Max] <- "Low"
xDiscretized[bin1Max < x & x <= bin2Max] <- "Middle"
xDiscretized[bin2Max < x & x <= bin3Max] <- "High"
xDiscretized


# 16	Discretize the following vector into 3 bins of equal of near equal amounts of numbers.  No Code is necessary, just present the results as commented text. c(81, 3, 3, 4, 4, 5, 5, 5, 5, 5, 5, 5, 6, 6, 6, 6, 6, 7, 7, 7, 7, 8, 8, 9, 12, 23, 24, 25)
x <- c(81, 3, 3, 4, 4, 5, 5, 5, 5, 5, 5, 5, 6, 6, 6, 6, 6, 7, 7, 7, 7, 8, 8, 9, 12, 23, 24, 25)
x_thirds <- ntile(x, 3)
x_thirds
# the ntile dplyr function breaks breaks a vector into n different bins of the near equal observation size
table(x_thirds)

# 17	Submit to canvas the screenshot from item 1, the txt file from item 2, and an R script that contains the answers to items 3 through 16.  Submit by Saturday 11:57 PM to the Homework Submission site on Canvas in the Module called "Lesson 01".  The Assignment is called "Assignment 01".  If you cannot submit the assignment on time, please notify me before the deadline at ErnstHe@UW.edu and put "Data Science UW 2016 Assignment 01 late" (without quotes) in the email subject line


# 18	Reading assignment
#http://en.wikipedia.org/wiki/Cluster_analysis
#http://en.wikipedia.org/wiki/K-means_clustering
#http://home.deib.polimi.it/matteucc/Clustering/tutorial_html/
#http://www.sqlserverdatamining.com/ArtOfClustering/default.aspx
#19	Look through Preview section of Lesson 01 Overview