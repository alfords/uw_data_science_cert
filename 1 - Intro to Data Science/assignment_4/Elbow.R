# Elbow.R
# This exercise requires the following data file:  CollegePlans.csv
# 
# This script demonstrates the Elbow method on K Means.
# The Elbow method is used to determine the proper number of clusters
# 

# Exercise/Quiz: 
# What is the impact of normalizing the numeric columns, IQ and Parent Income?
# 1.) How many centroids are there if both IQ and Parent Income are normalized?
# 2.) How many centroids are there if IQ is normalized but Parent Income is not normalized?
# 3.) How many centroids are there if IQ is not normalized but Parent Income is normalized?
# 4.) How many centroids are there if neither IQ nor Parent Income are normalized?
# 5.) How does gender affect clustering? (See the final table of cluster centers aka centroids)
# 6.) Which numeric dimension, ParentIncome or IQ, has more effect if you do not normalize?
# 7.) Under which conditions do clusters separate the most on IQ?
# 8.) Why would clusters separate primarily on Parent Income?

# Clear objects from Memory
rm(list=ls())
# Clear Console:
cat("\014")

# Load the data
fileName="CollegePlans.csv"
DATAFRAME<-read.csv(file=fileName, stringsAsFactors=F)

# Look at the Data
head(DATAFRAME)
summary(DATAFRAME)

# Data Preparation
# Get rid of Key Column
DATAFRAME <- DATAFRAME[-1]
# Binarize 1st column
DATAFRAME[1] <- as.numeric(DATAFRAME[,1] == DATAFRAME[1,1])
# Binarize 4th column
DATAFRAME[4] <- as.numeric(DATAFRAME[,4] == DATAFRAME[1,4])
# Binarize 5th column
DATAFRAME[5] <- as.numeric(DATAFRAME[,5] == DATAFRAME[1,5])

# Z-score normalize ParentIncome column
ParentIncomeSD <- sd(DATAFRAME$ParentIncome)
ParentIncomeMean <- mean(DATAFRAME$ParentIncome)
# To prevent normalization of ParentIncome, comment-out the following line of code
DATAFRAME$ParentIncome <- (DATAFRAME$ParentIncome - ParentIncomeMean)/ParentIncomeSD

# Z-score normalize IQ column
IQSD <- sd(DATAFRAME$IQ)
IQMean <- mean(DATAFRAME$IQ)
# To prevent normalization of IQ, comment-out the following line of code
DATAFRAME$IQ <- (DATAFRAME$IQ - IQMean)/IQSD

# Look at the Data
head(DATAFRAME)
summary(DATAFRAME)

# Repeat k-means nstart times and find the k-means where
# the sum of squares from points to the assigned cluster centres is least
nstart <- 5
# auto-detect cluster count by Elbow criterion for betweenSS (intra-cluster)
maxClusterCount <- 15
testTSS<-c(0)
# The following loop creates nstart*maxClusterCount models.  This can take some time
for(clusterCount in 1:maxClusterCount)
{
    set.seed(31981)
    tModel<-kmeans(DATAFRAME, clusterCount, nstart=nstart)
    testTSS[clusterCount] <- tModel$betweenss
}

# Plot the variance between clusters
plot(testTSS)
Sys.sleep(0.5)

#update the max cluster count
maxClusterCount <- length(testTSS)

#scale the inter-cluster variance to the same size as the number of tested clusters
testTSSNormalized<-testTSS * maxClusterCount / max(testTSS)

# Plot the variance between clusters normalized to the maximum number of clusters
# Note:  Only the scale changes!
plot(testTSSNormalized)
Sys.sleep(0.5)

#compute the relative improvement (differential)
testTSSDiff<-c(maxClusterCount, testTSSNormalized[-1]-testTSSNormalized[-maxClusterCount])

# Plot the differential (first derivative)
plot(testTSSDiff)
Sys.sleep(0.5)

# look for the first cluster count that adds less than 1 of improvement
centers<-which(testTSSDiff < 1)[1]-1

plot(c(), c(), xlim=c(0, length(testTSSDiff)), ylim=c(0, length(testTSSDiff)), xlab="Number of Clusters", ylab="Change in Between-Cluster Variance", main="Determine Number of Clusters")
lines(1:length(testTSSDiff),     testTSSDiff,     lwd=3, lty=1, col="red")
points(1:length(testTSSDiff),     testTSSDiff, pch=21, bg='blue', col='black', cex=2)
abline(h=1, col = "yellow", lwd=5, lty=1)
abline(h=testTSSDiff[centers], col = "black", lwd=2, lty=5)
abline(v=centers, col = "black", lwd=2, lty=5)
legend("topright", text.font=1, legend=paste("Number of Clusters:", centers))

#Create the clustering model
set.seed(31981)
kMeansModel <- kmeans(DATAFRAME, centers=centers, nstart=nstart)

names(kMeansModel)
is(kMeansModel$centers)

CentersDf <- as.data.frame(kMeansModel$centers)
if (max(CentersDf$ParentIncome) < 5)
{
  CentersDf$ParentIncome <- round((CentersDf$ParentIncome * ParentIncomeSD) + ParentIncomeMean)
} else
{
  CentersDf$ParentIncome <- round(CentersDf$ParentIncome)
}
if (max(CentersDf$IQ) < 5)
{
  CentersDf$IQ <- round((CentersDf$IQ * IQSD) + IQMean)
} else
{
  CentersDf$IQ <- round(CentersDf$IQ)
}
CentersDf <- round(CentersDf, 2)
print(CentersDf)
