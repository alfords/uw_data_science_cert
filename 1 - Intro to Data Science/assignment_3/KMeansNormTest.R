# KMeansNormTest.R

# Clear Workspace
rm(list=ls())
# Clear Console:
cat("\014")

source("KMeans.R")
source("KMeansHelper.R")
source("KMeansNorm.R")

TestObservations <- as.matrix(read.csv("TestObservations.csv"))
TestCenters <- matrix(c(1, 1, -2, -2, 2, -2), nrow=3)


# TestObservations Distribution in second dimension
hist(TestObservations[,2], col=rgb(1,1,0,1))

# TestObservations Distribution in first dimension
hist(TestObservations[,1], col=rgb(0,0,1,0.25), add=T)

# What is the single most obvious difference between these two distributions?

# Test 1
KMeansNorm(clusterCenters = TestCenters, observations = TestObservations, normD1=F, normD2=F)
# Does clustering occur along one or two dimensions?  Which dimensions?  Why?
'Technically 2 dimensions, but because the magnitude is much greater on the Y axis, clustering effectively is only split on the Y axis.'

# Test 2
KMeansNorm(clusterCenters = TestCenters, observations = TestObservations, normD1=T, normD2=F)
# Does clustering occur along one or two dimensions?  Which dimensions?  Why?
'Technically 2 dimensions, but because the magnitude is much greater on the Y axis, clustering effectively is only split on the Y axis.'

# Test 3
KMeansNorm(clusterCenters = TestCenters, observations = TestObservations, normD1=F, normD2=T)
# Does clustering occur along one or two dimensions?  Which dimensions?  Why?
'Technically 2 dimensions, but because the magnitude is much greater on the X axis, clustering effectively is only split on the X axis.'


# Test 4
KMeansNorm(clusterCenters = TestCenters, observations = TestObservations, normD1=T, normD2=T)
# Does clustering occur along one or two dimensions?  Which dimensions?  Why?
'Clusters on both 2 dimensions due to normalization.'
