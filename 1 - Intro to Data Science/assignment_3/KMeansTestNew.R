# KMeansTestNew.R

# To set up test, do the following in R Studio:
# (1) The function ClusterPlot takes too much time:  Comment out the Sys.sleep().
# (2) Put The students' scripts into your staging directory
# (3) Source this script
# (4) Read A_A_HW2.txt in the staging directory

# Clear Workspace
rm(list=ls())
# Clear Plots
tryCatch({bogus<-dev.off()}, error=function(e){bogus <- 0})
# Clear Console:
cat("\014")

stagingDir <- "D:/BusinessIntelligence/DataScientist/2016.03.31_Spring/Lesson02/Homework"
files <- dir(stagingDir)
include <- grep(".*KMeans.*\\.R$",files,value = F)
exclude <- grep("Helper",files,value = F)
indices <- setdiff(include, exclude)
files <- files[indices]

reviews <- file.path(stagingDir,"A_A_HW2.txt")
con <- file(description=reviews)
open(con=con, open="w")
writeLines(as.character(Sys.time()), con=con)
close(con)

fileNo <- 0
while(fileNo < length(files))
{
  fileNo <- fileNo + 1
  savedObjects <- c("files", "fileNo","stagingDir")
  save(list=savedObjects, file="KMeansTestNew.RData")
  
  reviews <- file.path(stagingDir,"A_A_HW2.txt")
  con <- file(description=reviews)
  open(con=con, open="a")
  writeLines("############################", con=con)
  writeLines(files[fileNo], con=con)
  close(con)
  
  rm(list=ls())
  load("KMeansTestNew.RData")
  tryCatch({
    source(file.path(stagingDir,files[fileNo]))
    print("True")
  }, error=function(e){print("file does not source")}) # tryCatch
  source("KMeansHelper.R")
  load("KMeansTestNew.RData")
  reviews <- file.path(stagingDir,"A_A_HW2.txt")
  con <- file(description=reviews)
  open(con=con, open="a")
  
  Sys.sleep(0.5)
  
  writeLines("Test 0:  ClusterPlot is test of test", con=con)
  tryCatch({
    ClusterPlot()
    writeLines("Test 0:  TRUE", con=con)
  }, error=function(e){writeLines("ClusterPlot throws error:  Test does not work", con=con)}) # tryCatch
  
  tally <- -2
  writeLines("Test 1a:  findLabelOfClosestCluster", con=con)
  tryCatch({
    actualResult<-findLabelOfClosestCluster()
    expectedResult <- c(
      3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3,
      3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 1, 3, 3, 3, 1, 3, 3, 3, 3,
      3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 1, 3, 3, 1, 3, 3, 3, 3, 2, 2, 3, 3,
      2, 2, 3, 3, 3, 3, 2, 3, 3, 3, 3, 3, 3, 3, 1, 3, 3)
    success <- sum((actualResult - expectedResult)^2)< 0.0001
    if (success)
    {
      tally <- tally + 1
    } else
    {
      tally <- tally + 0.5
    } # if else
    writeLines(paste("Test 1a: ", success), con=con)
  }, error=function(e){writeLines("findLabelOfClosestCluster with default values throws error", con=con)}) # tryCatch
  
  writeLines("Test 1b:  findLabelOfClosestCluster", con=con)
  tryCatch({
    bestResult <- matrix(nrow=3, ncol=2, byrow= T, data=c(
      0.0332000,  0.6508000,
      1.5163158,  1.0057895,
      -0.7610256, -0.9071795))
    actualResult<-findLabelOfClosestCluster(observations = sampleObservations, clusterCenters=bestResult)
    expectedResult <- c(
      3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3,
      3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 1, 1, 1, 3, 1, 1, 1, 1, 1,
      1, 1, 3, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 2, 1, 1, 3, 1, 2, 2, 2, 1,
      2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 1, 2, 2, 3, 2)
    success <- sum((actualResult - expectedResult)^2)< 0.0001
    if (success)
    {
      tally <- tally + 1
    } else
    {
      tally <- tally + 0.5
    } # if else
    writeLines(paste("Test 1b: ", success), con=con)
  }, error=function(e){writeLines("findLabelOfClosestCluster with best centroids throws error", con=con)}) # tryCatch
  
  writeLines("Test 2a:  calculateClusterCenters default", con=con)
  tryCatch({
    actualResult<-calculateClusterCenters()
    expectedResult <- -matrix(nrow=3, ncol=2, byrow= T, data=c(
      0.02392857,  0.02464286,
      -0.10321429,  0.10071429,
      0.08370370, -0.13000000))
    success <- sum((actualResult[order(actualResult[,1]),] - expectedResult[order(expectedResult[,1]),])^2)< 0.0001
    if (success)
    {
      tally <- tally + 1
    } else
    {
      tally <- tally + 0.5
    } # if else
    writeLines(paste("Test 2a: ", success), con=con)
  }, error=function(e){writeLines("calculateClusterCenters with default values throws error", con=con)}) # tryCatch
  
  writeLines("Test 2b:  calculateClusterCenters test result", con=con)
  tryCatch({  
    clusterLabels = c(
      3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3,
      3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 1, 1, 1, 3, 1, 1, 1, 1, 1, 1, 1,
      3, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 2, 1, 1, 3, 1, 2, 2, 2, 1, 2, 2, 2,
      2, 2, 2, 2, 2, 2, 2, 2, 2, 1, 2, 2, 3, 2)
    actualResult<-calculateClusterCenters(observations=sampleObservations, clusterLabels=clusterLabels)
    expectedResult <- -matrix(nrow=3, ncol=2, byrow= T, data=c(
      -0.0332000, -0.6508000,
      -1.5163158, -1.0057895,
      0.7610256,  0.9071795))
    success <- sum((actualResult[order(actualResult[,1]),] - expectedResult[order(expectedResult[,1]),])^2)< 0.0001
    if (success)
    {
      tally <- tally + 1
    } else
    {
      tally <- tally + 0.5
    } # if else
    writeLines(paste("Test 2b: ", success), con=con)
  }, error=function(e){writeLines("calculateClusterCenters with best labels throws error", con=con)}) # tryCatch
  
  writeLines("Test 3a:  KMeans standard 3 clusters", con=con)
  tryCatch({
    actualResult<-KMeans()
    expectedResult <- -matrix(nrow=3, ncol=2, byrow= T, data=c(
      -0.0332000, -0.6508000,
      -1.5163158, -1.0057895,
      0.7610256,  0.9071795))
    success <- sum((actualResult[order(actualResult[,1]),] - expectedResult[order(expectedResult[,1]),])^2)< 0.0001
    if (success)
    {
      tally <- tally + 2.0
    } else
    {
      tally <- tally + 0.5
    } # if else
    writeLines(paste("Test 3a: ", success), con=con)
  }, error=function(e){writeLines("KMeans with default values throws error", con=con)}) # tryCatch
  
  writeLines("Test 3b:  KMeans different 3 centroids and points", con=con)
  tryCatch({
    testCentroids3 <- matrix(c(0, -2, -2, -2, -1, 0), nrow=3)
    newObservations <- sampleObservations
    numberOfObservations <- nrow(newObservations)
    pivot <- 1:numberOfObservations < numberOfObservations/2
    firstHalf <- newObservations[pivot,2]
    newObservations[pivot,2] <- newObservations[pivot,1]
    newObservations[pivot,1] <- firstHalf
    newObservations[!pivot,] <- -newObservations[!pivot,]
    actualResult<-KMeans(observations = newObservations, clusterCenters = testCentroids3)
    expectedResult <- matrix(nrow=3, ncol=2, byrow= T, data=c(
      0.2773077, -0.3000000,
      -0.8294444, -0.9416667,
      -1.6438095, -0.7385714))
    success <- sum((actualResult[order(actualResult[,1]),] - expectedResult[order(expectedResult[,1]),])^2)< 0.0001
    if (success)
    {
      tally <- tally + 2.0
    } else
    {
      tally <- tally + 0.5
    } # if else
    writeLines(paste("Test 3b: ", success), con=con)
  }, error=function(e){writeLines("KMeans with default values throws error", con=con)}) # tryCatch
  
  writeLines("Test 4a:  KMeans 4 clusters", con=con)
  tryCatch({
    testCentroids4 <- -matrix(c(0, -1.5, .8, 0, -0.7, -1, 1, 1), nrow=4)
    actualResult<-KMeans(clusterCenters = testCentroids4)
    expectedResult <- -matrix(nrow=4, ncol=2, byrow= T, data=c(
      -0.0332000, -0.6508000,
      -1.5163158, -1.0057895,
      0.8734286,  0.8557143,
      -0.2225000,  1.3575000))
    success <- sum((actualResult[order(actualResult[,1]),] - expectedResult[order(expectedResult[,1]),])^2)< 0.0001
    if (success)
    {
      tally <- tally + 1
    } else
    {
      tally <- tally + 0.5
    } # if else
    writeLines(paste("Test 4a: ", success), con=con)
  }, error=function(e){writeLines("KMeans with 4 clusters throws error", con=con)}) # tryCatch
  
  writeLines("Test 4b:  KMeans 2 clusters", con=con)
  tryCatch({
    testCentroids2 <- -matrix(c(-2, 2, -2, 2), nrow=2)
    actualResult<-KMeans(clusterCenters = testCentroids2)
    expectedResult <- -matrix(nrow=2, ncol=2, byrow= T, data=c(
      -0.7385714, -0.8280952,
      0.7575610,  0.8482927))
    success <- sum((actualResult[order(actualResult[,1]),] - expectedResult[order(expectedResult[,1]),])^2)< 0.0001
    if (success)
    {
      tally <- tally + 1
    } else
    {
      tally <- tally + 0.5
    } # if else
    writeLines(paste("Test 4b: ", success), con=con)
  }, error=function(e){writeLines("KMeans with two Clusters Throws error", con=con)}) # tryCatch
  
  writeLines("Test 5a:  KMeans unused cluster", con=con)
  tryCatch({
    finalResult <- -matrix(nrow=3, ncol=2, byrow= T, data=c(
      -0.0332000, -0.6508000,
      -1.5163158, -1.0057895,
      0.7610256,  0.9071795))
    finalResultPlus <- rbind(finalResult, c(10,10))
    actualResult<-KMeans(clusterCenters = finalResultPlus)
    expectedResult <- finalResult
    success <- sum((actualResult[order(actualResult[,1]),] - expectedResult[order(expectedResult[,1]),])^2)< 0.0001
    if (success)
    {
      tally <- tally + 1
    } else
    {
      tally <- tally + 0.5
    } # if else
    writeLines(paste("Test 5a: ", success), con=con)
  }, error=function(e){writeLines("KMeans unused cluster Throws error", con=con)}) # tryCatch
  
  writeLines("Test 5b:  KMeans 4 clusters; single point cluster", con=con)
  tryCatch({
    singlePointCluster <- -matrix(c(-1.47, -0.01, 0.79, 1.91, -1.04, -0.50, 0.94, 1.43), nrow=4)
    actualResult<-KMeans(clusterCenters = singlePointCluster)
    expectedResult <- -matrix(nrow=4, ncol=2, byrow= T, data=c(
      -1.466500000, -1.0430000,
      -0.007777778, -0.4992593,
      0.790571429,  0.9402857,
      1.910000000,  1.4300000))
    success <- sum((actualResult[order(actualResult[,1]),] - expectedResult[order(expectedResult[,1]),])^2)< 0.0001
    if (success)
    {
      tally <- tally + 1
    } else
    {
      tally <- tally + 0.5
    } # if else
    writeLines(paste("Test 5b: ", success), con=con)
  }, error=function(e){writeLines("Single Point Cluster Throws error", con=con)}) # tryCatch
  
  if (tally > 8) tally <- tally + 1
  writeLines(paste(files[fileNo]," has ", tally, " points"), con=con)
  close(con)
} # for
reviews <- file.path(stagingDir,"A_A_HW2.txt")
con <- file(description=reviews)
open(con=con, open="a")
writeLines("############################", con=con)
close(con)
