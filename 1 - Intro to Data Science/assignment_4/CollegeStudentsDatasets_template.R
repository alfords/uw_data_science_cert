# CollegeStudentsDatasets.R

# Read the data.  Do not change the following block
url <- "CollegePlans.csv"
Students <- read.csv(url, header=T, stringsAsFactors=FALSE)
Students <- Students[order(-Students$ParentIncome), ]
Students$CollegePlans <- as.numeric(Students$CollegePlans == "Plans to attend")
formula <- CollegePlans ~ Gender + ParentIncome + IQ + ParentEncouragement

# Set repeatable random seed. Do not change the following line
set.seed(4)

PartitionWrong <- function(dataSet, fractionOfTest = 0.3)
{
  numberOfRows <- nrow(dataSet)
  numberOfTestRows <- fractionOfTest * numberOfRows
  testFlag <- 1:numberOfRows <= numberOfTestRows
  testingData <- dataSet[testFlag, ]
  trainingData <- dataSet[!testFlag, ]
  dataSetSplit <- list(trainingData=trainingData, testingData=testingData)
  return(dataSetSplit)
}

PartitionExact <- function(dataSet, fractionOfTest = 0.3)
{
  # ********** Add code here
  return(dataSetSplit)
}

PartitionFast <- function(dataSet, fractionOfTest = 0.3)
{
  # ********** Add code here
  return(dataSetSplit)
}
