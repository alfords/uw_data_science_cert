# Quiz05a_Classification.R
# Copyright Ernst Henle 2016

# Clear Workspace
rm(list=ls())
# Clear Console:
cat("\014")

url <- "https://www.dropbox.com/s/6h3fj8u8ew9i21f/PatientReadmission.csv?dl=1" # PatientReadmission.csv
Patients <- read.csv(url, header=TRUE, stringsAsFactors=TRUE)
formulaClass <- Readmitted ~ .

# Partition the data
fractionOfTest=0.25
set.seed(2)
randoms <- runif(nrow(Patients))
cutoff <- quantile(randoms, fractionOfTest)
testFlag <- randoms <= cutoff
testingData <- Patients[testFlag, ]
trainingData <- Patients[!testFlag, ]

# Get Package
reposURL <- "http://cran.rstudio.com/"
if (!require("randomForest")) {install.packages("randomForest", dep=TRUE, repos=reposURL)} else {" randomForest is already installed "}
# Now that the package is installed, we want to load the package so that we can use its functions
library(randomForest)
if (!require("nnet")) {install.packages("nnet", dep=TRUE, repos=reposURL)} else {" nnet is already installed "}
# Now that the package is installed, we want to load the package so that we can use its functions
library(nnet)
if (!require("rpart")) {install.packages("rpart", dep=TRUE, repos=reposURL)} else {" rpart is already installed "}
# Now that the package is installed, we want to load the package so that we can use its functions
library(rpart)

# Q4:  What is d2?  The following lines of code create a neural net model, a random forrest, and a descision tree model.
set.seed(4)
Model.NN <- nnet(formula=formulaClass, data=d2, size=10, maxit=200)
Model.RF <- randomForest(formula=formulaClass, data=d2)
Model.DT <- rpart(formula=formulaClass, data=d2)

# Q5:  What is d1? The following lines of code create probabilities from three different models.  These probabilities will be used to test the accuracy of their respectives models. 
probabilities.NN <- predict(Model.NN, newdata=d1, type="raw")
probabilities.RF <- predict(Model.RF, newdata=d1, type="prob")[,2]
probabilities.DT <- predict(Model.DT, newdata=d1, type = "prob")[,2]
