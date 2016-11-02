# ClassifyStudents.R

# Clear objects from Memory
rm(list=ls())
# Clear Console:
cat("\014")

source("CollegeStudentsDatasets.R")

# Set repeatable random seed
set.seed(4)

###################################################

# Partition data between training and testing sets

# Replace the following line with a function that partitions the data correctly
StudentsSplit <- PartitionExact(Students, fractionOfTest=0.4) # ********** Change here
TestStudents <- StudentsSplit$testingData
TrainStudents <- StudentsSplit$trainingData

### declare threshold
threshold <- .7

###################################################

# Logistic Regression (glm, binomial)

# http://data.princeton.edu/R/glms.html
# http://www.statmethods.net/advstats/glm.html
# http://stat.ethz.ch/R-manual/R-patched/library/stats/html/glm.html
# http://www.stat.umn.edu/geyer/5931/mle/glm.pdf

# Create logistic regression
logit_model <- glm(formula, family = "binomial", data = TrainStudents)

# Predict the outcomes for the test data. (predict type="response")
predictedProbabilities <- predict(logit_model, newdata = TestStudents, type = "response")
logit_predicted <- ifelse((predictedProbabilities > threshold), "Attend", "Not Attend")



###################################################

# Naive Bayes

# http://cran.r-project.org/web/packages/e1071/index.html
# http://cran.r-project.org/web/packages/e1071/e1071.pdf
# Get the algorithm

reposURL <- "http://cran.rstudio.com/"
# install package with naive bayes if not alreay installed
if (!require("e1071")) {install.packages("e1071", dep=TRUE, repos=reposURL)} else {" e1071 is already installed "}
# Now that the package is installed, we want to load the package so that we can use its functions
library(e1071)


# Create Naive Bayes model
nb_model <- naiveBayes(formula, data = TrainStudents)

# Predict the outcomes for the test data. (predict type="raw")
predictedProbabilities <-  predict(nb_model, newdata = TestStudents, type = "raw")
nb_predicted <- ifelse((predictedProbabilities[,"1"] > threshold), "Attend", "Not Attend")



###################################################

# Confusion Matrices

actual <- ifelse(TestStudents$CollegePlans, "Attend", "NotAttend")

### Confusion Matrix for Logistic Regression

# convert the predicted probabilities to predictions using a threshold
print(" ")
print(" -------------------------------- ")
print("Confusion Matrix for Logistic Regression")
# create a table to compare predicted values to actual values
logit_confusion <- table(logit_predicted, actual, dnn = c("PREDICTED","ACTUAL"))
logit_confusion
logit_accuracy <- (logit_confusion[1,1] + logit_confusion[2,2]) / sum(logit_confusion)
logit_accuracy


### Confusion Matrix for Naive Bayes

# convert the predicted probabilities to predictions using a threshold
print(" ")
print(" -------------------------------- ")
print("Confusion Matrix Naive Bayes")

# create a table to compare predicted values to actual values
nb_confusion <- table(nb_predicted, actual, dnn = c("PREDICTED","ACTUAL"))
nb_confusion
nb_accuracy <- (nb_confusion[1,1] + nb_confusion[2,2]) / sum(nb_confusion)
nb_accuracy

###################################################

# Bad Partition; threshold = 0.5
# 
# --------------------------------
# "Confusion Matrix for Logistic Regression"
#              Actual
# Predicted    Attend  NotAttend
# Attend        934        116
# NotAttend     759       1071
# Accuracy defined as fraction of predictions that are correct
# Accuracy:  (934 + 1071)/(934 + 759 + 116 + 1071) = 70%
# 
# --------------------------------
# "Confusion Matrix Naive Bayes"
#            Actual
# Predicted   Attend NotAttend
# Attend       325        84
# NotAttend   1368      1103
# Accuracy defined as fraction of predictions that are correct
# Accuracy:  (325 + 1103)/(325 + 1368 + 84 + 1103) = 50%

# Fill in the rest:

# Exact Partition; threshold = 0.5
#
# --------------------------------
# "Confusion Matrix for Logistic Regression"
#            Actual
# Predicted   Attend NotAttend
# Attend      687       227
# NotAttend   260      1706
# Accuracy defined as fraction of predictions that are correct
# Accuracy:   83.09%
#
# --------------------------------
# "Confusion Matrix Naive Bayes"
#            Actual
# Predicted   Attend NotAttend
# Attend      570       209
# NotAttend   377      1724
# Accuracy defined as fraction of predictions that are correct
# Accuracy:   79.65%

# Fast Partition; threshold = 0.5
#
# --------------------------------
# "Confusion Matrix for Logistic Regression"
#            Actual
# Predicted   Attend NotAttend
# Attend      691       227
# NotAttend   262      1715
# Accuracy defined as fraction of predictions that are correct
# Accuracy:   83.11% 
#
# --------------------------------
# "Confusion Matrix Naive Bayes"
#            Actual
# Predicted   Attend NotAttend
# Attend      572       211 
# NotAttend   381      1731 
# Accuracy defined as fraction of predictions that are correct
# Accuracy:   79.55%

# Exact Partition;  threshold = 0.7
#
# --------------------------------
# "Confusion Matrix for Logistic Regression"
#            Actual
# Predicted   Attend NotAttend
# Attend      498        81
# NotAttend   449      1852
# Accuracy defined as fraction of predictions that are correct
# Accuracy:   81.60%
#
# --------------------------------
# "Confusion Matrix Naive Bayes"
#            Actual
# Predicted   Attend NotAttend
# Attend      425       100
# NotAttend   522      1833
# Accuracy defined as fraction of predictions that are correct
# Accuracy:   78.40%