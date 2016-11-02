# PregnancyExercise.R
# Copyright 2016 by Ernst Henle

#Clear Workspace
rm(list=ls())
# Clear Console:
cat("\014")

# A pregnancy test correctly predicted pregnancy 80%
# of the time among pregnant women.  10% of all the
# women were predicted pregnant but were actually not
# pregnant. The accuracy of the test was 89%. 

# Problem statement
# I  Always:  1TP + 1FP + 1FN + 1TN = 1
# II	 Recall :  TP / (TP + FN) = 0.80;  -2TP+ 0FP + 8FN + 0TN = 0
# III	Accuracy:  (TP + TN)/(TP + FP + TN + FN) = 0.89;  1TP + 0FP + 0FN + 1TN =  0.89
# IV	False Positive:  FP = 0.1; 0TP + 1FP + 0FN + 0TN = 0.1

# Problem statement expressed in terms of linear algebra:
# We want to solve the linear equation:  Ax = b
# Where:
#     A is the matrix
#     x is a vector of TP, FP, FN, TN
#     b is the right-hand side of the linear equation
# --------------    --------
#   matrix A        vector b
# --------------    --------
# TP  FP  FN  TN  | b
# --------------    --------
#  1   1   1   1  |	1
# -2   0   8   0	|	0
#  1   0   0   1	|	0.89
#  0   1   0   0	|	0.1
# --------------    --------
?solve
# A %*% x = b; x <- solve(A, b)
# inv(A) %*% b = x; x <- solve(A) %*% b

# Construct the matrix
#       TP  FP  FN  TN
r1 <- c( 1,  1,  1,  1) # I
r2 <- c(-2,  0,  8,  0) # II
r3 <- c( 100,  0,  0,  100) # III
r4 <- c( 0,  10,  0,  0) # IV
A = matrix(c(r1,r2,r3,r4), ncol=4, byrow=TRUE)
A

# The right-hand side of the linear system:
# Add code here
B <- c(1,0,89,1)
B

# Solve the equation:
x <- solve(A,B)
x
