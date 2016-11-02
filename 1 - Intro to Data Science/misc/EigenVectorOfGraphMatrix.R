# EigenVectorOfGraphMatrix.R
# Copyright 2016 Ernst Henle

#Clear Workspace
rm(list=ls())
# Clear Console:
cat("\014")

# Create the basic graph matrix for the example in the lecture slides
# The self-reference for node E is not represented in this matrix
nodeNames <- c("A", "B", "C", "D", "E", "F", "G", "H", "I", "J")
#      A  B  C  D  E  F  G  H  I  J
A <- c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0)
B <- c(1, 0, 0, 0, 0, 0, 0, 0, 0, 0)
C <- c(0, 1, 0, 0, 0, 0, 0, 0, 0, 0)
D <- c(0, 0, 1, 0, 0, 0, 1, 1, 0, 0) #        A  B  C  D  E  F  G  H  I  J
E <- c(0, 0, 0, 1, 0, 0, 0, 1, 0, 0) # E <- c(0, 0, 0, 1, 1, 0, 0, 1, 0, 0)
F <- c(0, 1, 1, 0, 0, 0, 0, 0, 0, 0)
G <- c(0, 0, 1, 0, 0, 0, 0, 0, 0, 0)
H <- c(0, 0, 0, 0, 0, 0, 0, 0, 1, 0)
I <- c(0, 0, 0, 1, 0, 0, 0, 0, 0, 1)
J <- c(0, 0, 0, 0, 0, 0, 0, 0, 1, 0)
initialMatrix = matrix(c(A, B, C, D, E, F, G, H, I, J), ncol=10, byrow=TRUE)
colnames(initialMatrix) <- nodeNames
rownames(initialMatrix) <- nodeNames
initialMatrix

# Determine the basic in-degree for each node
# This in-degree is an initial guess of a nodes popularity
# This in-degree will not be used any more
inDegree <- rowSums(initialMatrix)
inDegree

# Determine the out-degree for each node.
# The out-degree is very important in adjusting the graph matrix
outDegree <- colSums(initialMatrix)
outDegree

# To apply the out-degrees to the initial matrix 
outDegreeMatrix <- matrix(rep(outDegree, times=10), nrow=10, ncol=10, byrow=TRUE)

# The elements in the initial matrix are divided by their respective out-degrees
# Note that the columns all add up to 1 (one).  That means that any given node
# can only give out 1 unit of popularity.
adjustedMatrix <- initialMatrix / outDegreeMatrix
adjustedMatrix[is.na(adjustedMatrix)] <- 0
round(adjustedMatrix, 1)

# The adjusted matrix is adjusted some more by adding a background
# This background represents the possibility of going directly from
# any node to any other node without traversing the edges.
adjustedMatrix <- adjustedMatrix + 0.01 # 0.00
adjustedMatrix


popularity <- matrix(c(1, 1, 1, 1, 1, 1, 1, 1, 1, 1), ncol=1) # inDegree # 
print(round(t(popularity), 3))

# The following calculations demonstrate an iterative method
# of approaching the popularity.  Popularities are relative
# to each other.
# Modify popularity based on adjusted graph matrix
popularity <- adjustedMatrix %*% popularity
# normalize popularity so that popularities sum up to 1
popularity <- popularity/sum(popularity)
print(round(t(popularity), 3))
# Modify popularity based on adjusted graph matrix
popularity <- adjustedMatrix %*% popularity
# normalize popularity so that popularities sum up to 1
popularity <- popularity/sum(popularity)
print(round(t(popularity), 3))
# Modify popularity based on adjusted graph matrix
popularity <- adjustedMatrix %*% popularity
# normalize popularity so that popularities sum up to 1
popularity <- popularity/sum(popularity)
print(round(t(popularity), 3))

# The following calculations demonstrate an iterative method
# of approaching the popularity 
popularity <- matrix(c(1, 1, 1, 1, 1, 1, 1, 1, 1, 1), ncol=1) # inDegree # 
for (iter in 1:1000)
{
  # Modify popularity based on adjusted graph matrix
  popularity <- adjustedMatrix %*% popularity
  # normalize popularity so that popularities sum up to 1
  popularity <- popularity/sum(popularity)
}
print(round(t(popularity), 3))

# The following calculations demonstrate an exact method
# of determining the popularity 
eigenVectors <- eigen(adjustedMatrix)$vector
eigenVector <- eigenVectors[, 1]
names(eigenVector) <- nodeNames
round(Re(eigenVector/sum(eigenVector)),3)
