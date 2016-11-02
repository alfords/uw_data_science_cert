# KMeans_Skeleton.R
# Copyright 2016 by Ernst Henle

# Clear Workspace
rm(list=ls())
# Clear Console:
cat("\014")

# Obtain ClusterPlot.  Obtain samples for observations, clusterCenters, and labels.
source("KMeansHelper.R")

# ClusterPlot()
# Returns a plot with randomly labelled observations and clusterCenters

# calculateClusterCenters()
# Result: 
#             [,1]        [,2]
# [1,] -0.02392857 -0.02464286
# [2,]  0.1032142  -0.10071429
# [3,] -0.08370370  0.13000000

# findLabelOfClosestCluster()
# Result:
# [1] 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3
# [23] 3 3 3 3 3 3 3 3 3 3 3 3 3 1 3 3 3 1 3 3 3 3
# [45] 3 3 3 3 3 3 3 3 3 3 1 3 3 1 3 3 3 3 2 2 3 3
# [67] 2 2 3 3 3 3 2 3 3 3 3 3 3 3 1 3 3

# KMeans()
# Result:  
#            [,1]       [,2]
# [1,]  0.0332000  0.6508000
# [2,]  1.5163158  1.0057895
# [3,] -0.7610256 -0.9071795

# KMeans is a 2D K-means implementation.  
# The function are observations that will be clustered and initial clusterCenters. 
# returns K-mean clusterCenters
# The function does not normalize the inputs. 
KMeans <- function(observations = sampleObservations, clusterCenters = centersGuess)
{   
  # Initialize the cluster labels from the previous iteration with NULL
  Put code in place of this line
  # repeat the following processes using a loop.  Prevent infinite loop with a for loop of 25 iterations
  Put code in place of this line
  {
    # For each observation find the label of its closest cluster center
    Put code in place of this line
    # Plot observations and clusterCenters
    ClusterPlot(observations, clusterCenters, currentLabels)
    # If there was no change in cluster labels, then break
    Put code in place of this line
    # For each cluster of observations determine its center
    Put code in place of this line
    # Plot observations and clusterCenters
    ClusterPlot(observations, clusterCenters,  currentLabels)
    # remember currentLabels before currentLabels is re-assigned in the next iteration
    Put code in place of this line
  } # end of the for loop
  # Return the clusterCenters
  Put code in place of this line
} # end of KMeans.

# For each cluster of observations determine its center
# The inputs are the observations and the cluster labels of the observations
# The output is a vector of the new clusterCenters
calculateClusterCenters <- function(observations=sampleObservations, clusterLabels=labelsRandom)
{
  # How many clusterCenters will we make?  What is the maximum cluster label? 
  Put code in place of this line
  # Create a matrix where each row is a cluster center.  The number of columns reflects the dimensionality of the space.
  Put code in place of this line
  # For loop through each cluster label 
  Put code in place of this line
  {
    # Get only the observations from one cluster
    Put code in place of this line
    # Determine the mean of that cluster in the 1st dimension and assign this mean
    # to the 1st dimension of the center
    Put code in place of this line
    # Determine the mean of that cluster in the 2nd dimension and assign this mean
    # to the 2nd dimension of the center
    Put code in place of this line
  } # Ends the for loop through each cluster id
  # Return the clusterCenters
  Put code in place of this line
} # end of calculateClusterCenters

# A function that returns the cluster IDs for each observation
# The function takes the observations
# The function takes clusterCenters 
# The cluster that is closest to each observation will determine the cluster ID for that observation
# A cluster ID indicates the allegiance of a observation to a cluster
findLabelOfClosestCluster <- function(observations = sampleObservations, clusterCenters=centersGuess)
{
  # Get the number of clusterCenters
  Put code in place of this line
  # Get the number of observations
  Put code in place of this line
  # Create a matrix that will contain the squared distances from each observation to each center
  # The matrix has numberOfObservations rows and numberOfClusters columns
  Put code in place of this line
  # Determine the distance from the center to each observation
  # For loop for each observation number
  Put code in place of this line
  {
    # For loop for each center number
    Put code in place of this line
    {
      # What is the difference between the current observation and the current center?
      # In other words: What is the vector between the observation and center?
      Put code in place of this line
      # What is the distance squared of this vector?
      # In other words: what is the sum of the squares of the vector elements?
      Put code in place of this line
      # If the distance squared was NA then make it infinite
      Put code in place of this line
      # Assign the distance squared to the proper element in the matrix created above
      Put code in place of this line
    } # end of the for loop for each center number
  } # end of the for loop for each observation number
  # Determine the labels of the closest center
  Put code in place of this line
} # end of findLabelOfClosestCluster
