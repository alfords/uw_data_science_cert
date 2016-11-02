# KMeans.R

# KMeans is a 2D K-means implementation.  
# The function are observations that will be clustered and initial clusterCenters. 
# returns K-mean clusterCenters
# The function does not normalize the inputs. 
KMeans <- function(observations = sampleObservations, clusterCenters = centersGuess)
{   
  # Initialize the cluster labels from the prvious iteration
  previousLabels <- NULL
  # repeat the following processes using a loop.  Prevent infinite loop with a for loop
  for(iteration in 1:25)
  {
    # For each observation find the label of its closest cluster center
    currentLabels <- findLabelOfClosestCluster(observations, clusterCenters)
    # Plot observations and clusterCenters
    ClusterPlot(observations, clusterCenters, currentLabels)
    # If there was no change in cluster labels, then break
    if (identical(currentLabels, previousLabels)) break
    # For each cluster of observations determine its center
    clusterCenters <- calculateClusterCenters(observations, currentLabels)
    # Plot observations and clusterCenters
    ClusterPlot(observations, clusterCenters, currentLabels)
    # remember currentLabels before currentLabels is re-assigned in the next iteration
    previousLabels <- currentLabels
  } # end of the for loop
  # Return the clusterCenters
  ClusterPlot(observations, clusterCenters, currentLabels)
  return(clusterCenters)
} # end of KMeans

# For each cluster of observations determine its center
# The inputs are the observations and the cluster labels of the observations
# The output is a vector of the new clusterCenters
calculateClusterCenters <- function(observations=sampleObservations, clusterLabels=labelsRandom)
{
  # How many clusterCenters will we make?  What is the maximum cluster label? 
  uniqueLabels <- unique(clusterLabels)
  # Create a matrix where each row is a cluster center.  The numberof columns reflects the dimensionality of the space.
  clusterCenters <- matrix(nrow=length(uniqueLabels), ncol=ncol(observations))
  # For loop through each cluster label 
  for(currentLabel in uniqueLabels)
  {
    # Get only the observations from one cluster and put those in a matrix
    isCluster <- clusterLabels == currentLabel
    observationsOfCluster <- matrix(nrow=sum(isCluster), ncol=ncol(observations))
    observationsOfCluster[,] <- observations[isCluster,]
    # Determine the mean of that cluster in the 1st dimension and assign this mean
    # to the 1st dimension of the center
    clusterCenters[currentLabel, 1] <- mean(observationsOfCluster[, 1])
    # Determine the mean of that cluster in the 2nd dimension and assign this mean
    # to the 2nd dimension of the center
    clusterCenters[currentLabel, 2] <- mean(observationsOfCluster[, 2])
  } # Ends the for loop through each cluster id
  # Return the clusterCenters
  clusterCenters
} # end of calculateClusterCenters

# A function that returns the cluster IDs for each observation
# The function takes the observations
# The function takes clusterCenters 
# The cluster that is closest to each observation will determine the cluster ID for that observation
# A cluster ID indicates the allegiance of a observation to a cluster
findLabelOfClosestCluster <- function(observations = sampleObservations, clusterCenters=centersGuess)
{
  # Get the number of clusterCenters
  numberOfClusters <- nrow(clusterCenters)
  # Get the number of observations
  numberOfObservations <- nrow(observations)
  # Create a matrix that will contain the squared distances from each observation to each center
  # The matrix has numberOfObservations rows and numberOfClusters columns
  SquareOfVectorLength = matrix(nrow=numberOfObservations, ncol=numberOfClusters)
  # Determine the distance from the center to each observation
  # For loop for each observation number
  for (observationNo in 1:numberOfObservations)
  {
    # For loop for each center number
    for (clusterNo in 1:numberOfClusters)
    {
      # What is the difference between the current observation and the current center?
      # In other words: What is the vector between the observation and center?
      observationToCenter <- observations[observationNo, ] - clusterCenters[clusterNo, ]
      # What is the distance squared of this vector?
      # In other words: what is the sum of the squares of the vector elements?
      distanceSquared <- sum(observationToCenter^2)
      # If the distance squared was NA then make it infinite
      # Assign the distance squared to the proper element in the matrix created above
      if (is.na(distanceSquared)) distanceSquared <- Inf
      SquareOfVectorLength[observationNo, clusterNo] <- distanceSquared
    } # end of the for loop for each center number
  } # end of the for loop for each observation number
  # Determine the labelss of the closest
  max.col(-SquareOfVectorLength)
} # end of findLabelOfClosestCluster
