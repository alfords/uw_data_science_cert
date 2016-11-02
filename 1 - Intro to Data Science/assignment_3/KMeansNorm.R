# KMeansNorm.R

KMeansNorm <- function(observations = sampleObservations, clusterCenters = centersGuess, normD1 = F, normD2 = F)
{
  if (normD1)
  {
    # Determine mean and standard deviation of 1st dimension in observations
    d1_mean <- mean(observations[,1])
    d1_sd <- sd(observations[,1])
    
    # normalize 1st dimension of observations
    observations[,1] <- (observations[,1] - d1_mean) / d1_sd
    
    # normalize 1st dimension of clusterCenters
    clusterCenters[,1] <- (clusterCenters[,1] - d1_mean) / d1_sd
  }
  if (normD2)
  {
    # Determine mean and standard deviation of 2nd dimension in observations
    d2_mean <- mean(observations[,2])
    d2_sd <- sd(observations[,2])
    
    # normalize 2nd dimension of observations
    observations[,2] <- (observations[,2] - d2_mean) / d2_sd
    
    # normalize 2nd dimension of clusterCenters
    clusterCenters[,2] <- (clusterCenters[,2] - d2_mean) / d2_sd
  }
  
  clusterCenters <- KMeans(observations, clusterCenters)
  
  if (normD1)
  {
    # denormalize in first dimension
    observations[,1] <- (observations[,1]*d1_sd) + d1_mean
    clusterCenters[,1] <- (clusterCenters[,1]*d1_sd) + d1_mean
  } 
  if (normD2)
  {
    # denormalize in second dimension
    observations[,2] <- (observations[,2]*d2_sd) + d2_mean
    clusterCenters[,2] <- (clusterCenters[,2]*d2_sd) + d2_mean
  } 
  return(clusterCenters)
}

