
myFunction <- function(trafficMatrix, carInfo, packageMatrix) {
  # What is our goal?
  if(carInfo$load == 0) {
      carInfo$mem$goal <- nextPickup(trafficMatrix, carInfo, packageMatrix)
    } else {
      carInfo$mem$goal <- packageMatrix[carInfo$load, c(3,4)]
  }
  
  # How do we get there?
  carInfo$nextMove <- nextMove(trafficMatrix, carInfo, packageMatrix)
  
  ###
  # print(trafficMatrix)
  # print(packageMatrix)
  # print(carInfo)
  # stop()
  ###
  
  return(carInfo)
}


# Find the nearest pickup location for an undelivered package
nextPickup <- function(trafficMatrix, carInfo, packageMatrix) {
  distanceVector = abs(packageMatrix[,1] - carInfo$x) + abs(packageMatrix[,2] - carInfo$y)
  distanceVector[packageMatrix[,5] != 0] = Inf
  return(packageMatrix[which.min(distanceVector), c(1,2)])
}

# Find the move to get to carInfo$mem$goal
nextMove <- function(trafficMatrix, carInfo, packageMatrix) {
  if(carInfo$x < carInfo$mem$goal[1]) {
    return(6)
  } else if (carInfo$x > carInfo$mem$goal[1]) {
    return(4)
  } else if (carInfo$y < carInfo$mem$goal[2]) {
    return(8)
  } else if (carInfo$y > carInfo$mem$goal[2]) {
    return(2)
  } else {
    return(5)
  }
}














