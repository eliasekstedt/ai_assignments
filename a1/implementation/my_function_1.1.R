

myFunction <- function(trafficMatrix, carInfo, packageMatrix) {
  # What is our goal?
  if(carInfo$load == 0) {
    carInfo$mem$goal <- nextPickup(trafficMatrix, carInfo, packageMatrix)
  } else {
    carInfo$mem$goal <- packageMatrix[carInfo$load, c(3,4)]
  }
  
  # How do we get there?
  carInfo$nextMove <- nextMove(trafficMatrix, carInfo, packageMatrix)
  
  return(carInfo)
}

# Find the nearest pickup location for an undelivered package
nextPickup <- function(trafficMatrix, carInfo, packageMatrix) {
  # print(trafficMatrix)
  # print(carInfo)
  print("package matrix:")
  print(packageMatrix)
  
  distanceVector = abs(packageMatrix[,1] - carInfo$x) + abs(packageMatrix[,2] - carInfo$y)
  distanceVector[packageMatrix[,5] != 0] = Inf
  
  costVector <- get_cost_vector(trafficMatrix, carInfo, packageMatrix)
  print(distanceVector)
  print(costVector)
  print(distanceVector + costVector)
  stop('')
  return(packageMatrix[which.min(distanceVector), c(1,2)])
}

get_cost_vector <- function(trafficMatrix, carInfo, packageMatrix) {
  x_pos <- carInfo$x
  y_pos <- carInfo$y
  costVector <- c()
  
  for (i_row in 1:dim(packageMatrix)[1])
    x_dest <- packageMatrix[i_row,1]
    y_dest <- packageMatrix[i_row,2]
    costVector <- c(costVector, nextMove(x_pos, x_dest, y_pos, y_dest, trafficMatrix))
  return(costVector)
}

# Find the move to get to carInfo$mem$goal
nextMove <- function(x_pos, x_dest, y_pos, y_dest, trafficMatrix) {

  if ((x_pos == x_dest) && (y_pos == y_dest)) {
    return(5)
  }
  else if (x_pos == x_dest) {
    go_down <- as.integer(y_pos > y_dest)
    y_step <- 1 - 2 * go_down
    
    if (go_down == 1) {
      return(2)
    }
    else {
      return(8)
    }
  }
  else if (y_pos == y_dest) {
    go_left <- as.integer(x_pos > x_dest)
    x_step <- 1 - 2 * go_left
    if (go_left == 1) {
      return(4)
    }
    else {
      return(6)
    }
    
  }
  else {
    go_left <- as.integer(x_pos > x_dest)
    x_step <- 1 - 2 * go_left
    cost_x <- heuristic(x_pos + x_step, x_dest, y_pos, y_dest, trafficMatrix) + trafficMatrix$hroads[x_pos - go_left, y_pos]
    
    go_down <- as.integer(y_pos > y_dest)
    y_step <- 1 - 2 * go_down
    cost_y <- heuristic(x_pos, x_dest, y_pos + y_step, y_dest, trafficMatrix) + trafficMatrix$vroads[x_pos, y_pos - go_down]
    
    if (cost_x < cost_y) {
      if (x_pos < x_dest) {
        return(6)
      }
      else {
        return(4)
      }
    }
    else {
      if (y_pos < y_dest) {
        return(8)
      }
      else {
        return(2)
      }
    }
  }
}



stopif <- function(message, pos, values) {
  if (pos >= 10) {
    stop(paste0(" ", values))
  }
}


heuristic <- function(x_pos, x_dest, y_pos, y_dest, trafficMatrix) {
  if (x_pos == x_dest && y_pos == y_dest) {
    return(0)
  }
  else if (x_pos == x_dest) {
    go_down <- as.integer(y_pos > y_dest)
    y_step <- 1 - 2 * go_down
    return(heuristic(x_pos, x_dest, y_pos + y_step, y_dest, trafficMatrix) + trafficMatrix$vroads[x_pos, y_pos - go_down])
  }
  else if (y_pos == y_dest) {
    go_left <- as.integer(x_pos > x_dest)
    x_step <- 1 - 2 * go_left
    return(heuristic(x_pos + x_step, x_dest, y_pos, y_dest, trafficMatrix) + trafficMatrix$hroads[x_pos - go_left, y_pos])
  }
  else {
    go_left <- as.integer(x_pos > x_dest)
    x_step <- 1 - 2 * go_left
    cost_x <- heuristic(x_pos + x_step, x_dest, y_pos, y_dest, trafficMatrix) + trafficMatrix$hroads[x_pos - go_left, y_pos]
    
    go_down <- as.integer(y_pos > y_dest)
    y_step <- 1 - 2 * go_down
    cost_y <- heuristic(x_pos, x_dest, y_pos + y_step, y_dest, trafficMatrix) + trafficMatrix$vroads[x_pos, y_pos - go_down]
    
    return(min(cost_x, cost_y))
  }
}















