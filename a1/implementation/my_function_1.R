













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
    #print(trafficMatrix)
    # print(packageMatrix)
    # print(carInfo)
    #stop()
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
    x_pos <- carInfo$x
    x_dest <- carInfo$mem$goal[1]
    y_pos <- carInfo$y
    y_dest <- carInfo$mem$goal[2]

    if (x_pos == x_dest && y_pos == y_dest) {
        return(5)
    }

    go_left <- as.integer(x_pos > x_dest)
    x_step <- 1 - 2 * go_left
    cost_x <- heuristic(x_pos + x_step, x_dest, y_pos, y_dest, trafficMatrix)

    go_up <- as.integer(y_pos < y_dest)
    y_step <- 2 * go_up - 1
    cost_y <- heuristic(x_pos, x_dest, y_pos + y_step, y_dest, trafficMatrix)

    available_paths <- c(2, 4, 6, 8)
    if (x_pos < x_dest) {
        available_paths <- available_paths[available_paths != 4]
    }
    if (y_pos < y_dest) {
        available_paths <- available_paths[available_paths != 2]
    }
    if (cost_y < cost_x) {
        available_paths <- available_paths[available_paths != 6]
    }
    else {
        available_paths <- available_paths[available_paths != 8]
    }
    return(available_paths[1])


}


heuristic <- function(x_pos, x_dest, y_pos, y_dest, trafficMatrix) {
    if (x_pos == x_dest && y_pos == y_dest) {
        return(0)
    }
    if (x_pos == x_dest) {
        go_up <- as.integer(y_pos < y_dest)
        y_step <- 2 * go_up - 1
        return(heuristic(x_pos, x_dest, y_pos + y_step, y_dest, trafficMatrix) + trafficMatrix$hroads[x_pos, y_pos + go_up])
    }
    else if (y_pos == y_dest) {
        go_left <- as.integer(x_pos > x_dest)
        x_step <- 1 - 2 * go_left
        return(heuristic(x_pos + x_step, x_dest, y_pos, y_dest, trafficMatrix) + trafficMatrix$vroads[x_pos-go_left, y_pos])
    }
    else {
        go_left <- as.integer(x_pos > x_dest)
        x_step <- 1 - 2 * go_left
        cost_x <- heuristic(x_pos + x_step, x_dest, y_pos, y_dest, trafficMatrix) + trafficMatrix$vroads[x_pos-go_left, y_pos]

        go_up <- as.integer(y_pos < y_dest)
        y_step <- 2 * go_up - 1
        cost_y <- heuristic(x_pos, x_dest, y_pos + y_step, y_dest, trafficMatrix) + trafficMatrix$hroads[x_pos, y_pos + go_up]

        return(min(cost_x, cost_y))
    }
}
    


    











