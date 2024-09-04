











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
    
    #print(paste0(x_dest, ",", y_dest))

    if ((x_pos == x_dest) && (y_pos == y_dest)) {
        #print("stay")
        return(5)
    }
    else if (x_pos == x_dest) {
        go_down <- as.integer(y_pos > y_dest)
        y_step <- 1 - 2 * go_down
        #print(paste0("xx go_down: ", go_down))
        #carInfo$y <- y_pos + y_step
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
        #print(paste0("yy go_left: ", go_left))
        #carInfo$x <- x_pos + x_step
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
            #print(paste0("x<y go_left: ", go_left))
            #carInfo$x <- x_pos + x_step
            if (x_pos < x_dest) {
                return(6)
            }
            else {
                return(4)
            }
        }
        else {
            #print(paste0("y<x go_down: ", go_down))
            #carInfo$y <- y_pos + y_step
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
        #print("==========")
        return(0)
    }
    else if (x_pos == x_dest) {
        go_down <- as.integer(y_pos > y_dest)
        y_step <- 1 - 2 * go_down
        #print("x_pos==x_dest")
        return(heuristic(x_pos, x_dest, y_pos + y_step, y_dest, trafficMatrix) + trafficMatrix$vroads[x_pos, y_pos - go_down])
    }
    else if (y_pos == y_dest) {
        go_left <- as.integer(x_pos > x_dest)
        x_step <- 1 - 2 * go_left
        #print("y_pos == y_dest")
        return(heuristic(x_pos + x_step, x_dest, y_pos, y_dest, trafficMatrix) + trafficMatrix$hroads[x_pos - go_left, y_pos])
    }
    else {
        #print("other")
        go_left <- as.integer(x_pos > x_dest)
        x_step <- 1 - 2 * go_left
        #stopif("cost_x", values)
        cost_x <- heuristic(x_pos + x_step, x_dest, y_pos, y_dest, trafficMatrix) + trafficMatrix$hroads[x_pos - go_left, y_pos]

        go_down <- as.integer(y_pos > y_dest)
        y_step <- 1 - 2 * go_down
        #stopif("cost_y", values)
        cost_y <- heuristic(x_pos, x_dest, y_pos + y_step, y_dest, trafficMatrix) + trafficMatrix$vroads[x_pos, y_pos - go_down]

        return(min(cost_x, cost_y))
    }
}
    














