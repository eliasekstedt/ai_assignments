myFunction <- function(trafficMatrix, carInfo, packageMatrix) {
    # Determine the goal based on the current load status
    if (carInfo$load == 0) {
        # If not carrying a package, find the next pickup location
        carInfo$mem$goal <- nextPickup(trafficMatrix, carInfo, packageMatrix)
    } else {
        # If carrying a package, set the goal to the delivery location
        carInfo$mem$goal <- packageMatrix[carInfo$load, c(3, 4)]
    }
    
    # Determine the next move using A* algorithm
    carInfo$nextMove <- nextMove(trafficMatrix, carInfo, packageMatrix)
    
    return(carInfo)
}

# Find the nearest pickup location for an undelivered package
nextPickup <- function(trafficMatrix, carInfo, packageMatrix) {
    # Calculate Manhattan distances from the car's current location
    distanceVector = abs(packageMatrix[,1] - carInfo$x) + abs(packageMatrix[,2] - carInfo$y)
    distanceVector[packageMatrix[,5] != 0] = Inf  # Exclude already picked-up packages
    return(packageMatrix[which.min(distanceVector), c(1, 2)])
}

# A* based move decision function to get to carInfo$mem$goal
nextMove <- function(trafficMatrix, carInfo, packageMatrix) {
    goal <- carInfo$mem$goal
    start <- list(x = carInfo$x, y = carInfo$y)
    
    # A* search to find the optimal path to the goal
    path <- aStar(trafficMatrix, start, goal)
    
    # Return the first move direction from the path
    if (length(path) > 1) {
        nextNode <- path[[2]]
        return(getMoveDirection(start, nextNode))
    } else {
        # Stay in place if no move needed (already at goal)
        return(5)
    }
}

aStar <- function(trafficMatrix, start, goal) {
    # Initialize the frontier with the start node
    frontier <- list(list(x = start$x, y = start$y, g = 0, h = heuristic(start, goal), 
                          f = heuristic(start, goal), parent = NULL))
    
    # Explored set to keep track of visited nodes
    explored <- list()
    
    while (length(frontier) > 0) {
        # Find the node with the smallest f=g+h value in the frontier
        best_index <- which.min(sapply(frontier, function(node) node$f)) 
        current_node <- frontier[[best_index]] 
        frontier <- frontier[-best_index]  # Remove current node from the frontier
        
        # Check if goal has been reached
        if (current_node$x == goal[1] && current_node$y == goal[2]) {
            return(extractPath(current_node))  # Extract the path if goal is reached
        }
        
        # Add current node to explored set
        explored <- c(explored, list(current_node))
        
        # Expand current node (get neighbors)
        neighbors <- getNeighbors(current_node, trafficMatrix)
        
        for (neighbor in neighbors) {
            # Calculate g, h, and f values for the neighbor
            neighbor$g <- current_node$g + getCost(current_node, neighbor, trafficMatrix)
            neighbor$h <- heuristic(neighbor, goal)
            neighbor$f <- neighbor$g + neighbor$h
            
            # Skip neighbor if it has already been explored
            if (isExplored(neighbor, explored)) next
            
            # Check if neighbor is already in the frontier
            existing_index <- getFrontierIndex(neighbor, frontier)
            
            if (!is.null(existing_index)) {
                # If neighbor is in the frontier, update it if this path is cheaper
                if (neighbor$f < frontier[[existing_index]]$f) {
                    frontier[[existing_index]] <- neighbor
                }
            } else {
                # If neighbor is not in the frontier, add it
                frontier <- c(frontier, list(neighbor))
            }
        }
    }
    
    return(list(start))  # Return starting point if no path is found
}

# Check if the neighbor is already explored
isExplored <- function(neighbor, explored) {
    return(any(sapply(explored, function(n) n$x == neighbor$x && n$y == neighbor$y)))
}

# Get the index of the neighbor in the frontier if it exists, otherwise return NULL
getFrontierIndex <- function(neighbor, frontier) {
    # Find matching indices where neighbor's coordinates match with any node in the frontier
    match_indices <- sapply(frontier, function(n) n$x == neighbor$x && n$y == neighbor$y)
    
    # If a match is found, return the index of the first match
    if (any(match_indices)) {
        existing_index <- which(match_indices)[1]  # Get the first matching index
        return(existing_index)
    } else {
        return(NULL)  # No match found
    }
}


# Heuristic function: Manhattan distance
heuristic <- function(node, goal) {
    return(abs(node$x - goal[1]) + abs(node$y - goal[2]))
}

# Extract the path from the current node back to the start
extractPath <- function(node) {
    path <- list(node)
    while (!is.null(node$parent)) {
        node <- node$parent
        path <- c(list(node), path) # inserts the current parent node at the beginning of the path list
    }
    return(path)
}

# Get neighboring nodes
getNeighbors <- function(node, trafficMatrix) {
    neighbors <- list()
    x <- node$x
    y <- node$y
    
    # Define possible moves and their directions
    moves <- list(
        list(dx = -1, dy = 0, direction = 4), # Left
        list(dx = 1, dy = 0, direction = 6),  # Right
        list(dx = 0, dy = -1, direction = 2), # Down
        list(dx = 0, dy = 1, direction = 8)   # Up
    )
    
    # Check grid boundaries and add valid neighbors
    for (move in moves) {
        nx <- x + move$dx
        ny <- y + move$dy
        
        # Ensure nx and ny are within valid ranges and are numeric
        if (!is.na(nx) && !is.na(ny) && 
            nx >= 1 && nx <= 10 && 
            ny >= 1 && ny <= 10) {
            neighbors <- c(neighbors, list(list(x = nx, y = ny, parent = node)))
        }
    }
    
    return(neighbors)
}


# Calculate the movement cost between two nodes
getCost <- function(node, neighbor, trafficMatrix) {
    if (neighbor$x == node$x - 1) {
        return(trafficMatrix$hroads[node$x - 1, node$y]) # Left
    } else if (neighbor$x == node$x + 1) {
        return(trafficMatrix$hroads[node$x, node$y])     # Right
    } else if (neighbor$y == node$y - 1) {
        return(trafficMatrix$vroads[node$x, node$y - 1]) # Down
    } else if (neighbor$y == node$y + 1) {
        return(trafficMatrix$vroads[node$x, node$y])     # Up
    }
}

# Get the move direction to apply
getMoveDirection <- function(current, nextNode) {
    if (nextNode$x > current$x) return(6)  # Right
    if (nextNode$x < current$x) return(4)  # Left
    if (nextNode$y > current$y) return(8)  # Up
    if (nextNode$y < current$y) return(2)  # Down
    return(5)  # Stay
}

