source("R/WheresCroc.R")

myFunction <- function(moveInfo, readings, positions, edges, dnorm_params) {
    n <- dim(dnorm_params$salinity)[1]
    if (!"last_probs" %in% names(moveInfo)) {
        moveInfo$last_probs <- rep(1/n, n)
    }
    probs_given_readings <- probs_given_readings(readings, dnorm_params, n)
    probs_neighboring <- get_probs_neighboring(edges, moveInfo$last_probs, n)
    probs <- probs_given_readings * probs_neighboring
    probs_normalized <- (probs - min(probs)) / (max(probs) - min(probs))
    
    moveInfo$last_probs <- probs_normalized
    print(moveInfo$last_probs)
    moves <- c(0, 0)
    moveInfo$moves <- moves
    return(moveInfo)
}

get_probs_neighboring <- function(edges, last_probs, n) {
    probs <- c()
    for (i in 1:n) {
        neighbors <- getOptions(i, edges)
        probs <- c(probs, sum(last_probs[neighbors]) / length(neighbors))
    }
    probs_normalized <- (probs - min(probs)) / (max(probs) - min(probs))
    return(probs_normalized)
}

probs_given_readings <- function(readings, dnorm_params, n) {
    probs <- c()
    for (i in 1:n) {
        salinity <- dnorm(readings[1],
            dnorm_params$salinity[i, 1],
            dnorm_params$salinity[i, 2])
        phosphate <- dnorm(readings[2],
            dnorm_params$phosphate[i, 1],
            dnorm_params$nitrogen[i, 2])
        nitrogen <- dnorm(readings[3],
            dnorm_params$nitrogen[i, 1],
            dnorm_params$nitrogen[i, 2])
        probs <- c(probs, prod(salinity, phosphate, nitrogen))
    }
    probs_normalized <- (probs - min(probs)) / (max(probs) - min(probs))
    return(probs_normalized)
}


