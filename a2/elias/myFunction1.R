source("elias/WC_altered.R")

myFunction <- function(moveInfo, readings, positions, edges, dnorm_params) {
    n <- dim(dnorm_params$salinity)[1]
    if (!"last_probs" %in% names(moveInfo)) {
        moveInfo$last_probs <- rep(1/n, n)
    }
    probs_given_readings <- probs_given_readings(readings, dnorm_params, n)
    probs_neighboring <- get_probs_neighboring(edges, moveInfo$last_probs, n)
    probs <- probs_given_readings * probs_neighboring
    probs_normalized <- normalize(probs)
    pos_ranger <- positions[3]

    targets <- get_targets(probs_normalized)
  
    move_1 <- get_move(pos_ranger, targets, readings, edges, probs_normalized)
    if (move_1 == 0) {
        probs_normalized[pos_ranger] <- 0
        targets <- get_targets(probs_normalized)
        move_2 <- get_move(pos_ranger, targets, readings, edges, probs_normalized)
    } else {
        move_2 <- get_move(move_1, targets, readings, edges, probs_normalized)
    }
    if (move_2 == 0) {
        probs_normalized[move_1] <- 0
        targets <- get_targets(probs_normalized)
    }
    
    moveInfo$last_probs <- probs_normalized
    moveInfo$moves <- c(move_1, move_2)
    return(moveInfo)
}

get_move <- function(i_node, targets, readings, edges, probs) {
    options <- getOptions(i_node, edges)
    if (i_node %in% targets) {
        move <- 0
    } else {
        targets_in_options <- targets[targets %in% options]
        if (length(targets_in_options) > 0) {
        tio_probs <- probs[targets_in_options]
        #cat("\n", i_node, ",|", targets_in_options, "|,|", tio_probs, "|")
        move <- targets_in_options[which(tio_probs == max(tio_probs))]
        } else if (all(options < targets[1])) {
        move <- max(options)
        } else if (all(options > targets[1])) {
        move <- min(options)
        }
        else {
        move <- sample(options[which(abs(options-targets[1]) == min(abs(options-targets[1])))], 1)
        }
    }
    return(move)
}

get_probs_neighboring <- function(edges, last_probs, n) {
    probs <- c()
    for (i in 1:n) {
        neighbors <- getOptions(i, edges)
        probs <- c(probs, sum(last_probs[neighbors]) / length(neighbors))
    }
    probs_normalized <- normalize(probs)
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
    probs_normalized <- normalize(probs)
    return(probs_normalized)
}


normalize <- function(vector) {
    return((vector - min(vector)) / (max(vector) - min(vector)))
}

get_targets <- function(probs) {
    return(order(probs, decreasing = TRUE)[1:3])
}