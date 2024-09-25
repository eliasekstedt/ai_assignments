source("R/WheresCroc.R")

myFunction <- function(moveInfo, readings, positions, edges, dnorm_params) {
    n <- dim(dnorm_params$salinity)[1]
    if (!"last_p" %in% names(moveInfo)) {
        moveInfo$last_p <- rep(1/n, n)
    }
    if ((!is.na(positions[1]) && positions[1] < 0) | (!is.na(positions[2]) && positions[2] < 0)) {
        p_normalized <- rep(0, n)
        p_normalized[abs(positions[which(positions < 0)])] <- 1
        #print(p_normalized)
        #Sys.sleep(10)
    } else {
        p_obs <- p_obs(readings, dnorm_params, n)
        p_neighboring <- get_p_neighboring(edges, moveInfo$last_p, n)
        p <- p_obs * p_neighboring
        p_normalized <- normalize(p)
    }
    #print(positions)
    #if (!is.na(positions[1]) && positions[1] > 0) {
    #    #print("bp1")
    #    p_normalized[positions[1]] <- 1e-50
    #}
    #if (!is.na(positions[2]) && positions[2] > 0) {
    #    #print("bp2")
    #    p_normalized[positions[2]] <- 1e-50
    #}
    #print(p_normalized)
    pos_ranger <- positions[3]

    targets <- get_targets(p_normalized)

  
    move_1 <- get_move(pos_ranger, targets, readings, edges, p_normalized)
    if (move_1 == 0) {
        p_normalized[pos_ranger] <- 0
        targets <- get_targets(p_normalized)
        move_2 <- get_move(pos_ranger, targets, readings, edges, p_normalized)
    } else {
        move_2 <- get_move(move_1, targets, readings, edges, p_normalized)
    }
    if (move_2 == 0) {
        p_normalized[move_1] <- 0
        targets <- get_targets(p_normalized)
    }
    
    moveInfo$last_p <- p_normalized
    moveInfo$moves <- c(move_1, move_2)
    return(moveInfo)
}

get_move <- function(i_node, targets, readings, edges, p) {
    options <- getOptions(i_node, edges)
    if (i_node %in% targets) {
        move <- 0
    } else {
        targets_in_options <- targets[targets %in% options]
        if (length(targets_in_options) > 0) {
            tio_p <- p[targets_in_options]
            #cat("\n", i_node, ",|", targets_in_options, "|,|", tio_p, "|")
            move <- targets_in_options[which(tio_p == max(tio_p))][1]
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

get_p_neighboring <- function(edges, last_p, n) {
    p <- c()
    for (i in 1:n) {
        neighbors <- getOptions(i, edges)
        p <- c(p, sum(last_p[neighbors]) / length(neighbors)) 
    }
    return(p)
}

p_obs <- function(readings, dnorm_params, n) {
    p <- c()
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
        p <- c(p, prod(salinity, phosphate, nitrogen))
    }
    p_normalized <- normalize(p)
    return(p_normalized)
}

normalize <- function(vector) {
    return((vector - min(vector)) / (max(vector) - min(vector)))
}

get_targets <- function(p) {
    return(order(p, decreasing = TRUE)[1:3]) #
    #return(sample(1:40, 3)) # random targeting
}