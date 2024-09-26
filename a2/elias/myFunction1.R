source("R/WheresCroc.R")

myFunction <- function(moveInfo, readings, positions, edges, dnorm_params) {
    nr_nodes <- dim(dnorm_params$salinity)[1]
    if (!"last_p" %in% names(moveInfo)) {
        moveInfo$last_p <- rep(1/nr_nodes, nr_nodes)
    }
    p <- hmm(readings, dnorm_params, nr_nodes, edges, moveInfo, positions)
    p <- apply_backpacker_info(positions, p, nr_nodes)
    
    pos_ranger <- positions[3]
    targets <- get_targets(p)
    move_1 <- get_move(pos_ranger, targets, readings, edges, p)
    if (move_1 == 0) {
        p[pos_ranger] <- nearly_zero()
        targets <- get_targets(p)
        move_2 <- get_move(pos_ranger, targets, readings, edges, p)
    } else {
        move_2 <- get_move(move_1, targets, readings, edges, p)
    }
    if (move_2 == 0) {
        p[move_1] <- nearly_zero()
        targets <- get_targets(p)
    }
    
    #cat("\n", sum(moveInfo$last_p))
    moveInfo$last_p <- normalize(p)
    moveInfo$moves <- c(move_1, move_2)
    cat(
        "\n",
        "\n", positions, "|", getOptions(positions[3], edges),
        "\n", targets, "|", moveInfo$last_p[targets],
        "\n", moveInfo$moves
        )
    return(moveInfo)
}

hmm <- function(readings, dnorm_params, nr_nodes, edges, moveInfo, positions) {
    p_obs <- p_obs(readings, dnorm_params, nr_nodes)
    p_neighboring <- get_p_neighboring(edges, moveInfo$last_p, nr_nodes)
    #p_neighboring <- obsolete_get_p_neighboring(edges, moveInfo$last_p, nr_nodes)
    p <- p_obs * p_neighboring
    return(p)
}

find_path_to_target <- function(i_node, targets, edges, p, record=c()) {
    if (i_node %in% targets[1]) {
        return(length(record))
    } else {
        record <- c(record, i_node)
        options <- get_options(i_node, edges)
        options <- options[!(options %in% record)]
        record_lengths <- c()
        for (option in options) {
            record_lengths <- c(record_lengths, get_move(option, targets, edges, p, record))
        }
        return(min(record_lengths))
    }
}

get_move <- function(i_node, targets, edges, p, record=c()) {
    if (i_node %in% targets) {
        return(0)  # alt: ml next
    } else {
        record <- i_node
        options <- 
        
    }
}

#get_move <- function(i_node, targets, readings, edges, p) {
#    options <- getOptions(i_node, edges)
#    if (i_node %in% targets) {
#        move <- 0
#    } else {
#        targets_in_options <- targets[targets %in% options]
#        p_targets_in_options <- normalize(p)[targets_in_options]
#        if (length(targets_in_options) > 0) {
#            move <- targets_in_options[which(p_targets_in_options == max(p_targets_in_options))][1]
#        } else if (all(options < targets[1])) {
#            move <- max(options)
#        } else if (all(options > targets[1])) {
#            move <- min(options)
#        }
#        else {
#            move <- sample(options[which(abs(options-targets[1]) == min(abs(options-targets[1])))], 1)
#        }
#    }
#    return(move)
#}


apply_backpacker_info <- function(positions, p, nr_nodes) {
    if ((!is.na(positions[1]) && positions[1] < 0) | (!is.na(positions[2]) && positions[2] < 0)) {
        p <- replicate(nr_nodes, nearly_zero())
        p[abs(positions[which(positions < 0)])] <- 1
    }
    if (!is.na(positions[1]) && positions[1] > 0) {
        p[positions[1]] <- nearly_zero()
    }
    if (!is.na(positions[2]) && positions[2] > 0) {
        p[positions[2]] <- nearly_zero()
    }
    return(p)
}

get_p_neighboring <- function(edges, last_p, nr_nodes) {
    p <- c()
    for (i in 1:nr_nodes) {
        neighbors <- getOptions(i, edges)
        contributions <- 0
        for (m in 1:length(neighbors)) {
            neighbor <- neighbors[m]
            neighbor_last_p <- last_p[neighbor]
            neighbor_p_transition <- 1 / length(getOptions(m, edges))
            contributions <- contributions + neighbor_last_p * neighbor_p_transition
        }
        p <- c(p, contributions)
    }
    return(p)
}

obsolete_get_p_neighboring <- function(edges, last_p, nr_nodes) {
    p <- c()
    for (i in 1:nr_nodes) {
        neighbors <- getOptions(i, edges)
        p <- c(p, sum(last_p[neighbors]) / length(neighbors)) 
    }
    return(p)
}

p_obs <- function(readings, dnorm_params, nr_nodes) {
    p <- c()
    for (i in 1:nr_nodes) {
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
    return(p)
}

normalize <- function(vector) {
    return(vector / sum(vector))
    #return((vector - min(vector)) / (max(vector) - min(vector)))
}

get_targets <- function(p) {
    nr_targets <- 3
    return(order(p, decreasing = TRUE)[1:nr_targets]) #
    #return(sample(1:40, 3)) # random targeting
}

nearly_zero <- function() {
    return(runif(1, 1e-50, 1e-49))
}

