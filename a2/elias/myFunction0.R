
get_prob_of_node <- function(measures, means, sds) {
  nr_measures <- length(measures)
  probs_by_measure_type <- c()
  for (i in 1:nr_measures) {
    probs_by_measure_type <- c(probs_by_measure_type, dnorm(measures[i], mean=means[i], sd=sds[i]))
  }
  return(prod(probs_by_measure_type))
}

get_ml_node <- function(readings, probs) {
  nr_nodes <- dim(probs$salinity)[1]
  probs_node <- c()
  for (i_node in 1:nr_nodes) {
    means_node <- c(probs$salinity[i_node, 1], probs$phosphate[i_node, 1], probs$nitrogen[i_node, 1])
    sds_node <- c(probs$salinity[i_node, 2], probs$phosphate[i_node, 2], probs$nitrogen[i_node, 2])
    probs_node <- c(probs_node, get_prob_of_node(readings, means=means_node, sds=sds_node))
  }
  ml_node_i <- which(probs_node == max(probs_node))
  return(c(ml_node_i, probs_node[ml_node_i]))
}

get_move <- function(i_node, target, readings, edges, probs) {
  options <- getOptions(i_node, edges)
  if (i_node == target) {
    move <- 0
  } else {
    if (target %in% options) {
      move <- target
    } else if (all(options < target)) {
      move <- max(options)
    } else if (all(options > target)) {
      move <- min(options)
    }
    else {
      move <- sample(options[which(abs(options-target) == min(abs(options-target)))], 1)
    }
  }
  return(move)
}

find_moves <- function(moveInfo, readings, positions, edges, probs) {
  i_node <- positions[3]

  target <- get_ml_node(readings, probs)[1]
  move_1 <- get_move(i_node, target, readings, edges, probs)

  means_node <- c(probs$salinity[i_node, 1], probs$phosphate[i_node, 1], probs$nitrogen[i_node, 1])
  sds_node <- c(probs$salinity[i_node, 2], probs$phosphate[i_node, 2], probs$nitrogen[i_node, 2])
  prob_node <- get_prob_of_node(readings, means=means_node, sds=sds_node)
  if (prob_node > 1e-03) {
    move_2 <- 0
  } else {
    move_2 <- get_move(move_1, target, readings, edges, probs)
  }
  moveInfo$moves <- c(move_1, move_2)
  return(moveInfo)
}

myFunction <- function(moveInfo, readings, positions, edges, probs) {
  moveInfo <- find_moves(moveInfo, readings, positions, edges, probs)

  return(moveInfo)
}