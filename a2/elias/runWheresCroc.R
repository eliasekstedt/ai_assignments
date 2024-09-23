
file_name = "R/elias_WheresCroc.R"
source(file_name)
#stop()

moveInfo <- list(move=c(0, 1, 0, 2, 0), mem=c())
readings <- 
positions <- 
edges <- 
probs <- 

#makeMoves <- randomWC(moveInfo, readings, positions, edges, probs)
makeMoves <- manualWC

runWheresCroc(
  makeMoves,
  doPlot = T,
  showCroc = T,
  pause = 1,
  verbose = T,
  returnMem = F,
  mem = NA
  )
