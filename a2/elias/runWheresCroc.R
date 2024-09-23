
getwd()
file_name = list.files()
source(file_name)

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
