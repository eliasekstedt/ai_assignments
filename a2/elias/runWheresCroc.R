
source("R/WheresCroc.R")
source("elias/myFunction0.R")
#stop()

#myFunction <- randomWC
#myFunction <- manualWC
myFunction <- myFunction

if (T) {
testWC(
  myFunction = myFunction,
  verbose=1,
  returnVec=T,
    )
} else {
  for (i in 1:25) {
    print(paste0(cat("\n", "round "), i, ", begin!"))
    runWheresCroc(
      myFunction,
      doPlot = T,
      showCroc = T,
      pause = 1,
      verbose = T,
      returnMem = F,
      mem = NA
      ) 
  }

}
# 

