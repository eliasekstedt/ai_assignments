
file_name = "R/elias_WheresCroc.R"
source(file_name)
#stop()

#myFunction <- randomWC
#myFunction <- manualWC
myFunction <- myFunction

if (T) {
testWC(
  myFunction = myFunction,
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

