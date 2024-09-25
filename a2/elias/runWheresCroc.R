
source("elias/myFunction1.R")

if (T) {
  hmm <- testWC(
    myFunction = myFunction,
    verbose=1,
    returnVec=T,
    #n=5000
      )
  
  cat("\nmin   : ", min(hmm))
  cat("\nmax   : ", max(hmm))
  cat("\nmedian: ", median(hmm))
  library(ggplot2)
  df <- data.frame(values = hmm)
  ggplot(df, aes(x = values)) +
    geom_histogram(binwidth = 1, color = "black", fill = "lightblue") +
    labs(title = "nr moves per sim", x = "moves", y = "Frequency")

} else {
  for (i in 1:25) {
    print(paste0(cat("\n", "round "), i, ", begin!"))
    runWheresCroc(
      myFunction,
      doPlot = T,
      showCroc = T,
      pause = 0.1,
      verbose = T,
      returnMem = F,
      mem = NA
      ) 
  }

}


