
source("elias/myFunction1.R")

if (T) {
  N = 500
  hmm <- testWC(
    myFunction = myFunction,
    verbose=1,
    returnVec=T,
    n=N
      )
  
  cat("\nmin   : ", min(hmm))
  cat("\nmax   : ", max(hmm))
  cat("\nmedian: ", median(hmm))
  library(ggplot2)
  df <- data.frame(values = hmm)
  ggplot(df, aes(x = values)) +
    geom_histogram(binwidth = 1, color = "black", fill = "lightblue") +
    labs(title = "nr moves per sim", x = "moves", y = "Frequency") #+
    #ylim(0, 75*N/500) +
    #xlim(0, 45)

} else {
  N <- 1
  for (i in 1:N) {
    cat("\n", "round ", i, ", begin!")
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


