
implementation = "my_function_1.R"

library("DeliveryMan")
source(paste0("implementation/", implementation))


view = F # (does not work in vs code)
if (view) {
    runDeliveryMan(
       carReady = myFunction,
       dim = 10,
       turns = 2000,
       doPlot = T,
       pause = 0.1,
       del = 5,
       verbose = T
    )
} else {
    testDM(
      myFunction,
      verbose = 0,
      returnVec = FALSE,
      n = 500,
      seed = 21,
      timeLimit = 250
    )
}

























