
implementation = "my_function_1.R"

library("DeliveryMan")
source(paste0("implementation/", implementation))


testDM(
  manualDM,
  verbose = 0,
  returnVec = FALSE,
  n = 500,
  seed = 21,
  timeLimit = 250
)


























