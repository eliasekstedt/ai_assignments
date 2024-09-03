
implementation = "my_function_0.R"

library("DeliveryMan")
source(paste0("implementation/", implementation))


result <- testDM(
  myFunction,
  verbose = 0,
  returnVec = FALSE,
  n = 500,
  seed = 21,
  timeLimit = 250
)

print(result)

























