# Install and load ggplot2 (if not installed)
#install.packages("ggplot2")

# # Example hmm
# hmm <- c(1, 2, 2, 3, 4, 4, 4, 5, 5, 6, 6, 7, 8, 9, 10)
# 
# # Create a data frame from the hmm
#library(ggplot2)
#df <- data.frame(values = hmm)
#ggplot(df, aes(x = values)) +
#  geom_histogram(binwidth = 1, color = "black", fill = "lightblue") +
#  labs(title = "Histogram of Vector Elements", x = "Values", y = "Frequency")

##############
# vec <- c(1, 2, 3, 3, 0)
# 
# #class(w0)
# (w0 <- which(vec == min(vec)))
# length(w0)
# w0[1]
# 
# #class(w1)
# (w1 <- which(vec == max(vec)))
# length(w1)
# w1[1]
# 
# 
# indices <- order(vec, decreasing = TRUE)[1:3]
# print(vec)
# print(indices)
#head(sort(vec, decreasing = T), 3)


#targets <- c(1, 2, 3, 4)
#options <- c(5, 6)
#print(length(targets[targets %in% options]))

#nearly_zero <- function() {
#  return(runif(1, 1e-50, 1e-49))
#}
#replicate(3, nearly_zero())

#vector <- c(1, 2, 3, 4, 5)
#nvector <- vector / sum(vector)
#print(nvector)
#print(sum(nvector))

#vtest <- function() {
#  return(c(cost=1, top_p=2))
#}

#path_yield <- vtest()
#print(path_yield)
#print(path_yield["cost"] + path_yield["top_p"])

v <- rep(Inf, 5)
print(which(v==Inf))
print(min(v))

