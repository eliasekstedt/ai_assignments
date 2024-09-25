# Install and load ggplot2 (if not installed)
#install.packages("ggplot2")

# # Example hmm
# hmm <- c(1, 2, 2, 3, 4, 4, 4, 5, 5, 6, 6, 7, 8, 9, 10)
# 
# # Create a data frame from the hmm
library(ggplot2)
df <- data.frame(values = hmm)
ggplot(df, aes(x = values)) +
  geom_histogram(binwidth = 1, color = "black", fill = "lightblue") +
  labs(title = "Histogram of Vector Elements", x = "Values", y = "Frequency")

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


targets <- c(1, 2, 3, 4)
options <- c(5, 6)
print(length(targets[targets %in% options]))



