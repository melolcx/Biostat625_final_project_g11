# Input test data
test <- read.csv("finaltest.csv")

# Creat function to calculate mode
get_mode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

# Initialize a vector to store the final mode predictions
final_predictions <- matrix(0,ncol=3480,nrow = 1)
summary_predictions <- matrix(1,ncol=3480,nrow = 100)
result_2 <- list()

# Calculate the mode for each observation
for (i in 1:100) {
  file <- paste0(i,".Rdata")
  load(file)
  summary_predictions[i,] <- bootstrap_predictions
}

# Calculate the mode of these predictions
for (i in 1:3480) {
  final_predictions[1,i] <- get_mode(summary_predictions[,i])
}

# Find confident interval
error_predictions <- matrix(0,ncol=3480,nrow = 2)
for (i in 1:3480) {
  error_predictions[1,i] <- final_predictions[1,i]-qnorm(0.975)*sqrt(row_variances[i])
  error_predictions[2,i] <- final_predictions[1,i]+qnorm(0.975)*sqrt(row_variances[i])
}
n <- 0
for (i in 1:3480) {
  if ((test$health[1]-1)<=error_predictions[2,i]&
      (test$health[1]-1)>=error_predictions[1,i]){
    n <- n+1
  }
}

# Store the result
a <- n/nrow(test)
result_2 <- c(result_2,a)
