test <- read.csv("finaltest.csv")
get_mode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}
result_2 <- list()
# Initialize a vector to store the final mode predictions
final_predictions <- matrix(0,ncol=3480,nrow = 1)
summary_predictions <- matrix(1,ncol=3480,nrow = 100)

# Calculate the mode for each observation
for (i in 1:100) {
  # Extract the ith prediction from each bootstrap sample
  file <- paste0(i,".Rdata")
  load(file)
  summary_predictions[i,] <- bootstrap_predictions
}

for (i in 1:3480) {
# Calculate the mode of these predictions
  final_predictions[1,i] <- get_mode(summary_predictions[,i])
}

error_predictions <- matrix(0,ncol=3480,nrow = 2)
#confident interval
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
a <- n/nrow(test)
af <- a
a_3 <- a

result_2 <- c(result_2,af)

#result_2 <- c(result_2,sum(final_predictions == (test$health-1))/nrow(test))
