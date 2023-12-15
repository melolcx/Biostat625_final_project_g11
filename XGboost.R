library(xgboost)


complete.data <- read.csv("completedata.csv")

smp_size <- floor(0.8 * nrow(complete.data))

## Set the seed to make your partition reproducible
set.seed(625)
train_ind <- sample(seq_len(nrow(complete.data)), size = smp_size)

train <- complete.data[train_ind, ]
test <- complete.data[-train_ind, ]

write.csv(train, file = "C:/Users/Administrator/OneDrive/Nightmares/BIOSTAT 625/Final Project/train.csv")
write.csv(test, file = "C:/Users/Administrator/OneDrive/Nightmares/BIOSTAT 625/Final Project/test.csv")

## Proportion of NA
sum(is.na(complete.data))/prod(dim(complete.data))

## Parameters
train$health <- as.numeric(as.factor(train$health)) - 1

predictors <- as.matrix(train[, -which(names(train) == c("health", "uniqueid", "year", "personid"))])  # Exclude the target variable
label <- train$health

dtrain <- xgb.DMatrix(data = predictors, label = label)

params <- list(
  booster = "gbtree",
  objective = "multi:softprob",  # Use 'multi:softmax' for hard classification
  num_class = length(unique(train$health)),  # Number of classes
  eta = 0.1,
  max_depth = 6,
  subsample = 0.5,
  colsample_bytree = 0.5
)

nrounds <- 100  # Number of boosting rounds
model <- xgb.train(params = params, data = dtrain, nrounds = nrounds)

test.predictors <- as.matrix(test[, -which(names(train) == c("health", "uniqueid", "year", "personid"))])

dtest <- xgb.DMatrix(data = test.predictors)
predictions <- predict(model, dtest)

num_classes <- 5  # replace with the actual number of classes
pred_matrix <- matrix(predictions, ncol = num_classes, byrow = TRUE)

predicted_classes <- max.col(pred_matrix, ties.method = "first") - 1  # subtract 1 if classes are 0-indexed

sum(predicted_classes == (test$health-1))/nrow(test)

## Bootstrap
num_iterations <- 10
sample_size <- 10000

bootstrap_predictions <- list()

original_data = train[, -which(names(train) == c("uniqueid", "year", "personid"))]

test.predictors <- as.matrix(subset(test, select = -c(uniqueid, year, personid, health) ))

for (i in 1:num_iterations) {
  # Resample the dataset
  resampled_data <- original_data[sample(nrow(original_data), sample_size, replace = TRUE), ]
  
  # Prepare the data for XGBoost
  dtrain <- xgb.DMatrix(data = as.matrix(resampled_data[,-which(names(resampled_data) == "health")]),
                        label = as.numeric(as.factor(resampled_data$health)) - 1)
  
  # Define parameters (modify as needed)
  params <- list(
    booster = "gbtree",
    objective = "multi:softprob",
    num_class = length(unique(resampled_data$health)),
    eta = 0.1,
    max_depth = 6
  )
  
  # Train the model
  nrounds <- 100  # number of rounds
  model <- xgb.train(params = params, data = dtrain, nrounds = nrounds)
  
  # Make predictions (on the dataset of your choice, e.g., a test set)
  dtest <- xgb.DMatrix(data = test.predictors)
  predictions <- predict(model, dtest)
  
  # Convert predictions to class labels as shown in the previous response
  pred_matrix <- matrix(predictions, ncol = length(unique(resampled_data$health)), byrow = TRUE)
  predicted_classes <- max.col(pred_matrix, ties.method = "first") - 1
  
  # Store the predictions
  bootstrap_predictions[[i]] <- predicted_classes
}

## Get mode
get_mode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

# Initialize a vector to store the final mode predictions
final_predictions <- integer(length(bootstrap_predictions[[1]]))

# Calculate the mode for each observation
for (i in 1:length(final_predictions)) {
  # Extract the ith prediction from each bootstrap sample
  ith_predictions <- sapply(bootstrap_predictions, function(x) x[i])
  
  # Calculate the mode of these predictions
  final_predictions[i] <- get_mode(ith_predictions)
}

sum(final_predictions == (test$health-1))/nrow(test)