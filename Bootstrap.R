library(xgboost)

i <- Sys.getenv("SLURM_ARRAY_TASK_ID")

sample_size <- 1000

train <- read.csv("finaltrain.csv")
test <-  read.csv("finaltest.csv")

original_data <- train[, -which(names(train) == c("uniqueid", "year", "personid"))]

test.predictors <- as.matrix(subset(test, select = -c(uniqueid, year, personid, health) ))


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
  bootstrap_predictions <- predicted_classes


Sys.sleep(5)
save(bootstrap_predictions, file=paste0(i, ".Rdata"))
