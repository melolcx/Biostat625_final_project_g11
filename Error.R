# Set function to calculate final mode
calculate_mode <- function(file_list) {
  modes <- matrix(NA, ncol=3480,nrow = 1)
  data <- matrix(NA, nrow =100 , ncol = 3480)
  
  for (i in 1:100) {
    load(file_list[i])
    data[i,] <- bootstrap_predictions
  }
  
  for (i in 1:3480) {
    # Calculate the mode of these predictions
    modes[1,i] <- get_mode(data[,i])
  }
  return(modes)
}

# Set file name
file_names <- sprintf("%d.Rdata", 1:10000)
result <- matrix(NA, nrow = 100, ncol =3480 )

# Get 100 groups mode data
for (group_start in seq(1, 10000, by = 100)) {
  group_files <- file_names[group_start:(group_start + 99)]
  group_modes <- calculate_mode(group_files)
  result[group_start %/% 100 + 1, ] <- group_modes
}

#Calculate the variance list
calculate_row_variance <- function(matrix_row) {
    if (all(is.na(matrix_row))) {
      return(NA)
    } else {
      return(var(matrix_row, na.rm = TRUE))
    }
}

# Apply the function to each row of the matrixmax
row_variances <- apply(result, 2, calculate_row_variance)
