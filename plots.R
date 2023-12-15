plot(x = c(1:5), y = result, ylim = c(0.4, 0.55), type = "l", lwd = 1,
     main = "Iteration number vs Accuracy", xlab = "Iteration number",
     ylab="Accuarcy",xaxt = "n")
points(x = c(1:5), y = result)
axis(1, at = c(1, 2, 3, 4, 5), labels = c(1, 10, 100, 1000, 2000))

plot(x = c(1:5), y = result_2, ylim = c(0.6, 0.75), type = "l", lwd = 1,
     main = "Sample Size vs Accuracy", xlab = "Sample Size",
     ylab="Accuarcy",xaxt = "n")
points(x = c(1:5), y = result_2)
axis(1, at = c(1, 2, 3, 4, 5), labels = c(1000, 2000, 5000, 10000, 20000))

sample_indices <- 500:600

library(ggplot2)

data_1 <- data.frame(index = 1:3480, 
                     Prediction = final_predictions[1,], 
                     Lower_CI = error_predictions[1,], 
                     Upper_CI = error_predictions[2,])

sample_indices <- sample(1:3480, 100)

# Create ggplot
ggplot(data = data_1[sample_indices, ], aes(x = index, y = Prediction)) +
  geom_point(aes(color = "prediction"), size = 2, show.legend = FALSE) +
  geom_errorbar(aes(ymin = Lower_CI, ymax = Upper_CI), color = "blue", width = 0.2) +
  labs(x = "Index", y = "Prediction",
       title = "Predictions with Confidence Intervals") +
  theme_minimal()+
  theme(plot.title = element_text(hjust = 0.5))
