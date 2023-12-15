

install.packages("binom")
library(binom)

# Total number of observations
total_observations <- nrow(test)

correct_predictions <- sum(final_predictions == (test$health-1))

# Proportion of correct predictions
prop_correct <- correct_predictions / total_observations

# Calculate a confidence interval for the proportion using binom.confint
conf_interval <- binom.confint(correct_predictions, total_observations, method = "wilson")

conf_interval <- binom.confint(correct_predictions, total_observations, method = "exact")

conf_interval <- binom.confint(correct_predictions, total_observations, method = "asymptotic")

conf_interval <- binom.confint(correct_predictions, total_observations, method = "clopper-pearson")

conf_interval <- prop.test(correct_predictions, total_observations)$conf.int

boot_intervals <- boot.ci(boot_results, type = "bca")