# Task1 (b)

# Create the contingency table
sheehan_data <- matrix(c(9, 2, 0, 6), nrow = 2, byrow = TRUE,
                       dimnames = list(Age = c(">=48", "<48"),
                                       Status = c("Discharged", "Not Discharged")))

# i
# Perform Pearson's Chi-squared test
test_result <- chisq.test(sheehan_data, correct = FALSE) # correct=FALSE for Pearson's test

# Print results
print(test_result)

# ii
prop_result <-prop.test(x = c(9, 0), n = c(11, 6), correct = FALSE)

# iii
# Perform Fisher's exact test
fisher_test_result <- fisher.test(sheehan_data)

# Print results
print(fisher_test_result)

# iv.
# Updated contingency table
updated_data <- matrix(c(9, 4, 0, 6), nrow = 2, byrow = TRUE)

# install.packages("DescTools")
library(DescTools)
GTest(updated_data)

# Manual calculation of expected counts and G^2
expected <- outer(rowSums(updated_data), colSums(updated_data), "*") / sum(updated_data)
G_sq <- 2 * sum(updated_data * log(updated_data / expected), na.rm = TRUE)
p_value <- pchisq(G_sq, df = 1, lower.tail = FALSE)
print(paste("G-squared:", G_sq, "p-value:", p_value))



# Task 2
# Create the data frame for Georgia data
georgia_data <- data.frame(
  expand.grid(D = c("Yes", "No"), V = c("White", "Black"), A = 1:6),
  counts = c(2, 60, 1, 181, 2, 15, 1, 21, 6, 7, 2, 9, 9, 3, 2, 4, 9, 0, 4, 3, 17, 0, 4, 0)
)

# (a)

# Fit loglinear models
# Minimal model
mod1 <- glm(counts ~ A + V + D, data = georgia_data, family = poisson)

# Conditional independence model (fairness)
mod_fair <- glm(counts ~ A*V + A*D, data = georgia_data, family = poisson)

# Model with all two-way interactions
mod_homog <- glm(counts ~ (A + V + D)^2, data = georgia_data, family = poisson)

# Saturated model
mod_sat <- glm(counts ~ A*V*D, data = georgia_data, family = poisson)

# Compare models
anova(mod_fair, mod_homog)
summary(mod_fair)

# (b)
# Assuming mod_homog is the selected model
# Goodness of fit
summary(mod_homog) # Look at Deviance and df

# Standardized residuals
residuals(mod_homog, type = "pearson")