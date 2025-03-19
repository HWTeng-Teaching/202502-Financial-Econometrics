##################################### C03Q27_a################################################

# Load necessary libraries
library(ggplot2)
library(dplyr)
library(readr)
library(car)

# Load the dataset (ensure you have the correct file path)
motel_data <- read_csv("C:/Users/PINYKEWD/Documents/motel.csv")

# Convert column names to lowercase
colnames(motel_data) <- tolower(colnames(motel_data))

# Check column names
print(colnames(motel_data))

# Plot motel_pct and comp_pct versus time
ggplot(motel_data, aes(x = time)) +
  geom_line(aes(y = motel_pct, color = "Motel Occupancy")) +
  geom_line(aes(y = comp_pct, color = "Competitor Occupancy")) +
  labs(title = "Occupancy Rates Over Time",
       x = "Time",
       y = "Occupancy Percentage",
       color = "Legend") +
  theme_minimal()

# Estimate the regression model
model <- lm(motel_pct ~ comp_pct, data = motel_data)
summary(model)

# Construct a 95% confidence interval for beta_2
confint(model, level = 0.95)

# Assess the precision of the estimate
beta2_estimate <- coef(model)[2]
beta2_ci <- confint(model, level = 0.95)[2, ]

if (abs(beta2_ci[2] - beta2_ci[1]) < 0.1 * abs(beta2_estimate)) {
  precision <- "The estimate of beta2 is relatively precise."
} else {
  precision <- "The estimate of beta2 is not very precise."
}

print(precision)

##################################### C03Q27_b################################################

# Construct a 90% confidence interval for the expected MOTEL_PCT when COMP_PCT = 70
comp_pct_value <- data.frame(comp_pct = 70)
predicted_motel_pct <- predict(model, newdata = comp_pct_value, interval = "confidence", level = 0.90)

# Print the results
print(predicted_motel_pct)

##################################### C03Q27_c################################################

# Extract coefficient (beta_2) and standard error for COMP_PCT
beta2 <- coef(model)[2]  # Coefficient for COMP_PCT
se_beta2 <- summary(model)$coefficients[2, 2]  # Standard error for beta_2

# Compute t-statistic for testing H0: β2 ≤ 0 vs H1: β2 > 0
t_statistic <- beta2 / se_beta2

# Degrees of freedom (n - 2)
df <- nrow(motel_data) - 2

# Critical t-value for a one-sided test at alpha = 0.01
t_critical <- qt(1 - 0.01, df)

# Print results
cat("t-statistic:", t_statistic, "\n")
cat("Critical t-value at alpha = 0.01:", t_critical, "\n")

# Conclusion based on the t-statistic and critical value
if (t_statistic > t_critical) {
  cat("Reject the null hypothesis: There is sufficient evidence that β2 > 0.\n")
} else {
  cat("Fail to reject the null hypothesis: There is not enough evidence that β2 > 0.\n")
}

##################################### C03Q27_d################################################

# Extract coefficient (beta_2) and standard error for COMP_PCT
beta2 <- coef(model)[2]  # Coefficient for COMP_PCT
se_beta2 <- summary(model)$coefficients[2, 2]  # Standard error for beta_2

# Compute t-statistic for testing H0: β2 = 1 vs H1: β2 ≠ 1
t_statistic <- (beta2 - 1) / se_beta2

# Degrees of freedom (n - 2)
df <- nrow(motel_data) - 2

# Critical t-values for a two-sided test at alpha = 0.01
t_critical_upper <- qt(1 - 0.005, df)  # 0.005 for the upper tail (2.5% tail on each side)
t_critical_lower <- qt(0.005, df)     # 0.005 for the lower tail

# Print results
cat("t-statistic:", t_statistic, "\n")
cat("Critical t-values at alpha = 0.01 (two-tailed test):", t_critical_lower, "to", t_critical_upper, "\n")

# Conclusion based on the t-statistic and critical values
if (abs(t_statistic) > t_critical_upper) {
  cat("Reject the null hypothesis: There is sufficient evidence that β2 ≠ 1.\n")
} else {
  cat("Fail to reject the null hypothesis: There is not enough evidence that β2 ≠ 1.\n")
}


##################################### C03Q27_d################################################

# Fit the linear regression model
model <- lm(motel_pct ~ comp_pct, data = motel_data)

# Calculate the residuals (observed - predicted values)
motel_data$residuals <- residuals(model)

# Plot residuals against TIME
ggplot(motel_data, aes(x = time, y = residuals)) +
  geom_line(color = "blue") +
  labs(title = "Residuals of MOTEL_PCT vs. COMP_PCT over Time",
       x = "Time",
       y = "Residuals") +
  theme_minimal()

# Check the residuals during time periods 17 to 23
# Filter the data for time periods 17 to 23
residuals_time_17_23 <- motel_data %>%
  filter(time >= 17 & time <= 23)

# Calculate the mean sign of residuals during time periods 17-23
mean_sign_residuals <- mean(sign(residuals_time_17_23$residuals))

# Display the predominant sign of the residuals during the time periods
cat("The predominant sign of the residuals during time periods 17-23 is:", 
    ifelse(mean_sign_residuals > 0, "positive", "negative"), "\n")
