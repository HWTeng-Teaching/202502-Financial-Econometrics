# Define given parameters
beta0 <- 7.61733         # Intercept (unused in test)
beta1 <- 0.01309         # Estimated slope coefficient
se_beta1 <- 0.00215      # Standard error for beta1
sample_size <- 64        # Number of observations
df <- sample_size - 2    # Degrees of freedom for the t-test

# (a) Hypotheses:
# H0: beta1 = 0 (no effect)
# H1: beta1 > 0 (positive effect)

# (b) Calculate the t-statistic
t_value <- beta1 / se_beta1
cat("Calculated t-statistic:", t_value, "\n")
# Under the null hypothesis, t_value follows a t-distribution with 62 degrees of freedom.

# (c) Find the critical t-value for a 1% one-sided test
alpha <- 0.01
critical_t <- qt(1 - alpha, df)
cat("Critical t-value at the 1% significance level:", critical_t, "\n")

# (d) Compare the t-value with the critical value to decide the hypothesis
if(t_value > critical_t) {
  cat("Decision: Reject H0 at the 1% level.\n")
} else {
  cat("Decision: Fail to reject H0 at the 1% level.\n")
}

# Compute the one-tailed p-value
p_value <- pt(t_value, df, lower.tail = FALSE)
cat("One-tailed p-value:", p_value, "\n")

# Economic interpretation:
cat("Interpretation: There is a statistically significant positive relationship between GDP and medals.\n")

# (e) Recalculate t-statistic (this reiterates the earlier computation)
t_recomputed <- (beta1 - 0) / se_beta1
cat("Recomputed t-statistic:", t_recomputed, "\n")

# Final decision based on the t-statistic and the critical value
if (t_recomputed > critical_t) {
  cat("Final Conclusion: Reject H0. GDP is significantly positively associated with medals at the 1% level.\n")
} else {
  cat("Final Conclusion: Fail to reject H0. There is insufficient evidence of a positive relationship at the 1% level.\n")
}



# Question 7: Revised Regression Analysis

# Provided regression output values:
slope <- 1.029       # Estimated slope coefficient (b1)
t_slope <- 10.75     # t-value associated with the slope

se_intercept <- 2.672  # Standard error for the intercept (b0)
t_intercept <- 4.31    # t-value for the intercept

num_obs <- 51         # Total number of observations
df <- num_obs - 2     # Degrees of freedom for the regression

# (a) Compute the estimated intercept:
# The intercept is calculated by multiplying the t-value for the intercept by its standard error.
intercept <- t_intercept * se_intercept
cat("Estimated intercept (b0):", intercept, "\n")

# (b) Plot the estimated linear relationship:
# Create a sequence of values representing the BACHELOR percentages:
bachelor_percent <- seq(0, 50, length.out = 100)
# Calculate the predicted INCOME values using the regression equation:
predicted_income <- intercept + slope * bachelor_percent

# Plot the relationship:
plot(bachelor_percent, predicted_income, type = "l", col = "red", lwd = 2,
     xlab = "BACHELOR (%)", ylab = "INCOME (Thousands of Dollars)",
     main = "Estimated Relationship: INCOME vs. BACHELOR")
grid()

# (c) Calculate the standard error of the slope:
# The standard error of the slope is computed as slope divided by its t-value.
se_slope <- slope / t_slope
cat("Calculated standard error of slope:", se_slope, "\n")

# (d) Test the hypothesis for the intercept (H0: intercept = 10):
# Compute the t-statistic for testing if the intercept equals 10.
t_stat_intercept <- (intercept - 10) / se_intercept
cat("t-statistic for testing intercept = 10:", t_stat_intercept, "\n")

# (e) Compute the p-value for the two-tailed test of the intercept:
p_value_intercept <- 2 * (1 - pt(abs(t_stat_intercept), df))
cat("p-value for intercept test:", p_value_intercept, "\n")

# Visualize the rejection region for the intercept test:
curve(dt(x, df), from = -5, to = 5, col = "black", lwd = 2,
      xlab = "t-value", ylab = "Density", main = "Two-tailed Test for Intercept")
# Determine the critical t-value for a two-tailed 5% significance test:
critical_t <- qt(0.975, df)
abline(v = c(-critical_t, critical_t), col = "red", lwd = 2, lty = 2)  # Critical boundaries
abline(v = t_stat_intercept, col = "blue", lwd = 2)  # Observed t-value

# (f) Construct a 99% confidence interval for the slope:
alpha_99 <- 0.01
t_crit_99 <- qt(1 - alpha_99/2, df)
CI_99_slope <- slope + c(-1, 1) * t_crit_99 * se_slope
cat("99% Confidence Interval for slope:", CI_99_slope, "\n")

# (g) Test the hypothesis for the slope (H0: slope = 1) at the 5% level:
# Compute the t-statistic for testing if the slope equals 1.
t_stat_slope <- (slope - 1) / se_slope
# Calculate the two-tailed p-value for this test:
p_value_slope <- 2 * (1 - pt(abs(t_stat_slope), df))
cat("t-statistic for testing slope = 1:", t_stat_slope, "\n")
cat("p-value for testing slope = 1:", p_value_slope, "\n")

# Make the hypothesis decision for the slope:
if (p_value_slope < 0.05) {
  cat("Conclusion: Reject H0; the slope is significantly different from 1.\n")
} else {
  cat("Conclusion: Fail to reject H0; there is not enough evidence to conclude the slope differs from 1.\n")
}




## Question 17: Urban and Rural Model Analysis

library(ggplot2)

# --- Urban Model Parameters ---
urban_int     <- -10.76   # Coefficient 1 (urban intercept)
urban_slope   <- 2.46     # Coefficient 2 (urban slope)
urban_int_se  <- 2.27     # Standard error for urban intercept
urban_slope_se<- 0.16     # Standard error for urban slope
n_urban       <- 986

# --- Rural Model Parameters ---
rural_int     <- -4.88    # Coefficient 1 (rural intercept)
rural_slope   <- 1.80     # Coefficient 2 (rural slope)
rural_int_se  <- 3.29     # Standard error for rural intercept
rural_slope_se<- 0.24     # Standard error for rural slope
n_rural       <- 214

# (a) Hypothesis test for the urban model's slope (beta2):
#    H0: urban_slope = 1.80 versus H1: urban_slope > 1.80
alpha <- 0.05
t_stat_urban <- (urban_slope - 1.80) / urban_slope_se
crit_val_urban <- qt(1 - alpha, df = n_urban - 2)
cat("a. Urban model t-statistic:", round(t_stat_urban, 3), "\n")
cat("   Critical value (alpha =", alpha, "):", round(crit_val_urban, 3), "\n")
if (t_stat_urban > crit_val_urban) {
  cat("   Decision: Reject H0 – evidence suggests beta2 is greater than 1.80.\n\n")
} else {
  cat("   Decision: Fail to reject H0 – insufficient evidence to support beta2 > 1.80.\n\n")
}

# (b) 95% Confidence Interval for expected WAGE from the rural model when EDUC = 16
expected_wage_rural <- rural_int + rural_slope * 16
# Provided error details for the prediction:
given_se <- 0.833         # Given standard error for expected value
cov_rural <- -0.761       # Given covariance between rural coefficients
# Calculate the combined standard error:
se_rural_pred <- sqrt((rural_int_se^2) + (16^2 * rural_slope_se^2) + (2 * 16 * cov_rural))
se_rural_pred
# Use the critical t-value from the rural degrees of freedom:
crit_val_rural <- qt(0.975, df = n_rural - 2)
margin_error_rural <- crit_val_rural * se_rural_pred
ci_lower_rural <- expected_wage_rural - margin_error_rural
ci_upper_rural <- expected_wage_rural + margin_error_rural
cat("b. 95% CI for expected WAGE (rural) when EDUC = 16: [",
    round(ci_lower_rural, 2), ",", round(ci_upper_rural, 2), "]\n\n")

# (c) 95% Confidence Interval for expected WAGE using the urban model when EDUC = 16
expected_wage_urban <- urban_int + urban_slope * 16
cov_urban <- -0.345  # Provided covariance for urban coefficients
se_urban_pred <- sqrt((urban_int_se^2) + (16^2 * urban_slope_se^2) + (2 * 16 * cov_urban))
crit_val_urban_ci <- qt(0.975, df = n_urban - 2)
margin_error_urban <- crit_val_urban_ci * se_urban_pred
ci_lower_urban <- expected_wage_urban - margin_error_urban
ci_upper_urban <- expected_wage_urban + margin_error_urban
cat("c. 95% CI for expected WAGE (urban) when EDUC = 16: [",
    round(ci_lower_urban, 2), ",", round(ci_upper_urban, 2), "]\n\n")

# (d) Hypothesis test for the rural regression intercept:
#     H0: rural_int >= 4 versus H1: rural_int < 4
t_stat_rural_int <- (rural_int - 4) / rural_int_se
crit_val_rural_int <- qt(alpha, df = n_rural - 2)
cat("d. Rural intercept t-statistic:", round(t_stat_rural_int, 2), "\n")
cat("   Critical value at alpha =", alpha, ":", round(crit_val_rural_int, 3), "\n")
if (t_stat_rural_int < crit_val_rural_int) {
  cat("   Decision: Reject H0 \n\n")
} else {
  cat("   Decision: Fail to reject H0.\n\n")
}


## Question 19: MOTEL Data Analysis

# --- Data Loading ---
data_url <- "http://www.principlesofeconometrics.com/poe5/data/rdata/motel.rdata"
con <- url(data_url, "rb")  # Open a binary connection
load(con)                   # Load the dataset (assumed to be 'motel')
close(con)                  # Close the connection

# (a) Plot MOTEL_PCT and COMP_PCT versus TIME on a single graph.
#     The plot style and colors have been updated.
plot_occ <- ggplot(motel, aes(x = time)) +
  geom_line(aes(y = motel_pct, color = "Motel Occupancy (%)"), size = 1.2) +
  geom_line(aes(y = comp_pct, color = "Competitor Occupancy (%)"), size = 1.2) +
  labs(title = "Occupancy Rates Over Time",
       x = "Time (Months)",
       y = "Occupancy Rate (%)") +
  scale_color_manual(values = c("Motel Occupancy (%)" = "darkorange",
                                "Competitor Occupancy (%)" = "steelblue")) +
  theme_classic() +
  theme(plot.title = element_text(face = "bold", size = 14))
print(plot_occ)

# Estimate the regression model: MOTEL_PCT ~ COMP_PCT
model_b <- lm(motel_pct ~ comp_pct, data = motel)
summary(model_b)

# Extract and display the 95% confidence interval for the slope (beta2)
conf_interval <- confint(model_b, level = 0.95)
cat("95% CI for beta2 (slope):", round(conf_interval[2, 1], 4), "to",
    round(conf_interval[2, 2], 4), "\n\n")

# (b) Construct a 90% confidence interval for the expected MOTEL_PCT given COMP_PCT = 70
alpha_90 <- 0.10
t_crit_90 <- qt(1 - alpha_90/2, df = df.residual(model_b))
# Extract coefficients and their variance-covariance components
b0_hat   <- coef(model_b)[1]
b1_hat   <- coef(model_b)[2]
var_b0   <- vcov(model_b)[1, 1]
var_b1   <- vcov(model_b)[2, 2]
cov_b0b1 <- vcov(model_b)[1, 2]
# For COMP_PCT = 70:
comp_val <- 70
expected_motel <- b0_hat + b1_hat * comp_val
# Variance for the linear prediction
var_lin <- var_b0 + (comp_val^2 * var_b1) + (2 * comp_val * cov_b0b1)
se_lin <- sqrt(var_lin)
margin_error_90 <- t_crit_90 * se_lin
ci_lower_90 <- expected_motel - margin_error_90
ci_upper_90 <- expected_motel + margin_error_90
cat("b. 90% CI for expected MOTEL_PCT when COMP_PCT =", comp_val, ": [",
    round(ci_lower_90, 3), ",", round(ci_upper_90, 3), "]\n\n")

# (c) Test H0: β2 ≤ 0 vs. H1: β2 > 0 at α = 0.01 using model_b
t_stat_beta2 <- (b1_hat - 0) / sqrt(var_b1)
crit_val_beta2 <- qt(1 - 0.01, df = df.residual(model_b))
cat("c. t-statistic for beta2:", round(t_stat_beta2, 2), "\n")
cat("   Critical value at α = 0.01:", round(crit_val_beta2, 3), "\n")
if (t_stat_beta2 > crit_val_beta2) {
  cat("   Conclusion: Reject H0 \n\n")
} else {
  cat("   Conclusion: Fail to reject H0 – insufficient evidence\n\n")
}

# (d) Test H0: β2 = 1 vs. H1: β2 ≠ 1 at α = 0.01
t_stat_beta2_eq1 <- (b1_hat - 1) / sqrt(var_b1)
crit_val_beta2_eq1 <- qt(1 - 0.005, df = df.residual(model_b))
cat("d. t-statistic for testing beta2 = 1:", round(t_stat_beta2_eq1, 2), "\n")
cat("   Critical value (two-tailed) at α = 0.01:", round(crit_val_beta2_eq1, 3), "\n")
if (abs(t_stat_beta2_eq1) > crit_val_beta2_eq1) {
  cat("   Conclusion: Reject H0.\n")
} else {
  cat("   Conclusion: Fail to reject H0.\n")
}

# (e) Obtain and plot the least squares residuals from the MOTEL_PCT ~ COMP_PCT regression
model_residuals <- residuals(model_b)
residual_df <- data.frame(Time = motel$time, Residuals = model_residuals)

# Create a residual plot with updated style
plot_resid <- ggplot(residual_df, aes(x = Time, y = Residuals)) +
  geom_line(color = "darkgreen", size = 1.2) +
  geom_hline(yintercept = 0, linetype = "dotted", color = "purple", size = 1) +
  labs(title = "Residuals from Regression: MOTEL_PCT on COMP_PCT",
       x = "Time (Months)",
       y = "Residuals") +
  theme_light() +
  theme(plot.title = element_text(face = "bold", size = 14))
print(plot_resid)

# Assess the predominant sign of the residuals over time
predominant_sign <- ifelse(mean(model_residuals) > 0, "Positive", "Negative")
cat("e. Predominant sign of residuals:", predominant_sign, "\n")
