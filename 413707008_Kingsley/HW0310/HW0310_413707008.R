## Question 1
# Parameter estimates from the problem
b1    <- 7.61733
b2    <- 0.01309
se_b2 <- 0.00215
n     <- 64
df    <- n - 2

# (a) Hypotheses:
# H0: b2 = 0
# H1: b2 > 0

# (b) Test statistic:
t_stat <- b2 / se_b2
cat("Test statistic (t):", t_stat, "\n")

# Under H0, t ~ t_{df=62}

# (c) Critical value at 1% (one-sided):
alpha  <- 0.01
t_crit <- qt(1 - alpha, df)
cat("Critical value at 1% one-sided:", t_crit, "\n")

# (d) Decision:
if(t_stat > t_crit) {
  cat("Reject H0 at the 1% level.\n")
} else {
  cat("Fail to reject H0 at the 1% level.\n")
}

# p-value (one-sided):
p_val <- pt(t_stat, df, lower.tail = FALSE)
cat("One-sided p-value:", p_val, "\n")

# Economic conclusion:
cat("Conclusion: There is a statistically significant positive relationship between GDP and medals.\n")

# (e) The level of significance of a test is the probability of committing a Type I error.
# Compute t-statistic
t_stat <- (b2 - 0) / se_b2
cat("Test statistic (t-value):", t_stat, "\n")

# Decision on hypothesis test
if (t_stat > t_crit) {
  cat("Conclusion: Reject H0. GDP is significantly positively associated with medals won at the 1% level.\n")
} else {
  cat("Conclusion: Fail to reject H0. There is not enough evidence to conclude a positive relationship at the 1% level.\n")
}

## Question 7
# Given regression results
b1 <- 1.029  # Slope coefficient
t_b1 <- 10.75  # t-value for slope

se_b0 <- 2.672  # Standard error of intercept
t_b0 <- 4.31  # t-value for intercept

n <- 51  # Number of observations
df <- n - 2  # Degrees of freedom

# (a) Calculate the estimated intercept
b0 <- t_b0 * se_b0  # Intercept formula: b0 = t * SE
cat("Estimated intercept (b0):", b0, "\n")

# (b) Sketch the estimated relationship
bachelor_values <- seq(0, 50, length.out = 100)  # Generate BACHELOR values
income_values <- b0 + b1 * bachelor_values  # Compute estimated INCOME values

plot(bachelor_values, income_values, type="l", col="blue", lwd=2,
     xlab="BACHELOR (%)", ylab="INCOME (Thousands of Dollars)", 
     main="Estimated Relationship: INCOME vs. BACHELOR")
grid()

# (c) Calculate the standard error of the slope coefficient
# Formula: SE = b1 / t_b1
se_b1 <- b1 / t_b1
cat("Calculated standard error of slope:", se_b1, "\n")

# (d) Calculate t-statistic for testing H0: intercept = 10
# Formula: t = (b0 - 10) / se_b0
t_stat_intercept <- (b0 - 10) / se_b0
cat("t-statistic for testing intercept=10:", t_stat_intercept, "\n")

# (e) Compute p-value for two-tailed test of intercept = 10
p_value_intercept <- 2 * (1 - pt(abs(t_stat_intercept), df))
cat("p-value for intercept test:", p_value_intercept, "\n")

# Sketch the rejection region
curve(dt(x, df), from=-5, to=5, col="blue", lwd=2,
      xlab="t-value", ylab="Density", 
      main="Two-tailed Test for Intercept")
qt(0.975, df = df) # Two-tailed: 0.05/2 = 0.025, t value = 2.0096
abline(v = c(-2.0096, 2.0096), col="red", lwd=2, lty=2)  # Critical values
abline(v = t_stat_intercept, col="green", lwd=2)  # Computed t-value

# (f) Construct a 99% confidence interval for the slope
alpha_99 <- 0.01
t_crit_99 <- qt(1 - alpha_99/2, df)
CI_99_slope <- b1 + c(-1, 1) * t_crit_99 * se_b1
cat("99% Confidence Interval for slope:", CI_99_slope, "\n")

# (g) Test H0: slope = 1 at 5% significance level
t_stat_slope <- (b1 - 1) / se_b1
p_value_slope <- 2 * (1 - pt(abs(t_stat_slope), df))
cat("t-statistic for testing slope=1:", t_stat_slope, "\n")
cat("p-value for testing slope=1:", p_value_slope, "\n")

# Decision on hypothesis test
if (p_value_slope < 0.05) {
  cat("Conclusion: Reject H0; slope is significantly different from 1.\n")
} else {
  cat("Conclusion: Fail to reject H0; slope is not significantly different from 1.\n")
}

## Question 17
# Load necessary libraries
library(ggplot2)

# Urban model parameters
urban_beta1 <- -10.76
urban_beta2 <- 2.46
urban_se_beta1 <- 2.27
urban_se_beta2 <- 0.16
N_urban <- 986

# Rural model parameters
rural_beta1 <- -4.88
rural_beta2 <- 1.80
rural_se_beta1 <- 3.29
rural_se_beta2 <- 0.24
N_rural <- 214

# a. Hypothesis testing for the urban regression
# Null Hypothesis: H0: beta2 = 1.80
# Alternative Hypothesis: H1: beta2 > 1.80
alpha <- 0.05
t_statistic <- (urban_beta2 - 1.80) / urban_se_beta2
critical_value <- qt(1 - alpha, df = N_urban - 2)  # One-tailed test

cat("a. t-statistic:", round(t_statistic, 3), "\n")
cat("Critical value at alpha =", alpha, ":", round(critical_value, 3), "\n")
cat("Conclusion: ", ifelse(t_statistic > critical_value, "Reject H0: t falls in the rejection region, so we 
reject the null hypothesis and accept the alternative.", "Fail to reject H0"), "\n\n")

# b. 95% confidence interval for rural regression
expected_WAGE <- rural_beta1 + rural_beta2 * 16
std_error <- 0.833  # Given standard error
covariance <- -0.761  # Given covariance
std_error_rural <- sqrt((rural_se_beta1^2) + 16^2*(rural_se_beta2^2) + (2 * 16 * covariance))
std_error_rural
critical_value_b <- qt(1-alpha/2, df = N_rural - 2) 
# Confidence interval calculation
margin_of_error <- critical_value_b * std_error
#margin_of_error <- critical_value_b * std_error_rural
ci_lower <- expected_WAGE - margin_of_error
ci_upper <- expected_WAGE + margin_of_error
cat("b. 95% CI for expected WAGE if EDUC = 16: [", round(ci_lower, 2), ",", round(ci_upper, 2), "]\n\n")


# c. 95% confidence interval for expected WAGE using urban regression
expected_WAGE_urban <- urban_beta1 + urban_beta2 * 16
covariance <- -0.345
std_error_c <- sqrt((urban_se_beta1^2) + 16^2*(urban_se_beta2^2) + (2 * 16 * covariance))  # Combined SE
critical_value_c <- qt(0.975, df = N_urban - 2)
# Confidence interval calculation
margin_of_error_c <- critical_value_c * std_error_c
ci_lower_c <- expected_WAGE_urban - margin_of_error_c
ci_upper_c <- expected_WAGE_urban + margin_of_error_c
cat("c. 95% CI for expected WAGE if EDUC = 16 (urban): [", round(ci_lower_c, 2), ",", round(ci_upper_c, 2), "]\n\n")

# d. Hypothesis testing for the rural regression intercept
# Null Hypothesis: H0: beta1 >= 4
# Alternative Hypothesis: H1: beta1 < 4
t_statistic_d <- (rural_beta1-4) / rural_se_beta1
critical_value_d <- qt(alpha, df = N_rural - 2)  # One-tailed test

cat("d. t-statistic for intercept:", round(t_statistic_d, 2), "\n")
cat("Critical value at alpha =", alpha, ":", round(critical_value_d, 3), "\n")
cat("Conclusion: ", ifelse(t_statistic_d < critical_value_d, "Reject H0: t falls in the rejection region, so we 
reject the null hypothesis and accept the alternative.", "Fail to reject H0"), "\n")

## Question 19
# Define the URL
url <- "http://www.principlesofeconometrics.com/poe5/data/rdata/motel.rdata"
# Open a connection to the URL
con <- url(url, "rb")  # "rb" = read binary mode
# Load the RData file directly from the web
load(con)
# Close the connection
close(con)

# Assuming the dataset is already loaded as 'motel'
# a. Plot MOTEL_PCT and COMP_PCT versus TIME on the same graph
plot_a <- ggplot(motel, aes(x = time)) +
  geom_line(aes(y = motel_pct, color = "Motel Occupancy (%)"), size = 1) +
  geom_line(aes(y = comp_pct, color = "Competitor Occupancy (%)"), size = 1) +
  labs(title = "Occupancy Rates Over Time",
       x = "Time (Months)",
       y = "Occupancy Rate (%)") +
  scale_color_manual(values = c("blue", "red")) +
  theme_minimal()

print(plot_a)

# Estimate the regression model
model_b <- lm(motel_pct ~ comp_pct, data = motel)
summary(model_b)

# 95% confidence interval for the slope (beta_2)
conf_int <- confint(model_b, level = 0.95)
cat("95% CI for beta_2:", round(conf_int[2, 1], 4), "to", round(conf_int[2, 2], 4), "\n")

# b. Construct a 90% interval estimate for expected MOTEL_PCT given COMP_PCT = 70
# Required parameters
alpha <- 0.10  # for 90% CI
tcr <- qt(1 - alpha / 2, df = df.residual(model_b))  # Critical value
b1 <- coef(model_b)[1]  # Intercept
b2 <- coef(model_b)[2]  # Slope
varb1 <- vcov(model_b)[1, 1]  # Variance of intercept
varb2 <- vcov(model_b)[2, 2]  # Variance of slope
covb12 <- vcov(model_b)[1, 2]  # Covariance

# Calculate the expected value for COMP_PCT = 70
comp_pct_value <- 70
expected_value <- b1 + b2 * comp_pct_value

# Calculate the standard error for the estimated value
varL <- varb1 + (comp_pct_value^2 * varb2) + (2 * comp_pct_value * covb12)  # Variance of the linear estimate
seL <- sqrt(varL)  # Standard error of L

# Calculate the confidence interval
lowbL <- expected_value - tcr * seL
upbL <- expected_value + tcr * seL

cat("b. 90% CI for expected MOTEL_PCT given COMP_PCT =", comp_pct_value, ": [", 
    round(lowbL, 3), ",", round(upbL, 3), "]\n\n")

# c. Test the null hypothesis H0: β2 ≤ 0 against H1: β2 > 0 at the α = 0.01 level of significance
t_statistic_c <- (b2 - 0) / sqrt(varb2)  # Calculate t-statistic
critical_value_c <- qt(1 - 0.01, df = df.residual(model_b))  # One-tailed test

cat("c. t-statistic for β2:", round(t_statistic_c, 2), "\n")
cat("Critical value at α = 0.01:", round(critical_value_c, 3), "\n")
cat("Conclusion: ", ifelse(t_statistic_c > critical_value_c, "Reject H0", "Fail to reject H0"), "\n\n")

# d. Test the null hypothesis H0: β2 = 1 against H1: β2 ≠ 1 at the α = 0.01 level of significance
t_statistic_d <- (b2 - 1) / sqrt(varb2)  # Calculate t-statistic for β2 = 1
critical_value_d <- qt(1 - 0.005, df = df.residual(model_b))  # Two-tailed test

cat("d. t-statistic for β2 = 1:", round(t_statistic_d, 2), "\n")
cat("Critical value at α = 0.01 (two-tailed):", round(critical_value_d, 3), "\n")
cat("Conclusion: ", ifelse(abs(t_statistic_d) > critical_value_d, "Reject H0", "Fail to reject H0"), "\n")

# e. Calculate the least squares residuals from the regression of MOTEL_PCT on COMP_PCT
# Calculate the residuals
residuals_e <- residuals(model_b)
residuals_e

# Create a data frame for plotting
residual_data <- data.frame(TIME = motel$time, Residuals = residuals_e)

# Plot residuals against TIME
plot_e <- ggplot(residual_data, aes(x = TIME, y = Residuals)) +
  geom_line(color = "blue", size = 1) +  # Change points to a line
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  labs(title = "Residuals from MOTEL_PCT on COMP_PCT",
       x = "Time",
       y = "Residuals") +
  theme_minimal()

print(plot_e)

# Check for unusual features in the residuals
predominant_sign <- ifelse(mean(residuals_e) > 0, "Positive", "Negative")
cat("Predominant sign of residuals during the specified periods (17-23):", predominant_sign, "\n")

