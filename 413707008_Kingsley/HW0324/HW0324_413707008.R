# Question 23
# Define the URL
url <- "http://www.principlesofeconometrics.com/poe5/data/rdata/cocaine.rdata"
# Open a connection to the URL
con <- url(url, "rb")  # "rb" = read binary mode
# Load the RData file directly from the web
load(con)
# Close the connection
close(con)

# Fit the regression model
model <- lm(price ~ quant + qual + trend, data = cocaine)
summary_model <- summary(model)

# Extract R-squared
r_squared <- summary_model$r.squared

# Extract coefficients table
coefs <- summary_model$coefficients

# Calculate degrees of freedom
n <- nrow(cocaine)
k <- length(coefs[, 1])
df <- n - k

# Calculate the one-sided critical t-value at the 5% significance level
critical_t_one_sided <- qt(0.95, df)

# Hypothesis test for quant
beta2_est <- coefs["quant", "Estimate"]
beta2_t   <- coefs["quant", "t value"]
beta2_p2s <- coefs["quant", "Pr(>|t|)"]
beta2_p1s <- beta2_p2s / 2

# Hypothesis test for qual
beta3_est <- coefs["qual", "Estimate"]
beta3_t   <- coefs["qual", "t value"]
beta3_p2s <- coefs["qual", "Pr(>|t|)"]
beta3_p1s <- beta3_p2s / 2

# The trend coefficient
beta4_est <- coefs["trend", "Estimate"]

# Print Results
cat("Regression Results:\n")
print(summary_model)

cat("\nProportion of variation explained (R-squared):", r_squared, "\n\n")

cat("Degrees of Freedom:", df, "\n")
cat("Critical t-value (One-sided, 5% level):", critical_t_one_sided, "\n")

cat("Hypothesis Test for quant (Expecting beta2 < 0):\n")
cat("  Estimate for quant (beta2):", beta2_est, "\n")
cat("  t-value:", beta2_t, "\n")
cat("  Two-sided p-value:", beta2_p2s, "\n")
cat("  One-sided p-value (H1: beta2 < 0):", beta2_p1s, "\n")
cat("  Critical t-value for one-sided test:", critical_t_one_sided, "\n")
if(beta2_t < -critical_t_one_sided) {
  cat("  Result: Reject H0. Evidence supports a quantity discount.\n\n")
} else {
  cat("  Result: Fail to reject H0. No evidence of a quantity discount.\n\n")
}

cat("Hypothesis Test for qual (Expecting beta3 > 0):\n")
cat("  Estimate for qual (beta3):", beta3_est, "\n")
cat("  t-value:", beta3_t, "\n")
cat("  Two-sided p-value:", beta3_p2s, "\n")
cat("  One-sided p-value (H1: beta3 > 0):", beta3_p1s, "\n")
cat("  Critical t-value for one-sided test:", critical_t_one_sided, "\n")
if(beta3_t > critical_t_one_sided) {
  cat("  Result: Reject H0. Evidence supports a quality premium.\n\n")
} else {
  cat("  Result: Fail to reject H0. No evidence of a quality premium.\n\n")
}

cat("Average Annual Change in Cocaine Price (trend coefficient):", beta4_est, "\n")
