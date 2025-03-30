# Question 23
# Specify the URL for the dataset
url <- "http://www.principlesofeconometrics.com/poe5/data/rdata/cocaine.rdata"

# Establish a connection to the URL
con <- url(url, "rb")  # "rb" signifies reading in binary mode

# Directly load the RData file from the web
load(con)

# Close the connection after loading the data
close(con)

# Fit the regression model using the data
model <- lm(price ~ quant + qual + trend, data = cocaine)
summary_model <- summary(model)

# Extract the R-squared value from the summary
r_squared <- summary_model$r.squared

# Retrieve the coefficients table from the summary
coefs <- summary_model$coefficients

# Calculate the degrees of freedom (n - k)
n <- nrow(cocaine)
k <- ncol(coefs)
df <- n - k

# Compute the one-tailed critical t-value at a 5% significance level
critical_t_one_sided <- qt(0.95, df)

# Hypothesis testing for the 'quant' variable (Expecting beta2 < 0)
beta2_est <- coefs["quant", "Estimate"]
beta2_t <- coefs["quant", "t value"]
beta2_p2s <- coefs["quant", "Pr(>|t|)"]
beta2_p1s <- beta2_p2s / 2

# Hypothesis testing for the 'qual' variable (Expecting beta3 > 0)
beta3_est <- coefs["qual", "Estimate"]
beta3_t <- coefs["qual", "t value"]
beta3_p2s <- coefs["qual", "Pr(>|t|)"]
beta3_p1s <- beta3_p2s / 2

# Retrieve the trend coefficient
beta4_est <- coefs["trend", "Estimate"]

# Display the regression results
cat("Regression Summary:\n")
print(summary_model)

cat("\nR-squared (Explained Variation):", r_squared, "\n\n")

cat("Degrees of Freedom:", df, "\n")
cat("Critical t-value (One-sided, 5% level):", critical_t_one_sided, "\n")

# Perform hypothesis test for 'quant'
cat("Hypothesis Test for quant (Expecting beta2 < 0):\n")
cat("  Estimated beta2 (quant):", beta2_est, "\n")
cat("  t-value:", beta2_t, "\n")
cat("  Two-sided p-value:", beta2_p2s, "\n")
cat("  One-sided p-value (H1: beta2 < 0):", beta2_p1s, "\n")
cat("  Critical t-value for one-sided test:", critical_t_one_sided, "\n")
if (beta2_t < -critical_t_one_sided) {
  cat("  Result: Reject H0. There is evidence of a quantity discount.\n\n")
} else {
  cat("  Result: Fail to reject H0. No evidence of a quantity discount.\n\n")
}

# Perform hypothesis test for 'qual'
cat("Hypothesis Test for qual (Expecting beta3 > 0):\n")
cat("  Estimated beta3 (qual):", beta3_est, "\n")
cat("  t-value:", beta3_t, "\n")
cat("  Two-sided p-value:", beta3_p2s, "\n")
cat("  One-sided p-value (H1: beta3 > 0):", beta3_p1s, "\n")
cat("  Critical t-value for one-sided test:", critical_t_one_sided, "\n")
if (beta3_t > critical_t_one_sided) {
  cat("  Result: Reject H0. There is evidence of a quality premium.\n\n")
} else {
  cat("  Result: Fail to reject H0. No evidence of a quality premium.\n\n")
}

# Display the trend coefficient (average annual change in price)
cat("Average Annual Change in Cocaine Price (trend coefficient):", beta4_est, "\n")
