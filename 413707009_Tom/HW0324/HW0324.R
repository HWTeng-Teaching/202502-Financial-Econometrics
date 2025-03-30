# Question 23
# Define the dataset URL
dataset_url <- "http://www.principlesofeconometrics.com/poe5/data/rdata/cocaine.rdata"

# Open a connection to the URL
connection <- url(dataset_url, "rb")  # "rb" means read in binary mode

# Load the RData file directly from the web
load(connection)

# Close the connection after loading the data
close(connection)

# Fit the regression model using the data
regression_model <- lm(price ~ quant + qual + trend, data = cocaine)
model_summary <- summary(regression_model)

# Extract the R-squared value from the summary
r_squared_value <- model_summary$r.squared

# Retrieve the coefficients table from the summary
coefficients_table <- model_summary$coefficients

# Calculate the degrees of freedom (n - k)
sample_size <- nrow(cocaine)
num_coefficients <- ncol(coefficients_table)
degrees_of_freedom <- sample_size - num_coefficients

# Calculate the one-sided critical t-value at a 5% significance level
critical_t_value <- qt(0.95, degrees_of_freedom)

# Hypothesis test for the 'quant' variable (expecting beta2 < 0)
quant_estimate <- coefficients_table["quant", "Estimate"]
quant_t_value <- coefficients_table["quant", "t value"]
quant_p_value_two_sided <- coefficients_table["quant", "Pr(>|t|)"]
quant_p_value_one_sided <- quant_p_value_two_sided / 2

# Hypothesis test for the 'qual' variable (expecting beta3 > 0)
qual_estimate <- coefficients_table["qual", "Estimate"]
qual_t_value <- coefficients_table["qual", "t value"]
qual_p_value_two_sided <- coefficients_table["qual", "Pr(>|t|)"]
qual_p_value_one_sided <- qual_p_value_two_sided / 2

# Retrieve the trend coefficient
trend_estimate <- coefficients_table["trend", "Estimate"]

# Print the regression results
cat("Regression Summary:\n")
print(model_summary)

cat("\nR-squared (Proportion of explained variation):", r_squared_value, "\n\n")

cat("Degrees of Freedom:", degrees_of_freedom, "\n")
cat("Critical t-value (One-sided, 5% level):", critical_t_value, "\n")

# Perform hypothesis test for 'quant'
cat("Hypothesis Test for quant (Expecting beta2 < 0):\n")
cat("  Estimate for beta2 (quant):", quant_estimate, "\n")
cat("  t-value:", quant_t_value, "\n")
cat("  Two-sided p-value:", quant_p_value_two_sided, "\n")
cat("  One-sided p-value (H1: beta2 < 0):", quant_p_value_one_sided, "\n")
cat("  Critical t-value for one-sided test:", critical_t_value, "\n")
if (quant_t_value < -critical_t_value) {
  cat("  Result: Reject H0. There is evidence of a quantity discount.\n\n")
} else {
  cat("  Result: Fail to reject H0. No evidence of a quantity discount.\n\n")
}

# Perform hypothesis test for 'qual'
cat("Hypothesis Test for qual (Expecting beta3 > 0):\n")
cat("  Estimate for beta3 (qual):", qual_estimate, "\n")
cat("  t-value:", qual_t_value, "\n")
cat("  Two-sided p-value:", qual_p_value_two_sided, "\n")
cat("  One-sided p-value (H1: beta3 > 0):", qual_p_value_one_sided, "\n")
cat("  Critical t-value for one-sided test:", critical_t_value, "\n")
if (qual_t_value > critical_t_value) {
  cat("  Result: Reject H0. There is evidence of a quality premium.\n\n")
} else {
  cat("  Result: Fail to reject H0. No evidence of a quality premium.\n\n")
}

# Display the trend coefficient (average annual change in price)
cat("Average Annual Change in Cocaine Price (trend coefficient):", trend_estimate, "\n")
