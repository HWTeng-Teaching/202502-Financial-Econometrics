##################################### 8.16_a################################################

file_path <- "C:/Users/PINYKEWD/Documents/vacation.csv"

# Read the data (assuming CSV format)
vacation <- read.csv(file_path)

# Verify data loaded correctly
cat("First 6 rows of data:\n")
head(vacation)
cat("\nColumn names:", names(vacation))

# Step 2: Run Regression
# ----------------------------
# Fit OLS model
model <- lm(miles ~ income + age + kids, data = vacation)

# Show regression results
cat("\n\nRegression summary:\n")
print(summary(model))

# ----------------------------
# Step 3: Confidence Interval
# ----------------------------
# Get 95% CI for kids coefficient
kids_ci <- confint(model, "kids", level = 0.95)
cat("\n95% Confidence Interval for kids effect:\n")
print(kids_ci)


##################################### 8.16_b################################################

# Assuming you already have the model from earlier:
# model <- lm(miles ~ income + age + kids, data = vacation)

# Calculate residuals
residuals <- resid(model)

# Plot residuals vs INCOME
plot(vacation$income, residuals,
     xlab = "INCOME (in $1000s)",
     ylab = "Residuals",
     main = "Residuals vs INCOME")
abline(h = 0, col = "red", lty = 2)

# Plot residuals vs AGE
plot(vacation$age, residuals,
     xlab = "AGE",
     ylab = "Residuals",
     main = "Residuals vs AGE")
abline(h = 0, col = "red", lty = 2)

##################################### 8.16_c################################################

# Install and load lmtest package if not already installed
# install.packages("lmtest")
library(lmtest)

# Step 1: Sort data by increasing income
vacation_sorted <- vacation[order(vacation$income), ]

# Step 2: Subset first 90 and last 90 observations
first_90 <- vacation_sorted[1:90, ]
last_90 <- vacation_sorted[(nrow(vacation_sorted)-89):nrow(vacation_sorted), ]

# Step 3: Estimate the model on first 90 observations
model_first90 <- lm(miles ~ income + age + kids, data = first_90)
cat("Summary of model on first 90 observations:\n")
print(summary(model_first90))

# Step 4: Estimate the model on last 90 observations
model_last90 <- lm(miles ~ income + age + kids, data = last_90)
cat("\nSummary of model on last 90 observations:\n")
print(summary(model_last90))

# Step 5: Fit the full model on sorted data
full_model <- lm(miles ~ income + age + kids, data = vacation_sorted)

# Step 6: Perform Goldfeld–Quandt test for heteroskedasticity
gq_result <- gqtest(full_model, order.by = vacation_sorted$income, fraction = 0.2, alternative = "greater")

cat("\nGoldfeld–Quandt test results:\n")
print(gq_result)

# Null hypothesis (H0): Homoskedasticity (constant variance)
# Alternative hypothesis (H1): Heteroskedasticity (variance increases with income)

##################################### 8.16_d################################################

# Install packages if not already installed
# install.packages("sandwich")
# install.packages("lmtest")
install.packages("sandwich")  # Only run this once to install

library(sandwich)
library(lmtest)

# Fit the OLS model on full data
model <- lm(miles ~ income + age + kids, data = vacation)

# Obtain heteroskedasticity-robust covariance matrix (White's robust SE)
robust_cov <- vcovHC(model, type = "HC1")

# Use coeftest() to get robust standard errors and test statistics
robust_test <- coeftest(model, vcov = robust_cov)
print(robust_test)

# Extract coefficient estimate and robust standard error for 'kids'
coef_kids <- robust_test["kids", "Estimate"]
se_kids <- robust_test["kids", "Std. Error"]

# Construct 95% confidence interval for kids coefficient
alpha <- 0.05
z_crit <- qnorm(1 - alpha/2)  # 1.96 for 95% CI

ci_lower <- coef_kids - z_crit * se_kids
ci_upper <- coef_kids + z_crit * se_kids

cat("95% Robust Confidence Interval for effect of one more child (kids):\n")
cat(sprintf("[%.4f, %.4f]\n", ci_lower, ci_upper))

##################################### 8.16_e################################################

# Load required packages
# install.packages("sandwich")
# install.packages("lmtest")
library(sandwich)
library(lmtest)

# Step 1: Transform variables by dividing by income
vacation$y_star <- vacation$miles / vacation$income
vacation$income_star <- vacation$income / vacation$income  # equals 1, but keep for clarity
vacation$age_star <- vacation$age / vacation$income
vacation$kids_star <- vacation$kids / vacation$income

# Step 2: Fit GLS model by OLS on transformed variables
gls_model <- lm(y_star ~ income_star + age_star + kids_star, data = vacation)

# Step 3: Summary of GLS model (conventional standard errors)
summary(gls_model)

# Step 4: Extract coefficient and conventional SE for kids_star
coef_kids_gls <- coef(gls_model)["kids_star"]
se_kids_gls <- summary(gls_model)$coefficients["kids_star", "Std. Error"]

# Step 5: Calculate heteroskedasticity-robust SEs for GLS model
robust_cov_gls <- vcovHC(gls_model, type = "HC1")
robust_test_gls <- coeftest(gls_model, vcov = robust_cov_gls)

# Extract robust SE for kids_star
se_kids_gls_robust <- robust_test_gls["kids_star", "Std. Error"]

# Step 6: Construct 95% confidence intervals for kids effect

z_crit <- 1.96  # 95% confidence level

# Conventional GLS CI
ci_lower_gls <- coef_kids_gls - z_crit * se_kids_gls
ci_upper_gls <- coef_kids_gls + z_crit * se_kids_gls

# Robust GLS CI
ci_lower_gls_robust <- coef_kids_gls - z_crit * se_kids_gls_robust
ci_upper_gls_robust <- coef_kids_gls + z_crit * se_kids_gls_robust

cat("95% Conventional GLS Confidence Interval for effect of one more child (kids):\n")
cat(sprintf("[%.4f, %.4f]\n\n", ci_lower_gls, ci_upper_gls))

cat("95% Robust GLS Confidence Interval for effect of one more child (kids):\n")
cat(sprintf("[%.4f, %.4f]\n\n", ci_lower_gls_robust, ci_upper_gls_robust))

# Step 7: Interpretation note
cat("Note: The coefficient on kids in the GLS model corresponds to the effect on miles/income.\n")
cat("To interpret in original units, multiply the coefficient and interval by average income or consider alternative approaches.\n")

##################################### 8.18_a################################################
# Load necessary packages
library(lmtest)

# Read the data from the CSV file
cps5 <- read.csv("C:/Users/PINYKEWD/Documents/cps5.csv")

# Estimate the wage equation
wage_model <- lm(log(wage) ~ educ + exper + I(exper^2) + female + black + metro + south + midwest + west, data = cps5)

# Subset data for males and females
wage_male <- subset(cps5, female == 0)
wage_female <- subset(cps5, female == 1)

# Estimate the wage equation separately for males and females
wage_model_male <- lm(log(wage) ~ educ + exper + I(exper^2) + black + metro + south + midwest + west, data = wage_male)
wage_model_female <- lm(log(wage) ~ educ + exper + I(exper^2) + black + metro + south + midwest + west, data = wage_female)

# Goldfeld-Quandt test
gq_test_result <- gqtest(wage_model, order.by = ~female, data = cps5, alternative = "two.sided")

# Print the test result
print(gq_test_result)

# Extract test statistic and p-value
test_statistic <- gq_test_result$statistic
p_value <- gq_test_result$p.value

cat("Test Statistic:", test_statistic, "\n")
cat("P-value:", p_value, "\n")

# Determine rejection region and conclusion
alpha <- 0.05
df1 <- gq_test_result$parameter[1]
df2 <- gq_test_result$parameter[2]
critical_value <- qf(1 - alpha/2, df1, df2)

cat("Critical Value:", critical_value, "\n")

if (p_value < alpha) {
  cat("Conclusion: Reject the null hypothesis. There is evidence of heteroskedasticity related to gender.\n")
} else {
  cat("Conclusion: Fail to reject the null hypothesis. There is no significant evidence of heteroskedasticity related to gender.\n")
}


##################################### 8.18_b################################################

# Load necessary packages
library(lmtest)

# Read the data from the CSV file
cps5 <- read.csv("C:/Users/PINYKEWD/Documents/cps5.csv")

# Estimate the wage equation
wage_model <- lm(log(wage) ~ educ + exper + I(exper^2) + female + black + metro + south + midwest + west, data = cps5)

# a. NR2 test with METRO, FEMALE, BLACK as candidates
nr2_test_subset <- bptest(wage_model, ~ metro + female + black, data = cps5)

# Print the test result
print(nr2_test_subset)

# Extract test statistic and p-value
test_statistic_subset <- nr2_test_subset$statistic
p_value_subset <- nr2_test_subset$p.value

cat("Test Statistic (Subset):", test_statistic_subset, "\n")
cat("P-value (Subset):", p_value_subset, "\n")

# Determine conclusion for the subset test
alpha <- 0.01
if (p_value_subset < alpha) {
  cat("Conclusion (Subset): Reject the null hypothesis at the 1% level. There is evidence of heteroskedasticity.\n")
} else {
  cat("Conclusion (Subset): Fail to reject the null hypothesis at the 1% level. There is no significant evidence of heteroskedasticity.\n")
}

# b. NR2 test with all model explanatory variables as candidates
nr2_test_full <- bptest(wage_model, ~ educ + exper + I(exper^2) + female + black + metro + south + midwest + west, data = cps5)

# Print the test result
print(nr2_test_full)

# Extract test statistic and p-value
test_statistic_full <- nr2_test_full$statistic
p_value_full <- nr2_test_full$p.value

cat("Test Statistic (Full):", test_statistic_full, "\n")
cat("P-value (Full):", p_value_full, "\n")

# Determine conclusion for the full test
if (p_value_full < alpha) {
  cat("Conclusion (Full): Reject the null hypothesis at the 1% level. There is evidence of heteroskedasticity.\n")
} else {
  cat("Conclusion (Full): Fail to reject the null hypothesis at the 1% level. There is no significant evidence of heteroskedasticity.\n")
}

##################################### 8.18_c################################################

# Load necessary packages
library(lmtest)

# Read the data from the CSV file
cps5 <- read.csv("C:/Users/PINYKEWD/Documents/cps5.csv")

# Estimate the wage equation
wage_model <- lm(log(wage) ~ educ + exper + I(exper^2) + female + black + metro + south + midwest + west, data = cps5)

# White test for heteroskedasticity
white_test_result <- bptest(wage_model, ~ educ + exper + I(exper^2) + female + black + metro + south + midwest + west + I(educ^2) + I(exper^2) + I(female^2) + I(black^2) + I(metro^2) + I(south^2) + I(midwest^2) + I(west^2) + educ:exper + educ:female + educ:black + educ:metro + educ:south + educ:midwest + educ:west + exper:female + exper:black + exper:metro + exper:south + exper:midwest + exper:west + female:black + female:metro + female:south + female:midwest + female:west + black:metro + black:south + black:midwest + black:west + metro:south + metro:midwest + metro:west + south:midwest + south:west + midwest:west, data = cps5)

# Print the test result
print(white_test_result)

# Extract test statistic and p-value
test_statistic <- white_test_result$statistic
p_value <- white_test_result$p.value

cat("Test Statistic:", test_statistic, "\n")
cat("P-value:", p_value, "\n")

# Determine the 5% critical value for the test
degrees_of_freedom <- length(coef(wage_model)) + (length(coef(wage_model)) * (length(coef(wage_model)) - 1)) / 2 - 1  # Degrees of freedom for the Chi-squared distribution
critical_value <- qchisq(0.95, df = degrees_of_freedom)  # 5% critical value

cat("Degrees of Freedom:", degrees_of_freedom, "\n")
cat("5% Critical Value:", critical_value, "\n")

# Conclusion
alpha <- 0.05
if (test_statistic > critical_value) {
  cat("Conclusion: Reject the null hypothesis. There is evidence of heteroskedasticity.\n")
} else {
  cat("Conclusion: Fail to reject the null hypothesis. There is no significant evidence of heteroskedasticity.\n")
}

##################################### 8.18_d################################################

# Load necessary packages
library(sandwich)
library(lmtest)

# Read the data from the CSV file
cps5 <- read.csv("C:/Users/PINYKEWD/Documents/cps5.csv")

# Estimate the wage equation
wage_model <- lm(log(wage) ~ educ + exper + I(exper^2) + female + black + metro + south + midwest + west, data = cps5)

# Get conventional OLS results
ols_summary <- summary(wage_model)
ols_coef <- ols_summary$coefficients
ols_se <- ols_coef[, "Std. Error"]

# Calculate White's heteroskedasticity-robust standard errors
robust_cov <- vcovHC(wage_model, type = "HC1")
robust_se <- sqrt(diag(robust_cov))

# Create a table to compare standard errors
results_table <- data.frame(
  Coefficient = names(coef(wage_model)),
  OLS_SE = ols_se,
  Robust_SE = robust_se
)

# Calculate 95% confidence intervals
alpha <- 0.05
results_table$OLS_CI_Lower <- ols_coef[, "Estimate"] - qnorm(1 - alpha/2) * results_table$OLS_SE
results_table$OLS_CI_Upper <- ols_coef[, "Estimate"] + qnorm(1 - alpha/2) * results_table$OLS_SE
results_table$Robust_CI_Lower <- coef(wage_model) - qnorm(1 - alpha/2) * results_table$Robust_SE
results_table$Robust_CI_Upper <- coef(wage_model) + qnorm(1 - alpha/2) * results_table$Robust_SE

# Determine if interval estimates got narrower or wider
results_table$Interval_Change <- ifelse(
  (results_table$Robust_CI_Upper - results_table$Robust_CI_Lower) < (results_table$OLS_CI_Upper - results_table$OLS_CI_Lower),
  "Narrower",
  "Wider"
)

# Print the results table
print(results_table)

# Analyze inconsistencies
inconsistencies <- results_table[sign(results_table$OLS_CI_Lower) != sign(results_table$Robust_CI_Upper),]

if (nrow(inconsistencies) > 0) {
  cat("\nPotential Inconsistencies (Intervals crossing zero):\n")
  print(inconsistencies)
} else {
  cat("\nNo significant inconsistencies found.\n")
}

##################################### 8.18_e################################################
# Load necessary packages
library(sandwich)
library(lmtest)

# Read the data
cps5 <- read.csv("C:/Users/PINYKEWD/Documents/cps5.csv")

# Step 1: Fit initial OLS model
ols_model <- lm(log(wage) ~ educ + exper + I(exper^2) + female + black + metro + south + midwest + west, data = cps5)

# Step 2: Obtain squared residuals from OLS
resid_sq <- resid(ols_model)^2

# Step 3: Regress squared residuals on METRO and EXPER to model variance
aux_model <- lm(resid_sq ~ metro + exper, data = cps5)

# Step 4: Predict variance (fitted values) and compute weights = 1 / predicted variance
pred_var <- fitted(aux_model)
weights <- 1 / pred_var

# Step 5: Fit weighted least squares (FGLS) using weights
fgls_model <- lm(log(wage) ~ educ + exper + I(exper^2) + female + black + metro + south + midwest + west,
                 data = cps5, weights = weights)

# Step 6: Summary of FGLS model with conventional standard errors
summary(fgls_model)

# Step 7: Calculate heteroskedasticity-robust standard errors for FGLS model
robust_cov_fgls <- vcovHC(fgls_model, type = "HC1")
robust_se_fgls <- sqrt(diag(robust_cov_fgls))

# Step 8: Extract coefficient estimates and robust SE for 'female' (example) or any coefficient of interest
coef_fgls <- coef(fgls_model)
coef_name <- "female"  # Change as needed, e.g., "kids" or "educ"

# Construct 95% confidence intervals for all coefficients
alpha <- 0.05
z_crit <- qnorm(1 - alpha/2)

# Prepare table comparing FGLS robust CIs with OLS robust CIs from part (d)

# OLS robust SEs from part (d)
robust_cov_ols <- vcovHC(ols_model, type = "HC1")
robust_se_ols <- sqrt(diag(robust_cov_ols))

# Coefficients from OLS
coef_ols <- coef(ols_model)

# Create comparison table
comparison <- data.frame(
  Coefficient = names(coef_ols),
  OLS_Estimate = coef_ols,
  OLS_Robust_SE = robust_se_ols,
  OLS_Robust_CI_Lower = coef_ols - z_crit * robust_se_ols,
  OLS_Robust_CI_Upper = coef_ols + z_crit * robust_se_ols,
  FGLS_Estimate = coef_fgls[names(coef_ols)],
  FGLS_Robust_SE = robust_se_fgls[names(coef_ols)],
  FGLS_Robust_CI_Lower = coef_fgls[names(coef_ols)] - z_crit * robust_se_fgls[names(coef_ols)],
  FGLS_Robust_CI_Upper = coef_fgls[names(coef_ols)] + z_crit * robust_se_fgls[names(coef_ols)]
)

print(comparison)

##################################### 8.18_f################################################

# Load necessary packages
library(sandwich)
library(lmtest)

# Read the data
cps5 <- read.csv("C:/Users/PINYKEWD/Documents/cps5.csv")

# Step 1: Fit initial OLS model
ols_model <- lm(log(wage) ~ educ + exper + I(exper^2) + female + black + metro + south + midwest + west, data = cps5)

# Step 2: Obtain squared residuals from OLS
resid_sq <- resid(ols_model)^2

# Step 3: Regress squared residuals on METRO and EXPER to model variance
aux_model <- lm(resid_sq ~ metro + exper, data = cps5)

# Step 4: Predict variance (fitted values) and compute weights = 1 / predicted variance
pred_var <- fitted(aux_model)
weights <- 1 / pred_var

# Step 5: Fit weighted least squares (FGLS) using weights
fgls_model <- lm(log(wage) ~ educ + exper + I(exper^2) + female + black + metro + south + midwest + west,
                 data = cps5, weights = weights)

# Step 6: Calculate heteroskedasticity-robust standard errors for FGLS model
robust_cov_fgls <- vcovHC(fgls_model, type = "HC1")
robust_se_fgls <- sqrt(diag(robust_cov_fgls))

# Step 7: OLS with robust SE from part (d)
robust_cov_ols <- vcovHC(ols_model, type = "HC1")
robust_se_ols <- sqrt(diag(robust_cov_ols))
coef_ols <- coef(ols_model)

# Step 8: Conventional FGLS from part (e)
ols_summary_fgls <- summary(fgls_model)  # conventional SEs for FGLS
coef_fgls_conv <- coef(fgls_model)
se_fgls_conv <- ols_summary_fgls$coefficients[, "Std. Error"]  # extract conventional SEs

# Construct 95% confidence intervals
alpha <- 0.05
z_crit <- qnorm(1 - alpha/2)

# Create a data frame to store the results
results <- data.frame(
  Coefficient = names(coef(ols_model)),
  OLS_Robust_Estimate = coef_ols,
  OLS_Robust_SE = robust_se_ols,
  FGLS_Conv_Estimate = coef_fgls_conv,
  FGLS_Conv_SE = se_fgls_conv,
  FGLS_Robust_Estimate = coef(fgls_model),
  FGLS_Robust_SE = robust_se_fgls
)

# Calculate confidence intervals for each case
results$OLS_Robust_CI_Lower <- results$OLS_Robust_Estimate - z_crit * results$OLS_Robust_SE
results$OLS_Robust_CI_Upper <- results$OLS_Robust_Estimate + z_crit * results$OLS_Robust_SE
results$FGLS_Conv_CI_Lower <- results$FGLS_Conv_Estimate - z_crit * results$FGLS_Conv_SE
results$FGLS_Conv_CI_Upper <- results$FGLS_Conv_Estimate + z_crit * results$FGLS_Conv_SE
results$FGLS_Robust_CI_Lower <- results$FGLS_Robust_Estimate - z_crit * results$FGLS_Robust_SE
results$FGLS_Robust_CI_Upper <- results$FGLS_Robust_Estimate + z_crit * results$FGLS_Robust_SE

# Print the results
print(results)


