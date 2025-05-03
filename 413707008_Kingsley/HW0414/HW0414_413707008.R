library(broom)
library(knitr)
library(car)
library(lmtest)

# 8.6
# a.
# Given data
n_males <- 577
sse_males <- 97161.9174
sigma_f <- 12.024
n_females <- 423
k <- 4  # Number of parameters

# Calculate variance estimates
sigma2_males <- sse_males / (n_males - k)
sigma2_females <- sigma_f^2

# Calculate F-statistic
f_stat <- sigma2_males / sigma2_females

# Critical values for two-sided test at 5% significance level
alpha <- 0.05
f_crit_upper <- qf(1 - alpha/2, n_males - k, n_females - k)
f_crit_lower <- qf(alpha/2, n_males - k, n_females - k)

# Calculate p-value (two-sided)
p_value <- 2 * min(pf(f_stat, n_males - k, n_females - k), 
                   1 - pf(f_stat, n_males - k, n_females - k))

# Output results
cat("Variance estimate for males:", sigma2_males, "\n")
cat("Variance estimate for females:", sigma2_females, "\n")
cat("F-statistic:", f_stat, "\n")
cat("Upper critical F-value:", f_crit_upper, "\n")
cat("Lower critical F-value:", f_crit_lower, "\n")
cat("p-value:", p_value, "\n")

if (f_stat > f_crit_upper || f_stat < f_crit_lower) {
  cat("Reject the null hypothesis: Variances are different.\n")
} else {
  cat("Fail to reject the null hypothesis: Variances are equal.\n")
}

# b.
# Given data
n_single <- 400
sse_single <- 56231.0382
n_married <- 600
sse_married <- 100703.0471
k <- 5  # Number of parameters

# Calculate variance estimates
sigma2_single <- sse_single / (n_single - k)
sigma2_married <- sse_married / (n_married - k)

# Calculate F-statistic
f_stat <- sigma2_married / sigma2_single

# Critical value for one-sided test at 5% significance level
alpha <- 0.05
f_crit <- qf(1 - alpha, n_married - k, n_single - k)

# Calculate p-value (one-sided)
p_value <- 1 - pf(f_stat, n_married - k, n_single - k)

# Output results
cat("Variance estimate for single individuals:", sigma2_single, "\n")
cat("Variance estimate for married individuals:", sigma2_married, "\n")
cat("F-statistic:", f_stat, "\n")
cat("Critical F-value (one-sided):", f_crit, "\n")
cat("p-value:", p_value, "\n")

if (f_stat > f_crit) {
  cat("Reject the null hypothesis: Married individuals have greater variance.\n")
} else {
  cat("Fail to reject the null hypothesis: No evidence that married individuals have greater variance.\n")
}

# c.
# Given data
NR2 <- 59.03
df <- 4  # Degrees of freedom based on explanatory variables

# Significance level
alpha <- 0.05

# Critical chi-squared value
chi_squared_critical <- qchisq(1 - alpha, df)

# Calculate p-value
p_value <- 1 - pchisq(NR2, df)

# Output results
cat("NR^2 statistic:", NR2, "\n")
cat("Degrees of freedom:", df, "\n")
cat("Critical chi-squared value:", chi_squared_critical, "\n")
cat("P-value:", p_value, "\n")

if (NR2 > chi_squared_critical) {
  cat("Reject the null hypothesis: Evidence of heteroskedasticity.\n")
} else {
  cat("Fail to reject the null hypothesis: No evidence of heteroskedasticity.\n")
}

# d.
# Critical value at 5% significance level with 14 degrees of freedom
critical_value <- qchisq(0.95, df = 14)
print(critical_value)
# Output: 23.68479

# Calculate p-value
p_value <- 1 - pchisq(78.82, df = 14)
print(p_value)
# Output: 4.680689e-11

# e & f: Please refer to the answers in the pdf file.

#===============================================================================
# 8.16
# Define the URL
url <- "http://www.principlesofeconometrics.com/poe5/data/rdata/vacation.rdata"
# Open a connection to the URL
con <- url(url, "rb")  # "rb" = read binary mode
# Load the RData file directly from the web
load(con)
# Close the connection
close(con)

names(vacation) <- toupper(names(vacation))
# now names(vacation) is: "MILES" "INCOME" "AGE" "KIDS"

# a.
# Estimate the model by OLS
model_ols <- lm(MILES ~ INCOME + AGE + KIDS, data = vacation)

# Summary of the OLS model
summary(model_ols)

# Construct 95% confidence interval for the coefficient of KIDS
confint(model_ols, "KIDS", level = 0.95)

# b.
# Extract residuals
residuals_ols <- residuals(model_ols)

# Create plots
par(mfrow = c(1, 2))
plot(vacation$INCOME, residuals_ols, 
     main = "Residuals vs. INCOME", 
     xlab = "INCOME ($1000)", ylab = "Residuals")
abline(h = 0, lty = 2)

plot(vacation$AGE, residuals_ols, 
     main = "Residuals vs. AGE", 
     xlab = "Average Age of Adults", ylab = "Residuals")
abline(h = 0, lty = 2)
par(mfrow = c(1, 1))

# c.
# Sort data by INCOME
vacation_sorted <- vacation[order(vacation$INCOME), ]

# Split data into two groups
group1 <- vacation_sorted[1:90, ]
group2 <- vacation_sorted[111:200, ]  # Using the last 90 observations

# Estimate models for each group
model_group1 <- lm(MILES ~ INCOME + AGE + KIDS, data = group1)
model_group2 <- lm(MILES ~ INCOME + AGE + KIDS, data = group2)

# Calculate Residual Sum of Squares (RSS)
rss1 <- sum(residuals(model_group1)^2)
rss2 <- sum(residuals(model_group2)^2)

# Degrees of freedom
df1 <- length(group1$MILES) - 4  # N - K (K = number of parameters)
df2 <- length(group2$MILES) - 4

# Calculate F-statistic (rss2/df2 divided by rss1/df1)
f_statistic <- (rss2/df2) / (rss1/df1)

# Critical F-value at 5% significance level
f_critical <- qf(0.95, df2, df1)

# Calculate p-value
p_value <- 1 - pf(f_statistic, df2, df1)

# Output results
cat("Goldfeld-Quandt F-statistic:", f_statistic, "\n")
cat("Critical F-value (5%):", f_critical, "\n")
cat("p-value:", p_value, "\n")

# Decision
if (p_value < 0.05) {
  cat("Reject the null hypothesis: Evidence of heteroskedasticity.\n")
} else {
  cat("Fail to reject the null hypothesis: No evidence of heteroskedasticity.\n")
}

# d.
# 1) Regular (model‐based) standard errors
kable(
  tidy(model_ols),
  caption = "Regular standard errors in the MILES equation"
)

# 2) Robust (HC1) standard errors
#    (car::hccm computes the HC1 covariance,
#     lmtest::coeftest applies it to the coefficients)
cov_hc1 <- hccm(model_ols, type = "hc1")
robust_se <- coeftest(model_ols, vcov. = cov_hc1)

kable(
  tidy(robust_se),
  caption = "Robust (HC1) standard errors in the MILES equation"
)

# Double check
# Load necessary packages
library(sandwich)
library(lmtest)

# Get robust standard errors
robust_se <- coeftest(model_ols, vcov = vcovHC(model_ols, type = "HC1"))
print(robust_se)

# Calculate 95% confidence interval for KIDS using robust standard errors
kids_coef <- coef(model_ols)["KIDS"]
kids_robust_se <- robust_se["KIDS", "Std. Error"]
t_critical <- qt(0.975, df = model_ols$df.residual)  # Using t-distribution

kids_robust_ci <- c(kids_coef - t_critical * kids_robust_se,
                    kids_coef + t_critical * kids_robust_se)

cat("OLS Coefficient for KIDS:", kids_coef, "\n")
cat("Robust 95% CI for KIDS:", kids_robust_ci[1], "to", kids_robust_ci[2], "\n")

# Compare with original confidence interval
kids_original_ci <- confint(model_ols, "KIDS", level = 0.95)
cat("Original 95% CI for KIDS:", kids_original_ci[1], "to", kids_original_ci[2], "\n")

# e.
# Create weights (inverse of variance)
weights <- 1 / (vacation$INCOME^2)

# Weighted least squares (GLS) estimation
model_gls <- lm(MILES ~ INCOME + AGE + KIDS, data = vacation, weights = weights)

# Summary of GLS model
summary_gls <- summary(model_gls)
print(summary_gls)

# GLS confidence interval for KIDS (conventional)
kids_gls_coef <- coef(model_gls)["KIDS"]
kids_gls_se <- summary_gls$coefficients["KIDS", "Std. Error"]
t_critical <- qt(0.975, df = model_gls$df.residual)

kids_gls_ci <- c(kids_gls_coef - t_critical * kids_gls_se,
                 kids_gls_coef + t_critical * kids_gls_se)

cat("GLS Coefficient for KIDS:", kids_gls_coef, "\n")
cat("GLS 95% CI for KIDS:", kids_gls_ci[1], "to", kids_gls_ci[2], "\n")

# Robust GLS standard errors
robust_se_gls <- coeftest(model_gls, vcov = vcovHC(model_gls, type = "HC1"))
kids_robust_gls_se <- robust_se_gls["KIDS", "Std. Error"]

# Robust GLS confidence interval
kids_robust_gls_ci <- c(kids_gls_coef - t_critical * kids_robust_gls_se,
                        kids_gls_coef + t_critical * kids_robust_gls_se)

cat("Robust GLS 95% CI for KIDS:", kids_robust_gls_ci[1], "to", kids_robust_gls_ci[2], "\n")

#===============================================================================
# 8.18
# Define the URL
url <- "http://www.principlesofeconometrics.com/poe5/data/rdata/cps5.rdata"
# Open a connection to the URL
con <- url(url, "rb")  # "rb" = read binary mode
# Load the RData file directly from the web
load(con)
# Close the connection
close(con)

names(cps5) <- toupper(names(cps5))
names(cps5)

# a.
# Create the squared experience variable
cps5$EXPER2 <- cps5$EXPER^2

# Separate data for males and females
males <- subset(cps5, FEMALE == 0)
females <- subset(cps5, FEMALE == 1)

# Estimate models for each group
model_males <- lm(log(WAGE) ~ EDUC + EXPER + EXPER2 + BLACK + METRO + 
                    SOUTH + MIDWEST + WEST, data = males)
model_females <- lm(log(WAGE) ~ EDUC + EXPER + EXPER2 + BLACK + METRO + 
                      SOUTH + MIDWEST + WEST, data = females)

# Calculate Residual Sum of Squares (RSS)
rss_males <- sum(residuals(model_males)^2)
rss_females <- sum(residuals(model_females)^2)

# Degrees of freedom
df_males <- length(males$WAGE) - length(coef(model_males))
df_females <- length(females$WAGE) - length(coef(model_females))

# Calculate variance estimates
var_males <- rss_males / df_males
var_females <- rss_females / df_females

# Calculate F-statistic (ensure larger variance is in numerator for one-sided test)
if (var_males > var_females) {
  f_stat <- var_males / var_females
  df1 <- df_males
  df2 <- df_females
} else {
  f_stat <- var_females / var_males
  df1 <- df_females
  df2 <- df_males
}

# Critical F-value at 5% significance level (two-sided test)
f_crit_upper <- qf(0.975, df1, df2)
f_crit_lower <- qf(0.025, df1, df2)

# Calculate p-value (two-sided)
p_value <- 2 * min(pf(f_stat, df1, df2), 1 - pf(f_stat, df1, df2))

# Output results
cat("Variance estimate for males:", var_males, "\n")
cat("Variance estimate for females:", var_females, "\n")
cat("F-statistic:", f_stat, "\n")
cat("Upper critical F-value:", f_crit_upper, "\n")
cat("Lower critical F-value:", f_crit_lower, "\n")
cat("p-value:", p_value, "\n")

if (f_stat > f_crit_upper || f_stat < f_crit_lower) {
  cat("Reject the null hypothesis: Variances differ between males and females.\n")
} else {
  cat("Fail to reject the null hypothesis: No evidence of different variances.\n")
}

b.
# Load necessary packages
library(lmtest)

# Estimate the full model
full_model <- lm(log(WAGE) ~ EDUC + EXPER + EXPER2 + FEMALE + BLACK + 
                   METRO + SOUTH + MIDWEST + WEST, data = cps5)

# Method 1: Using bptest function
bp_test1 <- bptest(full_model, ~ METRO + FEMALE + BLACK, data = cps5)
print(bp_test1)

# Method 2: Manual implementation of NR² test
# Extract residuals and square them
residuals_sq <- residuals(full_model)^2

# Auxiliary regression
aux_model1 <- lm(residuals_sq ~ METRO + FEMALE + BLACK, data = cps5)
r_squared1 <- summary(aux_model1)$r.squared
n <- nrow(cps5)
nr_squared1 <- n * r_squared1

# Degrees of freedom (number of variables in auxiliary regression)
df1 <- 3  # METRO, FEMALE, BLACK

# Critical chi-squared value at 1% significance level
chi_crit1 <- qchisq(0.99, df1)

# p-value
p_value1 <- 1 - pchisq(nr_squared1, df1)

# Output results
cat("NR² statistic (specific variables):", nr_squared1, "\n")
cat("Critical chi-squared value (1%):", chi_crit1, "\n")
cat("p-value:", p_value1, "\n")

if (p_value1 < 0.01) {
  cat("Reject the null hypothesis: Evidence of heteroskedasticity at 1% level.\n")
} else {
  cat("Fail to reject the null hypothesis: No evidence of heteroskedasticity at 1% level.\n")
}

# Repeat test using all explanatory variables
aux_model2 <- lm(residuals_sq ~ EDUC + EXPER + EXPER2 + FEMALE + BLACK + 
                   METRO + SOUTH + MIDWEST + WEST, data = cps5)
r_squared2 <- summary(aux_model2)$r.squared
nr_squared2 <- n * r_squared2

# Degrees of freedom (number of variables in auxiliary regression)
df2 <- 9  # All explanatory variables

# p-value
p_value2 <- 1 - pchisq(nr_squared2, df2)

# Output results
cat("\nNR² statistic (all variables):", nr_squared2, "\n")
cat("Critical chi-squared value (1%):", chi_crit1, "\n")
cat("p-value:", p_value2, "\n")

if (p_value2 < 0.01) {
  cat("Reject the null hypothesis: Evidence of heteroskedasticity at 1% level.\n")
} else {
  cat("Fail to reject the null hypothesis: No evidence of heteroskedasticity at 1% level.\n")
}

# c.
# Helper function to generate White test terms
generate_white_terms <- function(data, model) {
  # Extract model matrix without intercept
  X <- model.matrix(model)[, -1]
  
  # Create data frame for original variables, squares, and cross-products
  white_data <- data.frame(X)
  
  # Add squared terms
  for (var in colnames(X)) {
    white_data[[paste0(var, "_sq")]] <- X[, var]^2
  }
  
  # Add cross-products
  vars <- colnames(X)
  for (i in 1:(length(vars)-1)) {
    for (j in (i+1):length(vars)) {
      term_name <- paste0(vars[i], "_", vars[j])
      white_data[[term_name]] <- X[, vars[i]] * X[, vars[j]]
    }
  }
  
  return(white_data)
}

# Generate White test terms
white_data <- generate_white_terms(cps5, full_model)

# Get squared residuals
white_data$residuals_sq <- residuals(full_model)^2

# Create formula for White test
white_vars <- names(white_data)[!(names(white_data) %in% c("residuals_sq"))]
white_formula <- as.formula(paste("residuals_sq ~", paste(white_vars, collapse = " + ")))

# Estimate auxiliary regression
white_model <- lm(white_formula, data = white_data)
white_r_squared <- summary(white_model)$r.squared
white_nr_squared <- n * white_r_squared

# Degrees of freedom (number of regressors excluding constant)
white_df <- length(coefficients(white_model)) - 1

# Critical chi-squared value at 5% significance level
white_crit <- qchisq(0.95, white_df)

# p-value
white_p_value <- 1 - pchisq(white_nr_squared, white_df)

# Output results
cat("White test NR² statistic:", white_nr_squared, "\n")
cat("Degrees of freedom:", white_df, "\n")
cat("Critical chi-squared value (5%):", white_crit, "\n")
cat("p-value:", white_p_value, "\n")

if (white_p_value < 0.05) {
  cat("Reject the null hypothesis: Evidence of heteroskedasticity at 5% level.\n")
} else {
  cat("Fail to reject the null hypothesis: No evidence of heteroskedasticity at 5% level.\n")
}

# d.
# Load necessary packages
library(sandwich)
library(lmtest)

# Estimate model with conventional standard errors
conventional_se <- summary(full_model)

# Estimate model with robust standard errors
robust_se <- coeftest(full_model, vcov = vcovHC(full_model, type = "HC1"))

# Function to calculate confidence intervals
calculate_ci <- function(coef, se, df, level = 0.95) {
  alpha <- 1 - level
  t_crit <- qt(1 - alpha/2, df)
  lower <- coef - t_crit * se
  upper <- coef + t_crit * se
  return(c(lower, upper))
}

# Compare confidence intervals
df_residual <- full_model$df.residual
variables <- rownames(robust_se)

# Create data frame for comparison
ci_comparison <- data.frame(
  Variable = variables,
  Conventional_SE = NA,
  Robust_SE = NA,
  Conv_CI_Lower = NA,
  Conv_CI_Upper = NA,
  Robust_CI_Lower = NA,
  Robust_CI_Upper = NA,
  Width_Change = NA
)

for (i in 1:length(variables)) {
  var <- variables[i]
  conv_se <- conventional_se$coefficients[var, "Std. Error"]
  rob_se <- robust_se[var, "Std. Error"]
  
  conv_ci <- calculate_ci(coef(full_model)[var], conv_se, df_residual)
  rob_ci <- calculate_ci(coef(full_model)[var], rob_se, df_residual)
  
  conv_width <- conv_ci[2] - conv_ci[1]
  rob_width <- rob_ci[2] - rob_ci[1]
  width_change <- (rob_width - conv_width) / conv_width * 100
  
  ci_comparison[i, "Conventional_SE"] <- conv_se
  ci_comparison[i, "Robust_SE"] <- rob_se
  ci_comparison[i, "Conv_CI_Lower"] <- conv_ci[1]
  ci_comparison[i, "Conv_CI_Upper"] <- conv_ci[2]
  ci_comparison[i, "Robust_CI_Lower"] <- rob_ci[1]
  ci_comparison[i, "Robust_CI_Upper"] <- rob_ci[2]
  ci_comparison[i, "Width_Change"] <- width_change
}

# Display results
print(ci_comparison)

# Summarize which coefficients have wider/narrower intervals
wider <- ci_comparison$Variable[ci_comparison$Width_Change > 0]
narrower <- ci_comparison$Variable[ci_comparison$Width_Change < 0]

cat("\nCoefficients with wider intervals using robust SE:\n")
print(wider)

cat("\nCoefficients with narrower intervals using robust SE:\n")
print(narrower)

# e.
# Step 1: Estimate the model by OLS
model_ols <- full_model

# Step 2: Calculate squared residuals
ehatsq <- residuals(model_ols)^2

# Step 3: Regress log of squared residuals on METRO and EXPER
sighatsq.ols <- lm(log(ehatsq) ~ METRO + EXPER, data = cps5)

# Step 4: Generate predicted variances
vari <- exp(fitted(sighatsq.ols))

# Step 5: Estimate the model using FGLS with these weights
model_fgls <- lm(log(WAGE) ~ EDUC + EXPER + EXPER2 + FEMALE + BLACK + 
                   METRO + SOUTH + MIDWEST + WEST, data = cps5, weights = 1/vari)

# Summary of FGLS model
summary_fgls <- summary(model_fgls)

# Compare confidence intervals
fgls_coefs <- coef(model_fgls)
fgls_se <- summary_fgls$coefficients[, "Std. Error"]

# Create data frame for comparison with OLS robust
ci_comparison_fgls <- data.frame(
  Variable = variables,
  FGLS_SE = NA,
  Robust_SE = NA,
  FGLS_CI_Lower = NA,
  FGLS_CI_Upper = NA,
  Robust_CI_Lower = ci_comparison$Robust_CI_Lower,
  Robust_CI_Upper = ci_comparison$Robust_CI_Upper,
  Width_Change = NA
)

for (i in 1:length(variables)) {
  var <- variables[i]
  fgls_se_val <- summary_fgls$coefficients[var, "Std. Error"]
  
  fgls_ci <- calculate_ci(fgls_coefs[var], fgls_se_val, model_fgls$df.residual)
  
  fgls_width <- fgls_ci[2] - fgls_ci[1]
  rob_width <- ci_comparison$Robust_CI_Upper[i] - ci_comparison$Robust_CI_Lower[i]
  width_change <- (fgls_width - rob_width) / rob_width * 100
  
  ci_comparison_fgls[i, "FGLS_SE"] <- fgls_se_val
  ci_comparison_fgls[i, "Robust_SE"] <- ci_comparison$Robust_SE[i]
  ci_comparison_fgls[i, "FGLS_CI_Lower"] <- fgls_ci[1]
  ci_comparison_fgls[i, "FGLS_CI_Upper"] <- fgls_ci[2]
  ci_comparison_fgls[i, "Width_Change"] <- width_change
}

# Display results
print(ci_comparison_fgls)

# Summarize which coefficients have wider/narrower intervals
fgls_wider <- ci_comparison_fgls$Variable[ci_comparison_fgls$Width_Change > 0]
fgls_narrower <- ci_comparison_fgls$Variable[ci_comparison_fgls$Width_Change < 0]

cat("\nCoefficients with wider intervals using FGLS vs. OLS robust:\n")
print(fgls_wider)

cat("\nCoefficients with narrower intervals using FGLS vs. OLS robust:\n")
print(fgls_narrower)

# f.
# Calculate robust standard errors for FGLS
robust_se_fgls <- coeftest(model_fgls, vcov = vcovHC(model_fgls, type = "HC1"))

# Compare confidence intervals
ci_comparison_robust_fgls <- data.frame(
  Variable = variables,
  FGLS_SE = ci_comparison_fgls$FGLS_SE,
  FGLS_Robust_SE = NA,
  OLS_Robust_SE = ci_comparison$Robust_SE,
  FGLS_CI_Lower = ci_comparison_fgls$FGLS_CI_Lower,
  FGLS_CI_Upper = ci_comparison_fgls$FGLS_CI_Upper,
  FGLS_Robust_CI_Lower = NA,
  FGLS_Robust_CI_Upper = NA,
  OLS_Robust_CI_Lower = ci_comparison$Robust_CI_Lower,
  OLS_Robust_CI_Upper = ci_comparison$Robust_CI_Upper
)

for (i in 1:length(variables)) {
  var <- variables[i]
  fgls_robust_se <- robust_se_fgls[var, "Std. Error"]
  
  fgls_robust_ci <- calculate_ci(fgls_coefs[var], fgls_robust_se, model_fgls$df.residual)
  
  ci_comparison_robust_fgls[i, "FGLS_Robust_SE"] <- fgls_robust_se
  ci_comparison_robust_fgls[i, "FGLS_Robust_CI_Lower"] <- fgls_robust_ci[1]
  ci_comparison_robust_fgls[i, "FGLS_Robust_CI_Upper"] <- fgls_robust_ci[2]
}

# Display results
print(ci_comparison_robust_fgls)

library(dplyr)

# 1) Compute each interval’s width
ci_compare <- ci_comparison_robust_fgls %>%
  mutate(
    Width_FGLS            = FGLS_CI_Upper            - FGLS_CI_Lower,
    Width_FGLS_Robust     = FGLS_Robust_CI_Upper     - FGLS_Robust_CI_Lower,
    Width_OLS_Robust      = OLS_Robust_CI_Upper      - OLS_Robust_CI_Lower,
    # 2) Differences
    Diff_RobustFGLS_vs_FGLS      = Width_FGLS_Robust - Width_FGLS,
    Diff_RobustFGLS_vs_OLSRobust = Width_FGLS_Robust - Width_OLS_Robust
  )

# 3) Which coefficients got wider/narrower?
wider_rf_vs_f   <- ci_compare$Variable[ci_compare$Diff_RobustFGLS_vs_FGLS >  0]
narrower_rf_vs_f<- ci_compare$Variable[ci_compare$Diff_RobustFGLS_vs_FGLS <  0]

wider_rf_vs_o   <- ci_compare$Variable[ci_compare$Diff_RobustFGLS_vs_OLSRobust >  0]
narrower_rf_vs_o<- ci_compare$Variable[ci_compare$Diff_RobustFGLS_vs_OLSRobust <  0]

# 4) Print summaries
cat("\n--- Robust FGLS vs. FGLS ---\n")
cat("Wider under Robust FGLS:\n");    print(wider_rf_vs_f)
cat("Narrower under Robust FGLS:\n"); print(narrower_rf_vs_f)

cat("\n--- Robust FGLS vs. Robust OLS ---\n")
cat("Wider under Robust FGLS:\n");    print(wider_rf_vs_o)
cat("Narrower under Robust FGLS:\n"); print(narrower_rf_vs_o)

# g.
# Summarize all methods for all coefficients in wide format
coefficients <- names(coef(full_model))  # Get all coefficients from the model

# Create an empty data frame to store results
summary_wide <- data.frame(
  Method = c("OLS Conventional", "OLS Robust", "FGLS Conventional", "FGLS Robust")
)

# Loop through all coefficients
for (coef_name in coefficients) {
  # Extract coefficients and standard errors
  ols_conv_coef <- coef(full_model)[coef_name]
  ols_conv_se <- conventional_se$coefficients[coef_name, "Std. Error"]
  
  ols_rob_coef <- coef(full_model)[coef_name]
  ols_rob_se <- robust_se[coef_name, "Std. Error"]
  
  fgls_conv_coef <- coef(model_fgls)[coef_name]
  fgls_conv_se <- summary_fgls$coefficients[coef_name, "Std. Error"]
  
  fgls_rob_coef <- coef(model_fgls)[coef_name]
  fgls_rob_se <- robust_se_fgls[coef_name, "Std. Error"]
  
  # Add columns to the summary table
  summary_wide[[paste0(coef_name, "_Coef")]] <- c(ols_conv_coef, ols_rob_coef, fgls_conv_coef, fgls_rob_coef)
  summary_wide[[paste0(coef_name, "_SE")]] <- c(ols_conv_se, ols_rob_se, fgls_conv_se, fgls_rob_se)
}

# Print the summary table
print(summary_wide)
