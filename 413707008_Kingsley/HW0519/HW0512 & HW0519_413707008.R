# Load required packages
library(tidyverse)
library(AER)      # For 2SLS estimation
library(plm)      # For panel data models
library(dplyr)    # For data manipulation
library(car)      # For hypothesis testing
library(lmtest)   # For additional tests
library(sandwich) # For robust standard errors

# Question 15.20 - STAR Experiment Analysis
# Load the STAR data
url <- "https://www.principlesofeconometrics.com/poe5/data/rdata/star.rdata"
con <- url(url, "rb")
load(con)
close(con)

# Check data structure
cat("Data dimensions:", dim(star), "\n")
cat("Variable names:", names(star), "\n")
cat("Number of schools:", length(unique(star$schid)), "\n")
cat("Number of students:", length(unique(star$id)), "\n")

# a. Estimate regression with no fixed or random effects (Pooled OLS)
cat("\n=== PART A: POOLED OLS ===\n")
model_pooled <- lm(readscore ~ small + aide + tchexper + boy + white_asian + freelunch, data = star)
summary(model_pooled)

# b. Estimate with school fixed effects
cat("\n=== PART B: FIXED EFFECTS ===\n")
# Note: For cross-sectional data with school clustering, use only school ID as index
model_fe <- plm(readscore ~ small + aide + tchexper + boy + white_asian + freelunch, 
                data = star, index = "schid", model = "within")
summary(model_fe)

# c. Test for significance of school fixed effects
cat("\n=== PART C: TEST FOR FIXED EFFECTS ===\n")
# Create pooled model using plm for proper comparison
model_pooled_plm <- plm(readscore ~ small + aide + tchexper + boy + white_asian + freelunch, 
                        data = star, index = "schid", model = "pooling")

# F-test for fixed effects
fe_test <- pFtest(model_fe, model_pooled_plm)
print(fe_test)

# d. Estimate with random effects
cat("\n=== PART D: RANDOM EFFECTS ===\n")
model_re <- plm(readscore ~ small + aide + tchexper + boy + white_asian + freelunch, 
                data = star, index = "schid", model = "random")
summary(model_re)

# Lagrange multiplier test for random effects
cat("\nLagrange Multiplier Test for Random Effects:\n")
re_test <- plmtest(model_pooled_plm, effect = "individual")
print(re_test)

# e. Hausman test to choose between fixed and random effects
cat("\n=== PART E: HAUSMAN TEST ===\n")

# Method 1: Built-in Hausman test
cat("Built-in Hausman Test:\n")
hausman_overall <- phtest(model_fe, model_re)
print(hausman_overall)

# Method 2: Manual calculation for individual coefficients
cat("\nIndividual Coefficient Hausman Tests:\n")
fe_coefs <- coef(model_fe)
re_coefs <- coef(model_re)
fe_vcov <- vcov(model_fe)
re_vcov <- vcov(model_re)

# Variables to test (excluding intercept for FE)
vars_to_test <- c("small", "aide", "tchexper", "boy", "white_asian", "freelunch")

# Function to calculate Hausman t-statistic for individual coefficients
hausman_t_test <- function(var_name) {
  b_fe <- fe_coefs[var_name]
  b_re <- re_coefs[var_name]
  var_fe <- fe_vcov[var_name, var_name]
  var_re <- re_vcov[var_name, var_name]
  
  # Calculate t-statistic using formula (15.36)
  # Note: var_diff = var_fe - var_re (should be positive)
  var_diff <- var_fe - var_re
  
  if(var_diff > 0) {
    t_stat <- (b_fe - b_re) / sqrt(var_diff)
    p_value <- 2 * (1 - pnorm(abs(t_stat)))
  } else {
    t_stat <- NA
    p_value <- NA
  }
  
  return(list(
    variable = var_name,
    fe_coef = b_fe,
    re_coef = b_re,
    difference = b_fe - b_re,
    t_statistic = t_stat,
    p_value = p_value,
    significant_5pct = ifelse(is.na(t_stat), NA, abs(t_stat) > 1.96)
  ))
}

# Run tests for all variables
hausman_results <- lapply(vars_to_test, hausman_t_test)

# Display results in a nice format
results_df <- data.frame(
  Variable = sapply(hausman_results, function(x) x$variable),
  FE_Coef = round(sapply(hausman_results, function(x) x$fe_coef), 4),
  RE_Coef = round(sapply(hausman_results, function(x) x$re_coef), 4),
  Difference = round(sapply(hausman_results, function(x) x$difference), 4),
  T_Statistic = round(sapply(hausman_results, function(x) 
    ifelse(is.na(x$t_statistic), NA, x$t_statistic)), 4),
  P_Value = round(sapply(hausman_results, function(x) 
    ifelse(is.na(x$p_value), NA, x$p_value)), 4),
  Significant_5pct = sapply(hausman_results, function(x) x$significant_5pct)
)

print(results_df)

# f. Mundlak test
cat("\n=== PART F: MUNDLAK TEST ===\n")

# Create school averages for each variable
star_mundlak <- star %>%
  group_by(schid) %>%
  mutate(
    small_bar = mean(small, na.rm = TRUE),
    aide_bar = mean(aide, na.rm = TRUE),
    tchexper_bar = mean(tchexper, na.rm = TRUE),
    boy_bar = mean(boy, na.rm = TRUE),
    white_asian_bar = mean(white_asian, na.rm = TRUE),
    freelunch_bar = mean(freelunch, na.rm = TRUE)
  ) %>%
  ungroup()

# Create Mundlak OLS model (original variables + school averages)
mundlak_ols <- lm(readscore ~ small + aide + tchexper + boy + white_asian + freelunch +
                    small_bar + aide_bar + tchexper_bar + boy_bar + white_asian_bar + freelunch_bar,
                  data = star_mundlak)

# Test for joint significance of school averages
mundlak_test <- linearHypothesis(mundlak_ols, 
                                 c("small_bar = 0", "aide_bar = 0", "tchexper_bar = 0", 
                                   "boy_bar = 0", "white_asian_bar = 0", "freelunch_bar = 0"))

cat("Mundlak Test Results:\n")
cat("F-statistic:", round(mundlak_test$F[2], 2), "\n")
cat("P-value:", round(mundlak_test$`Pr(>F)`[2], 6), "\n")

# Interpretation
if(mundlak_test$`Pr(>F)`[2] < 0.05) {
  cat("\nCONCLUSION: Reject H0 - School averages are jointly significant.\n")
  cat("This suggests correlation between unobserved school effects and regressors.\n")
  cat("Fixed Effects estimation is preferred over Random Effects.\n")
} else {
  cat("\nCONCLUSION: Fail to reject H0 - School averages not jointly significant.\n")
  cat("Random Effects may be consistent.\n")
}

# Show coefficients of school averages
cat("\nCoefficients of school averages:\n")
mundlak_coefs <- coef(mundlak_ols)[c("small_bar", "aide_bar", "tchexper_bar", 
                                     "boy_bar", "white_asian_bar", "freelunch_bar")]
print(round(mundlak_coefs, 4))

# Summary comparison table
cat("\n=== SUMMARY COMPARISON ===\n")
comparison_vars <- c("small", "aide", "tchexper", "boy", "white_asian", "freelunch")

comparison_df <- data.frame(
  Variable = comparison_vars,
  Pooled_OLS = round(coef(model_pooled)[comparison_vars], 4),
  Fixed_Effects = round(coef(model_fe)[comparison_vars], 4),
  Random_Effects = round(coef(model_re)[comparison_vars], 4)
)

print(comparison_df)

cat("\n=== TEST SUMMARY ===\n")
cat("1. F-test for Fixed Effects: p-value =", round(fe_test$p.value, 6), "\n")
cat("2. LM test for Random Effects: p-value =", round(re_test$p.value, 6), "\n")
cat("3. Hausman test (FE vs RE): p-value =", round(hausman_overall$p.value, 6), "\n")
cat("4. Mundlak test: p-value =", round(mundlak_test$`Pr(>F)`[2], 6), "\n")

# Final recommendation
cat("\n=== FINAL RECOMMENDATION ===\n")
if(fe_test$p.value < 0.05) {
  cat("Fixed effects are significant (reject pooled OLS).\n")
}
if(re_test$p.value < 0.05) {
  cat("Random effects are significant (reject pooled OLS).\n")
}
if(hausman_overall$p.value < 0.05) {
  cat("Hausman test rejects RE in favor of FE.\n")
  cat("RECOMMENDATION: Use Fixed Effects model.\n")
} else {
  cat("Hausman test does not reject RE.\n")
  if(mundlak_test$`Pr(>F)`[2] < 0.05) {
    cat("But Mundlak test suggests FE is preferred.\n")
    cat("RECOMMENDATION: Use Fixed Effects model.\n")
  } else {
    cat("Mundlak test also supports RE.\n")
    cat("RECOMMENDATION: Random Effects model is appropriate.\n")
  }
}

# Question 15.17
# Load required packages
library(tidyverse)
library(plm)        # For panel data models
library(dplyr)      # For data manipulation
library(lmtest)     # For additional tests
library(car)        # For hypothesis testing

# Load the LIQUOR data
url <- "https://www.principlesofeconometrics.com/poe5/data/rdata/liquor5.rdata"
con <- url(url, "rb")
load(con)
close(con)

# Check data structure
cat("Data dimensions:", dim(liquor5), "\n")
cat("Variable names:", names(liquor5), "\n")
cat("Number of households:", length(unique(liquor5$hh)), "\n")
cat("Number of years:", length(unique(liquor5$year)), "\n")
cat("Years included:", sort(unique(liquor5$year)), "\n")

# Display first few observations
cat("\nFirst 10 observations:\n")
print(head(liquor5, 10))

# a. Create first-differenced observations and estimate without constant
cat("\n=== PART A: FIRST DIFFERENCES ===\n")

# Method A: More explicit dplyr
liquor5 <- liquor5 %>%
  ungroup() %>%                    # Remove any existing grouping
  arrange(hh, year) %>%           
  group_by(hh) %>%
  mutate(
    liquord = liquor - dplyr::lag(liquor, n = 1),    # Be explicit about dplyr::lag
    incomed = income - dplyr::lag(income, n = 1)
  ) %>%
  ungroup()

# Remove observations with missing first differences (first year for each household)
liquor_diff <- liquor5[!is.na(liquor5$liquord), ]

cat("Number of observations after first differencing:", nrow(liquor_diff), "\n")

# OLS regression of LIQUORD on INCOMED without constant
model_diff <- lm(liquord ~ incomed - 1, data = liquor_diff)
summary(model_diff)

# 95% confidence interval for the coefficient
conf_int_diff <- confint(model_diff, level = 0.95)
cat("\n95% Confidence Interval for INCOMED coefficient (First Differences):\n")
print(conf_int_diff)

# b. Random Effects estimation
cat("\n=== PART B: RANDOM EFFECTS ===\n")

# Convert to panel data format
liquor_panel <- pdata.frame(liquor5, index = c("hh", "year"))

# Random Effects estimation
model_re <- plm(liquor ~ income, data = liquor_panel, model = "random")
summary(model_re)

# 95% confidence interval for INCOME coefficient in RE model
conf_int_re <- confint(model_re, level = 0.95)
cat("\n95% Confidence Interval for INCOME coefficient (Random Effects):\n")
print(conf_int_re)

# Compare the two intervals
cat("\n=== COMPARISON OF CONFIDENCE INTERVALS ===\n")
cat("First Differences - INCOMED coefficient:\n")
cat("Estimate:", round(coef(model_diff)[1], 4), "\n")
cat("95% CI: [", round(conf_int_diff[1], 4), ",", round(conf_int_diff[2], 4), "]\n\n")

cat("Random Effects - INCOME coefficient:\n")
cat("Estimate:", round(coef(model_re)[2], 4), "\n")
cat("95% CI: [", round(conf_int_re[2,1], 4), ",", round(conf_int_re[2,2], 4), "]\n\n")

# c. LM test for random effects
cat("\n=== PART C: LM TEST FOR RANDOM EFFECTS ===\n")

# First estimate pooled OLS model for comparison
model_pooled <- plm(liquor ~ income, data = liquor_panel, model = "pooling")

# LM test for random effects (Breusch-Pagan test)
#lm_test <- plmtest(model_pooled, effect = "individual", type = "bp")
# Honda (default) - matches equation (15.35)
lm_test <- plmtest(model_pooled, effect = "individual")
cat("Lagrange Multiplier Test for Random Effects:\n")
print(lm_test)

# Manual calculation of LM statistic using equation (15.35)
cat("\nManual calculation of LM statistic:\n")

# Get residuals from pooled OLS
residuals_pooled <- residuals(model_pooled)

# Create data frame with residuals and household identifier
res_data <- data.frame(
  hh = liquor_panel$hh,
  year = liquor_panel$year,
  residual = residuals_pooled
)

# Calculate sum of squared residuals by household
res_by_hh <- res_data %>%
  group_by(hh) %>%
  summarise(
    T_i = n(),
    sum_res = sum(residual),
    .groups = 'drop'
  )

# Calculate components for LM statistic
N <- length(unique(res_data$hh))  # Number of households
T_bar <- nrow(res_data) / N       # Average time periods per household

# Corrected LM statistic calculation based on equation (15.35)
numerator_sum <- sum(res_by_hh$sum_res^2)  # ?????(?????????????)??
denominator_sum <- sum(residuals_pooled^2)  # ????????????????????

# The fraction inside the braces
fraction_term <- (numerator_sum / denominator_sum) - 1

# LM statistic with correct structure
LM_manual <- sqrt(N * T_bar / (2 * (T_bar - 1))) * fraction_term

cat("Manual LM statistic:", round(LM_manual, 4), "\n")
cat("Built-in LM statistic:", round(lm_test$statistic, 4), "\n")
cat("P-value:", round(lm_test$p.value, 6), "\n")

# Test conclusion
if(lm_test$p.value < 0.05) {
  cat("\nCONCLUSION: Reject H0 at 5% level.\n")
  cat("Random effects are present - use RE instead of pooled OLS.\n")
} else {
  cat("\nCONCLUSION: Fail to reject H0 at 5% level.\n")
  cat("No evidence of random effects - pooled OLS is adequate.\n")
}

# d. Mundlak-type test with time averages
cat("\n=== PART D: MUNDLAK TEST ===\n")

# Create time averages for INCOME by household
liquor_mundlak <- liquor5 %>%
  group_by(hh) %>%
  mutate(incomem = mean(income, na.rm = TRUE)) %>%
  ungroup()

# Convert to panel data format
liquor_mundlak_panel <- pdata.frame(liquor_mundlak, index = c("hh", "year"))

# Estimate RE model with time averages
model_mundlak <- plm(liquor ~ income + incomem, data = liquor_mundlak_panel, model = "random")
summary(model_mundlak)

# Test significance of ?? (coefficient on INCOMEM)
cat("\nTesting significance of ?? (coefficient on INCOMEM):\n")

# Extract coefficient and standard error for INCOMEM
gamma_coef <- coef(model_mundlak)["incomem"]
gamma_se <- sqrt(diag(vcov(model_mundlak)))["incomem"]
gamma_t <- gamma_coef / gamma_se
gamma_p <- 2 * (1 - pt(abs(gamma_t), df = df.residual(model_mundlak)))

cat("?? coefficient:", round(gamma_coef, 4), "\n")
cat("Standard error:", round(gamma_se, 4), "\n")
cat("t-statistic:", round(gamma_t, 4), "\n")
cat("p-value:", round(gamma_p, 6), "\n")

# Alternative: Use linearHypothesis for formal test
cat("\nFormal hypothesis test for ?? = 0:\n")
mundlak_test <- linearHypothesis(model_mundlak, "incomem = 0")
print(mundlak_test)

# Interpretation
cat("\n=== INTERPRETATION ===\n")
if(gamma_p < 0.05) {
  cat("CONCLUSION: Reject H0: ?? = 0 at 5% level.\n")
  cat("The coefficient on INCOMEM is statistically significant.\n")
  cat("This indicates correlation between the random effect ui and INCOME.\n")
  cat("The random effects estimator is NOT consistent for the model in part (b).\n")
  cat("Consider using fixed effects estimation instead.\n")
} else {
  cat("CONCLUSION: Fail to reject H0: ?? = 0 at 5% level.\n")
  cat("The coefficient on INCOMEM is not statistically significant.\n")
  cat("No evidence of correlation between the random effect ui and INCOME.\n")
  cat("It is OK to use the random effects estimator for the model in part (b).\n")
}

# Summary table comparing all models
cat("\n=== SUMMARY COMPARISON ===\n")

# Create comparison table
comparison_table <- data.frame(
  Model = c("First Differences", "Random Effects", "RE with Time Averages"),
  Income_Coefficient = c(
    round(coef(model_diff)[1], 4),
    round(coef(model_re)[2], 4),
    round(coef(model_mundlak)[2], 4)
  ),
  Standard_Error = c(
    round(sqrt(diag(vcov(model_diff)))[1], 4),
    round(sqrt(diag(vcov(model_re)))[2], 4),
    round(sqrt(diag(vcov(model_mundlak)))[2], 4)
  ),
  CI_Lower = c(
    round(conf_int_diff[1], 4),
    round(conf_int_re[2,1], 4),
    round(confint(model_mundlak)[2,1], 4)
  ),
  CI_Upper = c(
    round(conf_int_diff[2], 4),
    round(conf_int_re[2,2], 4),
    round(confint(model_mundlak)[2,2], 4)
  )
)

print(comparison_table)

# Final summary of all tests
cat("\n=== FINAL SUMMARY ===\n")
cat("1. First Differences vs Random Effects:\n")
cat("   - FD coefficient:", round(coef(model_diff)[1], 4), "\n")
cat("   - RE coefficient:", round(coef(model_re)[2], 4), "\n")
cat("   - Difference suggests presence of individual effects\n\n")

cat("2. LM Test for Random Effects:\n")
cat("   - LM statistic:", round(lm_test$statistic, 4), "\n")
cat("   - P-value:", round(lm_test$p.value, 6), "\n")
cat("   - Conclusion:", ifelse(lm_test$p.value < 0.05, "Random effects present", "No random effects"), "\n\n")

cat("3. Mundlak Test (?? = 0):\n")
cat("   - ?? coefficient:", round(gamma_coef, 4), "\n")
cat("   - P-value:", round(gamma_p, 6), "\n")
cat("   - Conclusion:", ifelse(gamma_p < 0.05, "RE not consistent", "RE is OK"), "\n")
