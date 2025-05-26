#################################### 15.17_b##############################################
# Install plm package (only once)
#install.packages("plm")

library(plm)

# Read your data (update the file path)
liquor <- read.csv("C:/Users/PINYKEWD/Documents/liquor5.csv")

# Check the structure to confirm column names
str(liquor)

# Convert to panel data frame using your column names for household and year
panel_data <- pdata.frame(liquor, index = c("hh", "year"))

# Estimate the random effects model: liquor expenditure on income
re_model <- plm(liquor ~ income, data = panel_data, model = "random")

# View summary of the model
summary(re_model)

# Extract coefficient and standard error for income
coef_income <- coef(re_model)["income"]
se_income <- sqrt(vcov(re_model)["income", "income"])

# Calculate 95% confidence interval
z_critical <- qnorm(0.975)  # approx 1.96
lower_ci <- coef_income - z_critical * se_income
upper_ci <- coef_income + z_critical * se_income

# Print results
cat("Random Effects Model Results:\n")
cat("Coefficient on income:", coef_income, "\n")
cat("95% Confidence Interval: [", lower_ci, ", ", upper_ci, "]\n")

#################################### 15.17_c##############################################

# Load necessary packages
library(dplyr)

# Read your data (update the path)
liquor <- read.csv("C:/Users/PINYKEWD/Documents/liquor5.csv")

# Number of households and time periods
N <- length(unique(liquor$hh))
T <- length(unique(liquor$year))

# Step 1: Run pooled OLS regression (no fixed or random effects)
pooled_ols <- lm(liquor ~ income, data = liquor)

# Step 2: Extract residuals and add to data frame
liquor$e_hat <- residuals(pooled_ols)

# Step 3: Calculate numerator and denominator terms for LM statistic

# Sum of residuals by household
sum_ei <- liquor %>%
  group_by(hh) %>%
  summarise(sum_e = sum(e_hat))

# Sum of squared residuals by household
sum_ei_sq <- liquor %>%
  group_by(hh) %>%
  summarise(sum_e_sq = sum(e_hat^2))

# Total sum of squared residuals
total_e_sq <- sum(liquor$e_hat^2)

# Calculate numerator: sum of squared sums of residuals by household
numerator <- sum(sum_ei$sum_e^2)

# Calculate LM statistic according to equation (15.35)
LM <- (sqrt(N * T) / (2 * (T - 1))) * (numerator / total_e_sq - 1)

cat("Lagrange Multiplier (LM) test statistic:", LM, "\n")

# Step 4: Compare LM to critical value from chi-square distribution with 1 df
critical_value <- qchisq(0.95, df = 1)

cat("Critical value at 5% significance level:", critical_value, "\n")

if (LM > critical_value) {
  cat("Reject null hypothesis: Evidence of random effects.\n")
} else {
  cat("Fail to reject null hypothesis: No evidence of random effects.\n")
}

#################################### 15.17_d##############################################

# Load necessary packages
library(plm)
library(dplyr)
library(lmtest)

# Read data (update path accordingly)
liquor <- read.csv("C:/Users/PINYKEWD/Documents/liquor5.csv")

# Convert to panel data frame
panel_data <- pdata.frame(liquor, index = c("hh", "year"))

# Step 1: Calculate individual time averages of INCOME
# This creates a new variable INCOMEM at the individual level
income_means <- panel_data %>%
  group_by(hh) %>%
  summarise(INCOMEM = mean(income))

# Merge INCOMEM back into panel_data
panel_data <- panel_data %>%
  left_join(income_means, by = "hh")

# Step 2: Estimate random effects model including INCOMEM
re_model_with_means <- plm(liquor ~ income + INCOMEM, data = panel_data, model = "random")

# Step 3: Summary of the model
summary(re_model_with_means)

# Step 4: Test significance of gamma (coefficient on INCOMEM)
# Extract coefficient and standard error for INCOMEM
coef_gamma <- coef(re_model_with_means)["INCOMEM"]
se_gamma <- sqrt(vcov(re_model_with_means)["INCOMEM", "INCOMEM"])

# Calculate t-statistic
t_stat <- coef_gamma / se_gamma

# Calculate p-value (two-sided test)
p_value <- 2 * (1 - pnorm(abs(t_stat)))

cat("Coefficient on INCOMEM:", coef_gamma, "\n")
cat("Standard error:", se_gamma, "\n")
cat("t-statistic:", t_stat, "\n")
cat("p-value:", p_value, "\n")

# Step 5: Interpretation
if (p_value < 0.05) {
  cat("Reject null hypothesis: INCOMEM is significant at 5% level.\n")
  cat("This suggests correlation between random effects and INCOME.\n")
  cat("Random effects estimator in (b) may be inconsistent.\n")
} else {
  cat("Fail to reject null hypothesis: INCOMEM is not significant at 5% level.\n")
  cat("No evidence of correlation between random effects and INCOME.\n")
  cat("Random effects estimator in (b) is appropriate.\n")
}

#################################### 15.20_d##############################################

# Load required packages
library(plm)
library(dplyr)

# Step 1: Load the data (update the file path accordingly)
star <- read.csv("C:/Users/PINYKEWD/Documents/star.csv")

# Step 2: Check structure and column names
str(star)

# Step 3: Convert to panel data frame with school as cross-sectional id
panel_star <- pdata.frame(star, index = c("schid", "id"))

# Step 4: Estimate random effects model
re_model <- plm(readscore ~ small + aide + tchexper + boy + white_asian + freelunch,
                data = panel_star, model = "random")

# View summary of random effects model
summary(re_model)

# ---------------------------------------
# Step 5: Lagrange Multiplier (LM) test for random effects

# Run pooled OLS with na.action = na.exclude to keep residuals aligned
pooled_ols <- lm(readscore ~ small + aide + tchexper + boy + white_asian + freelunch,
                 data = star, na.action = na.exclude)

# Extract residuals (length matches original data, NAs where data missing)
star$e_hat <- residuals(pooled_ols)

# Remove rows with NA residuals for LM test calculations
star_lm <- star %>% filter(!is.na(e_hat))

# Number of schools (N) and average observations per school (T)
N <- length(unique(star_lm$schid))
T <- nrow(star_lm) / N  # assumes balanced panel or close to it

# Sum of residuals by school
sum_ei <- star_lm %>%
  group_by(schid) %>%
  summarise(sum_e = sum(e_hat))

# Total sum of squared residuals
total_e_sq <- sum(star_lm$e_hat^2)

# Numerator for LM statistic
numerator <- sum(sum_ei$sum_e^2)

# Calculate LM statistic (equation 15.35)
LM <- (sqrt(N * T) / (2 * (T - 1))) * (numerator / total_e_sq - 1)

cat("Lagrange Multiplier (LM) test statistic:", LM, "\n")

# Critical value for chi-square with 1 df at 5% significance
critical_value <- qchisq(0.95, df = 1)
cat("Critical value at 5% significance level:", critical_value, "\n")

# Conclusion
if (LM > critical_value) {
  cat("Reject null hypothesis: Evidence of random effects.\n")
} else {
  cat("Fail to reject null hypothesis: No evidence of random effects.\n")
}


#################################### 15.20_e##############################################

# Load required packages
library(plm)

# Read data (update path)
star <- read.csv("C:/Users/PINYKEWD/Documents/star.csv")

# Convert to panel data frame
panel_star <- pdata.frame(star, index = c("schid", "id"))

# Estimate fixed effects model
fe_model <- plm(readscore ~ small + aide + tchexper + boy + white_asian + freelunch,
                data = panel_star, model = "within")

# Estimate random effects model
re_model <- plm(readscore ~ small + aide + tchexper + boy + white_asian + freelunch,
                data = panel_star, model = "random")

# Extract coefficients
coef_fe <- coef(fe_model)
coef_re <- coef(re_model)

# Extract covariance matrices
vcov_fe <- vcov(fe_model)
vcov_re <- vcov(re_model)

# Variables to test
vars_to_test <- c("small", "aide", "tchexper", "white_asian", "freelunch", "boy")

# Function to compute t-test statistic for difference in coefficients
hausman_t_test <- function(var_name) {
  diff <- coef_fe[var_name] - coef_re[var_name]
  var_diff <- vcov_fe[var_name, var_name] - vcov_re[var_name, var_name]
  t_stat <- diff / sqrt(abs(var_diff))  # Use abs() to avoid negative variance due to numerical issues
  p_value <- 2 * (1 - pnorm(abs(t_stat)))
  return(data.frame(variable = var_name, t_statistic = t_stat, p_value = p_value))
}

# Apply test for each variable
test_results <- do.call(rbind, lapply(vars_to_test, hausman_t_test))

print(test_results)

# Interpretation helper
test_results$significant <- ifelse(test_results$p_value < 0.05, "Yes", "No")

print(test_results)

#################################### 15.20_f##############################################

# Load required packages
library(plm)
library(car)      # For VIF
library(dplyr)

# Step 1: Load your data (update the path)
star <- read.csv("C:/Users/PINYKEWD/Documents/star.csv")

# Step 2: Check structure and variable names
str(star)

# Step 3: Check for missing values in key variables
summary(star[, c("readscore", "small", "aide", "tchexper", "boy", "white_asian", "freelunch")])

# Step 4: Remove rows with missing data in regressors or dependent variable
star_clean <- star %>%
  filter(!is.na(readscore) & !is.na(small) & !is.na(aide) & !is.na(tchexper) &
           !is.na(boy) & !is.na(white_asian) & !is.na(freelunch))

# Step 5: Check multicollinearity with VIF on pooled OLS (simpler model)
pooled_ols <- lm(readscore ~ small + aide + tchexper + boy + white_asian + freelunch,
                 data = star_clean)
vif_values <- vif(pooled_ols)
print(vif_values)

# Step 6: If any VIF > 10, consider removing or combining variables
# For example, if 'white_asian' and 'boy' are highly correlated, try removing one
# Here, let's assume all VIFs are acceptable; if not, adjust accordingly

# Step 7: Convert cleaned data to panel data frame
panel_star <- pdata.frame(star_clean, index = c("schid", "id"))

# Step 8: Estimate random effects model with alternative method to avoid singularity
re_model <- plm(readscore ~ small + aide + tchexper + boy + white_asian + freelunch,
                data = panel_star, model = "random", random.method = "walhus")

# Step 9: View summary of the model
summary(re_model)

