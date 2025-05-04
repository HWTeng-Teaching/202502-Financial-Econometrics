##################################### 10.18_a################################################

# Load your data (replace 'mroz.csv' with your actual file path if needed)
mroz <- read.csv("C:/Users/PINYKEWD/Documents/mroz.csv")

# Convert all column names to lowercase
colnames(mroz) <- tolower(colnames(mroz))

# Keep only married women who participate in the labor force (if not already filtered)
# For this exercise, we assume the data is already filtered to 428 participants.

# Create mothercoll and fathercoll dummy variables
mroz$mothercoll <- ifelse(mroz$mothereduc > 12, 1, 0)
mroz$fathercoll <- ifelse(mroz$fathereduc > 12, 1, 0)

# Percentages
percent_mothercoll <- mean(mroz$mothercoll) * 100
percent_fathercoll <- mean(mroz$fathercoll) * 100

cat(sprintf("Percentage of mothers with some college education: %.2f%%\n", percent_mothercoll))
cat(sprintf("Percentage of fathers with some college education: %.2f%%\n", percent_fathercoll))

##################################### 10.18_b################################################

# Assuming you've already created 'mothercoll' and 'fathercoll' from part (a)

# Calculate correlations
cor_matrix <- cor(mroz[, c("educ", "mothercoll", "fathercoll")])

# Print correlation matrix with 3 decimal places
print(round(cor_matrix, 3))

# Interpretation
cat("\nKey Observations:")
cat("\n1. EDUC vs MOTHERCOLL correlation:", cor_matrix["educ", "mothercoll"])
cat("\n2. EDUC vs FATHERCOLL correlation:", cor_matrix["educ", "fathercoll"])
cat("\n3. MOTHERCOLL vs FATHERCOLL correlation:", cor_matrix["mothercoll", "fathercoll"])

##################################### 10.18_c################################################

# Load necessary library
library(AER)  # for ivreg()

# Load your data (replace the path if needed)
mroz <- read.csv("C:/Users/PINYKEWD/Documents/mroz.csv")

# Convert all column names to lowercase
colnames(mroz) <- tolower(colnames(mroz))

# Create mothercoll and fathercoll dummy variables
mroz$mothercoll <- ifelse(mroz$mothereduc > 12, 1, 0)
mroz$fathercoll <- ifelse(mroz$fathereduc > 12, 1, 0)

# Calculate percentages
percent_mothercoll <- mean(mroz$mothercoll) * 100
percent_fathercoll <- mean(mroz$fathercoll) * 100

cat(sprintf("Percentage of mothers with some college education: %.2f%%\n", percent_mothercoll))
cat(sprintf("Percentage of fathers with some college education: %.2f%%\n", percent_fathercoll))

# Calculate correlation matrix for educ, mothercoll, fathercoll
cor_matrix <- cor(mroz[, c("educ", "mothercoll", "fathercoll")])
print(round(cor_matrix, 3))

cat("\nKey Observations:")
cat("\n1. EDUC vs MOTHERCOLL correlation:", cor_matrix["educ", "mothercoll"])
cat("\n2. EDUC vs FATHERCOLL correlation:", cor_matrix["educ", "fathercoll"])
cat("\n3. MOTHERCOLL vs FATHERCOLL correlation:", cor_matrix["mothercoll", "fathercoll"], "\n")

# --- Instrumental Variable Regression ---

# Note: Replace 'wage' with your actual dependent variable in the dataset
# Also ensure 'exper' and 'exper2' (or calculate exper2) exist in your data

# Create exper2 variable if not present
if (!"exper2" %in% colnames(mroz)) {
  mroz$exper2 <- mroz$exper^2
}

# Fit IV regression: wage ~ educ + exper + exper2
# Instrument educ with mothercoll
iv_model <- ivreg(wage ~ educ + exper + exper2 | mothercoll + exper + exper2, data = mroz)

# Summary with diagnostics
summary_iv <- summary(iv_model, diagnostics = TRUE)
print(summary_iv)

# Calculate 95% confidence interval for EDUC coefficient
coef_educ <- coef(summary_iv)["educ", "Estimate"]
se_educ <- coef(summary_iv)["educ", "Std. Error"]

lower_bound <- coef_educ - 1.96 * se_educ
upper_bound <- coef_educ + 1.96 * se_educ

cat(sprintf("95%% Confidence Interval for EDUC coefficient: [%.4f, %.4f]\n", lower_bound, upper_bound))

##################################### 10.18_d################################################

# First-stage regression: regress educ on mothercoll and controls
first_stage <- lm(educ ~ mothercoll + exper + exper2 + mothereduc + fathereduc, data = mroz)
summary(first_stage)

# Get the F-statistic for mothercoll (test H0: mothercoll has no effect)
anova_first_stage <- anova(first_stage)
f_stat_mothercoll <- anova_first_stage["mothercoll", "F value"]
cat(sprintf("F-statistic for mothercoll: %.2f\n", f_stat_mothercoll))

##################################### 10.18_e################################################

# Load necessary library
library(AER)  # For ivreg()

# Load the data (adjust path if necessary)
mroz <- read.csv("C:/Users/PINYKEWD/Documents/mroz.csv")

# Convert column names to lowercase for consistency
colnames(mroz) <- tolower(colnames(mroz))

# Create mothercoll and fathercoll dummy variables
mroz$mothercoll <- ifelse(mroz$mothereduc > 12, 1, 0)
mroz$fathercoll <- ifelse(mroz$fathereduc > 12, 1, 0)

# Create exper2 (experience squared) if not already present
if (!"exper2" %in% colnames(mroz)) {
  mroz$exper2 <- mroz$exper^2
}

# Check for zero or negative wages (log undefined for these)
cat("Number of zero or negative wages:", sum(mroz$wage <= 0, na.rm = TRUE), "\n")

# Clean data:
# - Keep only rows with positive wage (for log)
# - Remove rows with missing or infinite values in key variables
mroz_clean <- subset(mroz,
                     wage > 0 & 
                       !is.na(wage) & is.finite(wage) &
                       !is.na(educ) &
                       !is.na(exper) & !is.na(exper2) &
                       !is.na(mothercoll) & !is.na(fathercoll))

# Create log_wage safely after filtering
mroz_clean$log_wage <- log(mroz_clean$wage)

# Print number of observations after cleaning
cat("Number of observations after cleaning:", nrow(mroz_clean), "\n\n")

# Optional: Print percentages of parents with some college education
percent_mothercoll <- mean(mroz_clean$mothercoll) * 100
percent_fathercoll <- mean(mroz_clean$fathercoll) * 100
cat(sprintf("Percentage of mothers with some college education: %.2f%%\n", percent_mothercoll))
cat(sprintf("Percentage of fathers with some college education: %.2f%%\n\n", percent_fathercoll))

# Optional: Print correlation matrix for educ, mothercoll, fathercoll
cor_vars <- c("educ", "mothercoll", "fathercoll")
cor_matrix <- cor(mroz_clean[, cor_vars], use = "complete.obs")
print(round(cor_matrix, 3))

cat("\nKey Correlations:\n")
cat(sprintf("EDUC vs MOTHERCOLL: %.3f\n", cor_matrix["educ", "mothercoll"]))
cat(sprintf("EDUC vs FATHERCOLL: %.3f\n", cor_matrix["educ", "fathercoll"]))
cat(sprintf("MOTHERCOLL vs FATHERCOLL: %.3f\n\n", cor_matrix["mothercoll", "fathercoll"]))

# --- IV regression ---
# Model: log(wage) ~ exper + exper2 + educ
# Instruments for educ: mothercoll and fathercoll
iv_model_2instr <- ivreg(log_wage ~ exper + exper2 + educ | exper + exper2 + mothercoll + fathercoll, data = mroz_clean)

# Summary with diagnostics
summary_iv_2instr <- summary(iv_model_2instr, diagnostics = TRUE)
print(summary_iv_2instr)

# Extract coefficient and standard error for educ
coef_educ <- coef(summary_iv_2instr)["educ", "Estimate"]
se_educ <- coef(summary_iv_2instr)["educ", "Std. Error"]

# Calculate 95% confidence interval for educ coefficient
lower_bound <- coef_educ - 1.96 * se_educ
upper_bound <- coef_educ + 1.96 * se_educ

cat(sprintf("\n95%% Confidence Interval for EDUC coefficient (2 instruments): [%.4f, %.4f]\n",
            lower_bound, upper_bound))


##################################### 10.18_f################################################

# First-stage regression: educ ~ mothercoll + fathercoll + controls
first_stage <- lm(educ ~ mothercoll + fathercoll + exper + exper2, data = mroz_clean)

# Summary of first-stage regression
summary(first_stage)

# Joint F-test for mothercoll and fathercoll coefficients = 0
# Using linearHypothesis() from car package for joint test
if (!requireNamespace("car", quietly = TRUE)) {
  install.packages("car")
}
library(car)

joint_test <- linearHypothesis(first_stage, c("mothercoll = 0", "fathercoll = 0"))
print(joint_test)

# Extract the F-statistic and p-value from the test
f_stat <- joint_test$F[2]
p_value <- joint_test$`Pr(>F)`[2]

cat(sprintf("\nJoint F-test for mothercoll and fathercoll:\nF-statistic = %.3f, p-value = %.4g\n", f_stat, p_value))

# Interpretation
if (f_stat > 10) {
  cat("The instruments appear to be strong (F > 10).\n")
} else {
  cat("The instruments may be weak (F <= 10).\n")
}

##################################### 10.20_a################################################

# Assuming your data is already loaded as 'capm5'
# If not, load it:
# capm5 <- read.csv("path/to/capm5.csv")

# 1. Inspect the first few rows and column names
print(colnames(capm5))
head(capm5)

# 2. Calculate excess returns for Microsoft and the market
capm5$excess_msft <- capm5$msft - capm5$riskfree
capm5$excess_mkt  <- capm5$mkt - capm5$riskfree

# 3. Estimate the CAPM model via OLS
capm_ols <- lm(excess_msft ~ excess_mkt, data = capm5)

# 4. View the regression summary
summary(capm_ols)


##################################### 10.20_b################################################

# 1. Calculate the market excess return
capm5$excess_mkt <- capm5$mkt - capm5$riskfree

# 2. Create the RANK variable (rank the market excess returns from smallest to largest)
capm5$RANK <- rank(capm5$excess_mkt, ties.method = "first")

# 3. First-stage regression: regress excess_mkt on RANK
first_stage <- lm(excess_mkt ~ RANK, data = capm5)
summary_first_stage <- summary(first_stage)
print(summary_first_stage)

# 4. Extract and print key statistics
r2_first_stage <- summary_first_stage$r.squared
coef_rank <- summary_first_stage$coefficients["RANK", "Estimate"]
pval_rank <- summary_first_stage$coefficients["RANK", "Pr(>|t|)"]

cat(sprintf("\nFirst-stage R^2: %.4f\n", r2_first_stage))
cat(sprintf("Coefficient of RANK: %.4f\n", coef_rank))
cat(sprintf("P-value of RANK: %.4g\n", pval_rank))

# 5. Interpretation guidance
cat("\nInterpretation:\n")
cat("If the coefficient of RANK is highly significant (very small p-value) and R^2 is high, RANK is a strong IV for excess_mkt.\n")
cat("If R^2 is low, RANK is a weak IV and may not be suitable for IV estimation.\n")

##################################### 10.20_c################################################

# 1. Calculate excess returns
capm5$excess_msft <- capm5$msft - capm5$riskfree
capm5$excess_mkt  <- capm5$mkt - capm5$riskfree

# 2. Create RANK variable
capm5$RANK <- rank(capm5$excess_mkt, ties.method = "first")

# 3. First-stage regression: excess_mkt ~ RANK
first_stage <- lm(excess_mkt ~ RANK, data = capm5)
capm5$vhat <- resid(first_stage)  # First-stage residuals

# 4. Augmented regression: excess_msft ~ excess_mkt + vhat
augmented_model <- lm(excess_msft ~ excess_mkt + vhat, data = capm5)
summary_augmented <- summary(augmented_model)
print(summary_augmented)

# 5. Test the significance of vhat at the 1% level
pval_vhat <- summary_augmented$coefficients["vhat", "Pr(>|t|)"]
cat(sprintf("\nP-value for vhat: %.4g\n", pval_vhat))

if (pval_vhat < 0.01) {
  cat("vhat is significant at the 1% level: Market return is endogenous (measured with error).\n")
} else {
  cat("vhat is NOT significant at the 1% level: Cannot reject exogeneity of the market return.\n")
}

##################################### 10.20_d################################################

# Load the AER package for ivreg
if (!requireNamespace("AER", quietly = TRUE)) install.packages("AER")
library(AER)

# 1. Calculate excess returns if not already done
capm5$excess_msft <- capm5$msft - capm5$riskfree
capm5$excess_mkt  <- capm5$mkt - capm5$riskfree

# 2. Create RANK variable if not already done
capm5$RANK <- rank(capm5$excess_mkt, ties.method = "first")

# 3. IV/2SLS estimation: use RANK as an instrument for excess_mkt
iv_capm <- ivreg(excess_msft ~ excess_mkt | RANK, data = capm5)
summary(iv_capm)

# 4. Compare with OLS estimate from part (a)
ols_capm <- lm(excess_msft ~ excess_mkt, data = capm5)
summary(ols_capm)

# 5. Print both beta estimates for comparison
beta_ols <- coef(ols_capm)["excess_mkt"]
beta_iv  <- coef(iv_capm)["excess_mkt"]

cat(sprintf("\nOLS beta estimate: %.4f\n", beta_ols))
cat(sprintf("IV (2SLS) beta estimate: %.4f\n", beta_iv))

# 6. Interpretation guidance
cat("\nInterpretation:\n")
cat("Compare the OLS and IV beta estimates.\n")
cat("If the IV estimate is lower in magnitude, it may reflect attenuation bias due to measurement error in the market return.\n")
cat("If both are similar, measurement error may not be a major concern.\n")

##################################### 10.20_e################################################

# 1. Calculate excess returns if not already done
capm5$excess_mkt  <- capm5$mkt - capm5$riskfree

# 2. Create RANK variable if not already done
capm5$RANK <- rank(capm5$excess_mkt, ties.method = "first")

# 3. Create POS variable: 1 if excess_mkt > 0, 0 otherwise
capm5$POS <- as.integer(capm5$excess_mkt > 0)

# 4. First-stage regression: excess_mkt ~ RANK + POS
first_stage2 <- lm(excess_mkt ~ RANK + POS, data = capm5)
summary_first_stage2 <- summary(first_stage2)
print(summary_first_stage2)

# 5. Test the joint significance of RANK and POS using an F-test
if (!requireNamespace("car", quietly = TRUE)) install.packages("car")
library(car)
joint_test <- linearHypothesis(first_stage2, c("RANK = 0", "POS = 0"))
print(joint_test)

# 6. Extract and print R²
r2_first_stage2 <- summary_first_stage2$r.squared
cat(sprintf("\nFirst-stage R^2 with RANK and POS: %.4f\n", r2_first_stage2))

# 7. Interpretation guidance
cat("\nInterpretation:\n")
cat("If the joint F-test p-value is small (e.g., < 0.05), RANK and POS are jointly significant and thus strong instruments.\n")
cat("If R^2 is reasonably high, the instruments explain a good portion of the variation in excess_mkt.\n")


##################################### 10.20_f################################################

# 1. Calculate excess returns if not already done
capm5$excess_msft <- capm5$msft - capm5$riskfree
capm5$excess_mkt  <- capm5$mkt - capm5$riskfree

# 2. Create RANK and POS if not already done
capm5$RANK <- rank(capm5$excess_mkt, ties.method = "first")
capm5$POS  <- as.integer(capm5$excess_mkt > 0)

# 3. First-stage regression: excess_mkt ~ RANK + POS
first_stage <- lm(excess_mkt ~ RANK + POS, data = capm5)
capm5$vhat <- resid(first_stage)  # First-stage residuals

# 4. Augmented regression: excess_msft ~ excess_mkt + vhat
hausman_model <- lm(excess_msft ~ excess_mkt + vhat, data = capm5)
summary_hausman <- summary(hausman_model)
print(summary_hausman)

# 5. Test significance of vhat at 1% level
pval_vhat <- summary_hausman$coefficients["vhat", "Pr(>|t|)"]
cat(sprintf("\nP-value for vhat (Hausman test): %.4g\n", pval_vhat))

if (pval_vhat < 0.01) {
  cat("vhat is significant at the 1% level: Market return is endogenous (measured with error).\n")
} else {
  cat("vhat is NOT significant at the 1% level: Cannot reject exogeneity of the market return.\n")
}

##################################### 10.20_g################################################

# Load required package
if (!requireNamespace("AER", quietly = TRUE)) install.packages("AER")
library(AER)

# 1. Calculate excess returns for Microsoft and the market
capm5$excess_msft <- capm5$msft - capm5$riskfree
capm5$excess_mkt  <- capm5$mkt - capm5$riskfree

# 2. Create RANK and POS variables if not already done
capm5$RANK <- rank(capm5$excess_mkt, ties.method = "first")
capm5$POS  <- as.integer(capm5$excess_mkt > 0)

# 3. OLS estimation (from part a)
ols_capm <- lm(excess_msft ~ excess_mkt, data = capm5)
summary(ols_capm)

# 4. IV/2SLS estimation using RANK and POS as instruments for excess_mkt
iv_capm <- ivreg(excess_msft ~ excess_mkt | RANK + POS, data = capm5)
summary(iv_capm)

# 5. Compare OLS and IV estimates for beta
beta_ols <- coef(ols_capm)["excess_mkt"]
beta_iv  <- coef(iv_capm)["excess_mkt"]

cat(sprintf("\nOLS beta estimate: %.4f\n", beta_ols))
cat(sprintf("IV (2SLS) beta estimate (RANK + POS as IVs): %.4f\n", beta_iv))

cat("\nInterpretation:\n")
cat("Compare the OLS and IV beta estimates.\n")
cat("If the IV estimate is higher than OLS, it may indicate attenuation bias in OLS due to measurement error in the market return.\n")
cat("If both are similar, measurement error may not be a major issue.\n")

##################################### 10.20_h################################################

# Ensure AER is loaded
if (!requireNamespace("AER", quietly = TRUE)) install.packages("AER")
library(AER)

# 1. Calculate excess returns (if not already done)
capm5$excess_msft <- capm5$msft - capm5$riskfree
capm5$excess_mkt  <- capm5$mkt - capm5$riskfree

# 2. Create RANK and POS (if not already done)
capm5$RANK <- rank(capm5$excess_mkt, ties.method = "first")
capm5$POS  <- as.integer(capm5$excess_mkt > 0)

# 3. Estimate the IV/2SLS model using RANK and POS as IVs
iv_capm <- ivreg(excess_msft ~ excess_mkt | RANK + POS, data = capm5)

# 4. Obtain the IV/2SLS residuals
capm5$iv_resid <- resid(iv_capm)

# 5. Regress the IV residuals on all instruments (RANK and POS)
sargan_aux <- lm(iv_resid ~ RANK + POS, data = capm5)
sargan_r2 <- summary(sargan_aux)$r.squared

# 6. Compute the Sargan test statistic (n * R^2)
n <- nrow(capm5)
sargan_stat <- n * sargan_r2

# 7. Degrees of freedom = number of instruments - number of endogenous regressors
df <- 2 - 1  # 2 IVs, 1 endogenous regressor

# 8. Compute p-value
pval_sargan <- 1 - pchisq(sargan_stat, df)

# 9. Print results and interpretation
cat(sprintf("Sargan test statistic: %.4f\n", sargan_stat))
cat(sprintf("Degrees of freedom: %d\n", df))
cat(sprintf("P-value: %.4g\n", pval_sargan))

if (pval_sargan < 0.05) {
  cat("At the 5% significance level, reject the null: At least one instrument may be invalid.\n")
} else {
  cat("At the 5% significance level, do NOT reject the null: Instruments appear valid.\n")
}

##################################### 10.24_a################################################

# Load necessary packages
library(readr)
library(dplyr)
library(AER)
library(ggplot2)

# 1. Import the data
mroz <- read_csv("C:/Users/PINYKEWD/Documents/mroz.csv")  # Use your actual file path

# 2. Subset to married women who participate in the labor force
mroz_lf <- mroz %>% filter(lfp == 1)

# 3. Estimate the IV/2SLS model
# Model: log(wage) ~ educ + exper + expersq
# Instruments for educ: mothereduc and fathereduc
mroz_lf <- mroz_lf %>% mutate(expersq = exper^2,
                              lwage = log(wage))

iv_model <- ivreg(lwage ~ educ + exper + expersq | mothereduc + fathereduc + exper + expersq,
                  data = mroz_lf)

# 4. Calculate IV residuals
mroz_lf$ehat_iv <- residuals(iv_model)

# 5. Plot residuals vs exper
ggplot(mroz_lf, aes(x = exper, y = ehat_iv)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "loess", se = FALSE, color = "blue") +
  labs(title = "IV Residuals vs. Experience",
       x = "Experience (exper)",
       y = "IV Residuals") +
  theme_minimal()


##################################### 10.24_b################################################

# 1. Squared residuals
mroz_lf$ehat_iv_sq <- mroz_lf$ehat_iv^2

# 2. Auxiliary regression
aux_model <- lm(ehat_iv_sq ~ exper, data = mroz_lf)
summary(aux_model)

# 3. NR^2 test statistic
n <- nrow(mroz_lf)
R2 <- summary(aux_model)$r.squared
NR2 <- n * R2
NR2

# 4. p-value
p_value <- 1 - pchisq(NR2, df = 1)
p_value

#################################### 10.24_c###############################################

library(AER)
library(dplyr)
library(sandwich)
library(lmtest)

# Baseline model
iv_model <- ivreg(log(wage) ~ educ + exper + I(exper^2) | mothereduc + fathereduc + exper + I(exper^2),
                  data = mroz %>% filter(lfp == 1))
summary(iv_model) # Classical SEs

# Robust SEs
robust_se <- vcovHC(iv_model, type = "HC1")
coeftest(iv_model, vcov = robust_se) # Robust SEs

# 95% CI for 'educ' using robust SE
coef_educ <- coef(iv_model)["educ"]
se_educ <- sqrt(robust_se["educ", "educ"])
lower_ci <- coef_educ - 1.96 * se_educ
upper_ci <- coef_educ + 1.96 * se_educ
cat("95% CI for educ (robust SE): [", lower_ci, ", ", upper_ci, "]\n")

#################################### 10.24_d##############################################

library(AER)
library(dplyr)
library(boot)
library(sandwich)
library(lmtest)

# Prepare data
mroz_lf <- mroz %>%
  filter(lfp == 1) %>%
  mutate(lwage = log(wage), expersq = exper^2)

# Baseline IV model (for comparison)
iv_model <- ivreg(lwage ~ educ + exper + expersq | mothereduc + fathereduc + exper + expersq, data = mroz_lf)
summary(iv_model)

# Robust SE for comparison
robust_se <- vcovHC(iv_model, type = "HC1")
coeftest(iv_model, vcov = robust_se)

# Bootstrap function for IV estimate of 'educ'
iv_boot <- function(data, indices) {
  d <- data[indices, ]
  model <- ivreg(lwage ~ educ + exper + expersq | mothereduc + fathereduc + exper + expersq, data = d)
  coef(model)["educ"]
}

# Run bootstrap
set.seed(123)
boot_iv <- boot(data = mroz_lf, statistic = iv_boot, R = 200)

# Bootstrap SE
boot_se <- sd(boot_iv$t)
boot_se

# Point estimate for 'educ'
educ_coef <- coef(iv_model)["educ"]

# 95% CI (normal approximation)
lower_ci <- educ_coef - 1.96 * boot_se
upper_ci <- educ_coef + 1.96 * boot_se
cat("Bootstrap 95% CI for educ: [", lower_ci, ", ", upper_ci, "]\n")
