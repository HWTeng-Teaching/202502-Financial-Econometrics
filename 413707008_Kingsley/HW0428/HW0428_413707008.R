library(tidyverse)

# Question 18
# Define the URL
url <- "https://www.principlesofeconometrics.com/poe5/data/rdata/mroz.rdata"
# Open a connection to the URL
con <- url(url, "rb")  # "rb" = read binary mode
# Load the RData file directly from the web
load(con)
# Close the connection
close(con)

mroz = mroz %>% filter(wage > 0)

# a
# Create dummy variables for mother's and father's college education
mroz <- mroz %>%
  mutate(
    MOTHERCOLL = ifelse(mothereduc > 12, 1, 0),
    FATHERCOLL = ifelse(fathereduc > 12, 1, 0)
  )

# Calculate the percentage of parents with some college education
percentage_mother = mean(mroz$MOTHERCOLL) * 100
percentage_father = mean(mroz$FATHERCOLL) * 100

cat("Percentage of mothers with some college education:", percentage_mother, "%\n")
cat("Percentage of fathers with some college education:", percentage_father, "%\n")

# b
# Correlations between EDUC, MOTHERCOLL, and FATHERCOLL
correlations <- cor(mroz %>% select(educ, MOTHERCOLL, FATHERCOLL), use = "complete.obs")
print(correlations)

# Logical argument for MOTHERCOLL and FATHERCOLL as instruments
# MOTHERCOLL and FATHERCOLL are binary variables, reducing potential measurement error compared to continuous variables (MOTHEREDUC, FATHEREDUC).

# c
# Load necessary libraries
library(AER)

# Part (c): IV regression using MOTHERCOLL as the instrument for educ
iv_c <- ivreg(log(wage) ~ educ + exper + I(exper^2) | MOTHERCOLL + exper + I(exper^2), data = mroz)

# Summary with robust standard errors
summary(iv_c, vcov = sandwich)

# 95% confidence interval for educ coefficient
confint(iv_c, 'educ', level = 0.95)

# d
# First-stage regression: Regress EDUC on the instrument and other exogenous variables
first_stage <- lm(educ ~ MOTHERCOLL + exper + I(exper^2), data = mroz)

# Summary of the first-stage regression
summary(first_stage)

# Compute the robust F-test for MOTHERCOLL
# Use linearHypothesis from 'car' package or manual calculation with coeftest
library(car)

# H0: coefficient on MOTHERCOLL = 0
linearHypothesis(first_stage, "MOTHERCOLL = 0", vcov = vcovHC(first_stage, type = "HC1"))
linearHypothesis(first_stage, "MOTHERCOLL = 0")

# e
# IV regression using both MOTHERCOLL and FATHERCOLL as instruments
iv_e <- ivreg(log(wage) ~ educ + exper + I(exper^2) | MOTHERCOLL + FATHERCOLL + exper + I(exper^2), data = mroz)

# Summary with robust standard errors
summary(iv_e, vcov = sandwich)

# 95% confidence interval for the coefficient on educ
confint(iv_e, 'educ', level = 0.95)

# f
# First-stage regression using both instruments
first_stage_e <- lm(educ ~ MOTHERCOLL + FATHERCOLL + exper + I(exper^2), data = mroz)

# Summary of the first-stage regression
summary(first_stage_e)

# Test joint significance of both instruments
# Null hypothesis: coefficients on MOTHERCOLL and FATHERCOLL are both zero
library(car)
linearHypothesis(first_stage_e,
                 c("MOTHERCOLL = 0", "FATHERCOLL = 0"),
                 vcov = vcovHC(first_stage_e, type = "HC1"))
linearHypothesis(first_stage_e,
                 c("MOTHERCOLL = 0", "FATHERCOLL = 0"))

# g
# Load package for diagnostic test
library(sandwich)
library(lmtest)

# Perform Hansen J test (only valid under heteroskedasticity-robust conditions)
# ivreg object must be fit with ivreg from AER package
library(AER)
library(sandwich)

# Perform Hansen's J test for overidentifying restrictions
# Note: ivreg() stores residuals and instruments, so we can use `summary(ivreg_obj, diagnostics=TRUE)`
summary(iv_e, diagnostics = TRUE)

# Question 20
# Define the URL
url <- "https://www.principlesofeconometrics.com/poe5/data/rdata/capm5.rdata"
# Open a connection to the URL
con <- url(url, "rb")  # "rb" = read binary mode
# Load the RData file directly from the web
load(con)
# Close the connection
close(con)

# a
capm5 <- capm5 %>%
  mutate(
    rp_msft = msft - riskfree,      # Microsoft risk premium
    rp_mkt = mkt - riskfree         # Market risk premium
  )

# OLS estimation
model_a <- lm(rp_msft ~ rp_mkt, data = capm5)
summary(model_a)

# b
# Create RANK as the rank of market risk premium
capm5 <- capm5 %>%
  mutate(RANK = rank(rp_mkt))

# First-stage regression: regress rp_mkt on RANK
first_stage_b <- lm(rp_mkt ~ RANK, data = capm5)
summary(first_stage_b)
linearHypothesis(first_stage_b, c("RANK=0"))

# c
capm5$v_hat <- residuals(first_stage_b)  # Save first-stage residuals
augmented_model <- lm(rp_msft ~ rp_mkt + v_hat, data = capm5)
summary(augmented_model)

# d
iv_model <- ivreg(rp_msft ~ rp_mkt | RANK, data = capm5)
summary(iv_model, diagnostics = TRUE)

# e
capm5 <- capm5 %>%
  mutate(POS = ifelse(rp_mkt > 0, 1, 0))
first_stage_e <- lm(rp_mkt ~ RANK + POS, data = capm5)
summary(first_stage_e)
linearHypothesis(first_stage_e, c("RANK = 0", "POS = 0"))

# f
capm5$v_hat_e <- residuals(first_stage_e)
augmented_model_e <- lm(rp_msft ~ rp_mkt + v_hat_e, data = capm5)
summary(augmented_model_e, diagnostics = TRUE)

# g
iv_model_g <- ivreg(rp_msft ~ rp_mkt | RANK + POS, data = capm5)
summary(iv_model_g)

# h
# Get residuals
capm5$iv_resid <- residuals(iv_model_g)
sargan_model <- lm(iv_resid ~ RANK + POS, data = capm5)
summary(sargan_model)
n <- nrow(capm5)
R2_sargan <- summary(sargan_model)$r.squared
sargan_stat <- n * R2_sargan

# p-value with df = number of surplus instruments = 1
p_value <- 1 - pchisq(sargan_stat, df = 1)

cat("Sargan statistic:", sargan_stat, "\n")
cat("p-value:", p_value, "\n")

# Double check the results by using an automatic command.
library(AER)

# Assuming iv_model_g is already created:
summary(iv_model_g, diagnostics = TRUE)

# Question 24
# Define the URL
url <- "https://www.principlesofeconometrics.com/poe5/data/rdata/mroz.rdata"
# Open a connection to the URL
con <- url(url, "rb")  # "rb" = read binary mode
# Load the RData file directly from the web
load(con)
# Close the connection
close(con)

mroz = mroz %>% filter(wage > 0)

# a
iv_model <- ivreg(log(wage) ~ educ + exper + I(exper^2) | mothereduc + fathereduc + exper + I(exper^2), data = mroz)

# Add residuals to the dataset
mroz$e_iv <- residuals(iv_model)

# Plot residuals vs. experience
ggplot(mroz, aes(x = exper, y = e_iv)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "loess", se = FALSE, color = "blue") +
  labs(title = "IV Residuals vs. Experience",
       x = "Experience (EXPER)",
       y = "IV Residuals (ê_IV)") +
  theme_minimal()

# b
mroz$e_iv_sq <- mroz$e_iv^2
bp_model <- lm(e_iv_sq ~ exper, data = mroz)
summary(bp_model)
N <- nrow(mroz)
R2_bp <- summary(bp_model)$r.squared
bp_stat <- N * R2_bp
p_val <- 1 - pchisq(bp_stat, df = 1)

cat("Breusch-Pagan test statistic (NR²):", bp_stat, "\n")
cat("p-value:", p_val, "\n")

# c
# Robust standard errors
summary(iv_model)

coeftest(iv_model, vcov = vcovHC(iv_model, type = "HC1"))

coefci(iv_model, parm = "educ", level = 0.95, 
                    vcov. = vcovHC(iv_model, type = "HC1"))   # 95% CI using Robust SE
coefci(iv_model, parm = "educ", level = 0.95)  # 95% CI using Baseline SE

# d
library(boot)

# Step 1: Define a function to extract the coefficient on EDUC from a bootstrapped sample
boot_iv <- function(data, indices) {
  d <- data[indices, ]
  model <- ivreg(log(wage) ~ educ + exper + I(exper^2) | mothereduc + fathereduc + exper + I(exper^2), data = d)
  return(coef(model)["educ"])
}

# Step 2: Run bootstrap with B = 200
set.seed(123)  # for reproducibility
boot_result <- boot(data = mroz, statistic = boot_iv, R = 200)

# Step 3: Compute bootstrap SE
boot_se <- sd(boot_result$t)

# Use the full-sample coefficient from the IV model
beta_hat <- coef(iv_model)["educ"]

# 95% CI using normal approximation
ci_boot <- beta_hat + c(-1, 1) * 1.96 * boot_se

# Display
cat("Point estimate for EDUC (original model):", beta_hat, "\n")
cat("Bootstrap SE:", boot_se, "\n")
cat("95% CI using bootstrap SE:", ci_boot, "\n")

# Double check the results
library(AER)
library(sandwich)
library(boot)

# Step 1: Estimate the original IV model
iv_model <- ivreg(log(wage) ~ educ + exper + I(exper^2) | 
                    mothereduc + fathereduc + exper + I(exper^2), data = mroz)

# Step 2: Compute Baseline and Robust Standard Errors
se_baseline <- sqrt(diag(vcov(iv_model)))
se_robust <- sqrt(diag(vcovHC(iv_model, type = "HC1")))

# Step 3: Bootstrap SEs for all coefficients
boot_iv_all <- function(data, indices) {
  d <- data[indices, ]
  model <- ivreg(log(wage) ~ educ + exper + I(exper^2) |
                   mothereduc + fathereduc + exper + I(exper^2), data = d)
  return(coef(model))
}

set.seed(123)
boot_all <- boot(data = mroz, statistic = boot_iv_all, R = 200)
se_bootstrap <- apply(boot_all$t, 2, sd)

# Step 4: Combine SEs into a comparison table
se_table <- data.frame(
  Coefficient = names(se_baseline),
  Baseline_SE = se_baseline,
  Robust_SE = se_robust,
  Bootstrap_SE = se_bootstrap
)

# Round only the numeric columns (i.e., standard errors)
se_table[ , 2:4] <- round(se_table[ , 2:4], 4)

# Display the comparison table
print(se_table)
