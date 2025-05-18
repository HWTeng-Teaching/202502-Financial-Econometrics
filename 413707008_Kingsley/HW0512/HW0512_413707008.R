# Load required packages
library(tidyverse)
library(AER)  # For 2SLS estimation

# Question 28
# Load the truffles data
url <- "https://www.principlesofeconometrics.com/poe5/data/rdata/truffles.rdata"
con <- url(url, "rb")
load(con)
close(con)

# Part (b): Estimate the supply and demand equations using 2SLS
# First, identify the endogenous and exogenous variables
# Endogenous: p, q
# Exogenous: ps, di, pf

# Demand equation: p ~ q + ps + di
# Supply equation: p ~ q + pf

# First-stage regression: regress endogenous variable (q) on all exogenous variables
first_stage <- lm(q ~ ps + di + pf, data = truffles)
summary(first_stage)

# 2SLS for demand equation
demand_2sls <- ivreg(p ~ q + ps + di | ps + di + pf, data = truffles)
summary(demand_2sls, diagnostics = TRUE)

# 2SLS for supply equation
supply_2sls <- ivreg(p ~ q + pf | ps + di + pf, data = truffles)
summary(supply_2sls)

# Part (c): Estimate price elasticity of demand at the means
# First, calculate means of p and q
p_mean <- mean(truffles$p)
q_mean <- mean(truffles$q)

# Get the coefficient of q in the demand equation
q_coef <- coef(demand_2sls)["q"]

# Calculate price elasticity of demand at the means
# Elasticity = (bP/bQ) * (Q/P) = q_coef * (q_mean/p_mean)
elasticity <- q_coef * (q_mean/p_mean)
cat("Price elasticity of demand at the means:", elasticity, "\n")

# Part (d): Sketch supply and demand equations
# Set exogenous variables to specified values
DI_star <- 3.5
PF_star <- 23
PS_star <- 22

# Create a sequence of Q values
q_seq <- seq(min(truffles$q), max(truffles$q), length.out = 100)

# Calculate corresponding P values for demand curve
demand_intercept <- coef(demand_2sls)[1]
demand_q_coef <- coef(demand_2sls)[2]
demand_ps_coef <- coef(demand_2sls)[3]
demand_di_coef <- coef(demand_2sls)[4]

p_demand <- demand_intercept + demand_q_coef * q_seq + demand_ps_coef * PS_star + demand_di_coef * DI_star

# Calculate corresponding P values for supply curve
supply_intercept <- coef(supply_2sls)[1]
supply_q_coef <- coef(supply_2sls)[2]
supply_pf_coef <- coef(supply_2sls)[3]

p_supply <- supply_intercept + supply_q_coef * q_seq + supply_pf_coef * PF_star

# Create a data frame for plotting
plot_data <- data.frame(
  q = rep(q_seq, 2),
  p = c(p_demand, p_supply),
  curve = rep(c("Demand", "Supply"), each = length(q_seq))
)

# Create the plot
ggplot(plot_data, aes(x = q, y = p, color = curve)) +
  geom_line() +
  labs(title = "Supply and Demand Curves",
       x = "Quantity (Q)",
       y = "Price (P)") +
  theme_minimal()

# Part (e): Calculate equilibrium values
# Find intersection point of supply and demand curves
# At equilibrium: p_demand = p_supply
# demand_intercept + demand_q_coef * q_eq + demand_ps_coef * PS_star + demand_di_coef * DI_star = 
# supply_intercept + supply_q_coef * q_eq + supply_pf_coef * PF_star

# Solve for q_eq
q_eq <- (supply_intercept - demand_intercept + supply_pf_coef * PF_star - 
           demand_ps_coef * PS_star - demand_di_coef * DI_star) / 
  (demand_q_coef - supply_q_coef)

# Calculate p_eq using either demand or supply equation
p_eq <- demand_intercept + demand_q_coef * q_eq + demand_ps_coef * PS_star + demand_di_coef * DI_star

cat("Equilibrium quantity:", q_eq, "\n")
cat("Equilibrium price:", p_eq, "\n")

# Compare with reduced-form predictions from Table 11.2
# Note: We need to estimate the reduced form equations since Table 11.2 is not provided
# Reduced form for p
p_reduced <- lm(p ~ ps + di + pf, data = truffles)
# Reduced form for q
q_reduced <- lm(q ~ ps + di + pf, data = truffles)

# Predict p and q using the reduced form equations
p_pred <- predict(p_reduced, newdata = data.frame(ps = PS_star, di = DI_star, pf = PF_star))
q_pred <- predict(q_reduced, newdata = data.frame(ps = PS_star, di = DI_star, pf = PF_star))

cat("Predicted equilibrium price from reduced form:", p_pred, "\n")
cat("Predicted equilibrium quantity from reduced form:", q_pred, "\n")
cat("Difference in price predictions:", p_eq - p_pred, "\n")
cat("Difference in quantity predictions:", q_eq - q_pred, "\n")

# Part (f): Estimate supply and demand equations using OLS
# Demand equation (OLS)
demand_ols <- lm(p ~ q + ps + di, data = truffles)
summary(demand_ols)

# Supply equation (OLS)
supply_ols <- lm(p ~ q + pf, data = truffles)
summary(supply_ols)

# Compare OLS and 2SLS results with significance levels
# Function to extract coefficients and p-values
get_coef_and_pval <- function(model) {
  coefs <- coef(model)
  pvals <- summary(model)$coefficients[, 4]  # Extract p-values
  result <- matrix(nrow = length(coefs), ncol = 2)
  colnames(result) <- c("Estimate", "p-value")
  rownames(result) <- names(coefs)
  result[, 1] <- coefs
  result[, 2] <- pvals
  return(result)
}

# Demand equation comparison
cat("\nComparison of Demand Equation Coefficients:\n")
demand_ols_results <- get_coef_and_pval(demand_ols)
demand_2sls_results <- get_coef_and_pval(demand_2sls)

# Create a combined table with both estimates and p-values
demand_comparison <- cbind(
  "OLS Est" = demand_ols_results[, 1],
  "OLS p-val" = demand_ols_results[, 2],
  "2SLS Est" = demand_2sls_results[, 1],
  "2SLS p-val" = demand_2sls_results[, 2]
)
print(demand_comparison)

# Supply equation comparison
cat("\nComparison of Supply Equation Coefficients:\n")
supply_ols_results <- get_coef_and_pval(supply_ols)
supply_2sls_results <- get_coef_and_pval(supply_2sls)

# Create a combined table with both estimates and p-values
supply_comparison <- cbind(
  "OLS Est" = supply_ols_results[, 1],
  "OLS p-val" = supply_ols_results[, 2],
  "2SLS Est" = supply_2sls_results[, 1],
  "2SLS p-val" = supply_2sls_results[, 2]
)
print(supply_comparison)

# Add significance stars for easier interpretation
cat("\nSignificance codes: 0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1\n")

# Question 30
# Load required packages
library(AER)  # For 2SLS
library(car)  # For linearHypothesis

# Load the data
url <- "https://www.principlesofeconometrics.com/poe5/data/rdata/klein.rdata"
con <- url(url, "rb")
load(con)
close(con)

# Examine the data structure
str(klein)

# Part (a): OLS estimation of investment function
investment_ols <- lm(i ~ p + plag + klag, data = klein)
summary(investment_ols)

# Load the broom package for augment function
library(broom)

# Part (b): Reduced-form equation for profits (P)
klein = na.omit(klein)
profit_rf <- lm(p ~ plag + klag + g + tx + w2 + time + elag, data = klein)
summary(profit_rf)

# Test joint significance of all variables except plag and klag
library(car)
linearHypothesis(profit_rf, c("g = 0", "tx = 0", "w2 = 0", "time = 0", "elag = 0"))

# Use augment to create a dataframe with fitted values and residuals
# This ensures perfect matching with original observations
augmented_data <- augment(profit_rf, data = klein)

# Part (c): Hausman test for endogeneity using augmented data
hausman_test <- lm(i ~ p + plag + klag + .resid, data = augmented_data)
summary(hausman_test)

# Part (d): 2SLS estimation using all instruments
library(AER)
investment_2sls <- ivreg(i ~ p + plag + klag | plag + klag + g + tx + w2 + time + elag, 
                         data = klein)
summary(investment_2sls)

# Compare OLS and 2SLS estimates
investment_ols <- lm(i ~ p + plag + klag, data = klein)
cbind(OLS = coef(investment_ols), TwoSLS = coef(investment_2sls))

# Part (e): Manual 2SLS using fitted values from augmented data
manual_2sls <- lm(i ~ .fitted + plag + klag, data = augmented_data)
summary(manual_2sls)

# Extract full coefficient information
manual_coef <- summary(manual_2sls)$coefficients
auto_coef <- summary(investment_2sls)$coefficients

# Create a more comprehensive comparison
comparison <- cbind(
  Manual_2SLS_coef = manual_coef[, 1],
  Manual_2SLS_se = manual_coef[, 2],
  Manual_2SLS_t = manual_coef[, 3],
  Manual_2SLS_p = manual_coef[, 4],
  Auto_2SLS_coef = auto_coef[, 1],
  Auto_2SLS_se = auto_coef[, 2],
  Auto_2SLS_t = auto_coef[, 3],
  Auto_2SLS_p = auto_coef[, 4]
)

# Print the comparison
print(comparison)

# Part (f): Sargan test for instrument validity

# Step 1: Extract residuals from the manual 2SLS model from part (e)
e2_hat <- residuals(manual_2sls)

# Step 2: Regress these residuals on all exogenous and predetermined variables
# The instruments are: plag, klag, g, tx, w2, time, elag
sargan_reg <- lm(e2_hat ~ plag + klag + g + tx + w2 + time + elag, data = klein)

# Step 3: Get RB2 from this regression
sargan_r2 <- summary(sargan_reg)$r.squared

# Sample size
T <- length(e2_hat)

# Compute the Sargan test statistic: TRB2
sargan_stat <- T * sargan_r2

# Degrees of freedom = number of "surplus" instruments
# We have L = 5 external instruments (g, tx, w2, time, elag) 
# and B = 1 right-hand side endogenous variable (p)
# So df = L - B = 5 - 1 = 4
df <- 5 - 1

# Critical value from chi-square distribution (95th percentile)
critical_value <- qchisq(0.95, df)

# Print results
cat("Sargan Test Results:\n")
cat("RB2 from regression of residuals on instruments:", round(sargan_r2, 4), "\n")
cat("Sargan test statistic (TRB2):", round(sargan_stat, 4), "\n")
cat("Degrees of freedom:", df, "\n")
cat("Critical value (95th percentile of chi-square):", round(critical_value, 4), "\n")
cat("p-value:", round(1 - pchisq(sargan_stat, df), 4), "\n\n")

# Decision
cat("Decision: ")
if(sargan_stat > critical_value) {
  cat("Reject the null hypothesis. The surplus instruments are not valid.\n")
} else {
  cat("Fail to reject the null hypothesis. The surplus instruments appear to be valid.\n")
}

# Print the full regression results to check if any instruments are significant
cat("\nRegression of 2SLS residuals on all instruments:\n")
print(summary(sargan_reg))
