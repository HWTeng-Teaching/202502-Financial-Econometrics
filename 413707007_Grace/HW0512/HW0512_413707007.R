
library(tidyverse)
library(AER)  

#11.28
url <- "https://www.principlesofeconometrics.com/poe5/data/rdata/truffles.rdata"
con <- url(url, "rb")
load(con)
close(con)

# Endogenous: p, q
# Exogenous: ps, di, pf

# Demand equation: P ~ Q + PS + DI, instrument Q with PF
demand_2sls <- ivreg(p ~ q + ps + di | ps + di + pf, data = truffles)
summary(demand_2sls, diagnostics = TRUE)

# Supply equation: P ~ Q + PF, instrument Q with PS and DI
supply_2sls <- ivreg(p ~ q + pf | ps + di + pf, data = truffles)
summary(supply_2sls)

#c: Estimate price elasticity of demand at the means
mean_p <- mean(truffles$p)
mean_q <- mean(truffles$q)
# Get the coefficient of q in the demand equation
gamma1 <- coef(demand_2sls)["q"]
# Calculate price elasticity of demand at the means
elasticity <- (1 / gamma1) * (mean_p / mean_q)
elasticity

#d: Sketch supply and demand equations
# Set exogenous variables to specified values
DI_star <- 3.5
PF_star <- 23
PS_star <- 22

# Create a sequence of Q values
q_seq <- seq(0, 100, by=1)

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

# Plot
plot(q_seq, p_demand, type='l', col='blue', lwd=2, ylim=c(0,100),
     xlab="Quantity (Q)", ylab="Price (P)", main="Supply and Demand")
lines(q_seq, p_supply, col='red', lwd=2)
legend("topright", legend=c("Demand", "Supply"), col=c("blue", "red"), lwd=2)


#e: Calculate equilibrium values
# Find intersection point of supply and demand curves
# At equilibrium: p_demand = p_supply

# Solve for q_eq
q_eq <- (supply_intercept - demand_intercept + supply_pf_coef * PF_star - 
           demand_ps_coef * PS_star - demand_di_coef * DI_star) / 
  (demand_q_coef - supply_q_coef)

# Calculate p_eq using either demand or supply equation
p_eq <- demand_intercept + demand_q_coef * q_eq + demand_ps_coef * PS_star + demand_di_coef * DI_star

cat("Equilibrium quantity:", q_eq, "\n")
cat("Equilibrium price:", p_eq, "\n")

# Compare with reduced-form predictions from Table 11.2
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

#f: Estimate supply and demand equations using OLS
demand_ols <- lm(p ~ q + ps + di, data = truffles)
summary(demand_ols)
supply_ols <- lm(p ~ q + pf, data = truffles)
summary(supply_ols)

#11.30
# Load required packages
library(AER)  # For 2SLS
library(car)  # For linearHypothesis

# Load the data
url <- "https://www.principlesofeconometrics.com/poe5/data/rdata/klein.rdata"
con <- url(url, "rb")
load(con)
close(con)

#a: OLS estimation of investment function
investment_ols <- lm(i ~ p + plag + klag, data = klein)
summary(investment_ols)



#b: Reduced-form equation for profits (P)
klein = na.omit(klein)
profit_rf <- lm(p ~ plag + klag + g + tx + w2 + time + elag, data = klein)
summary(profit_rf)

# Test joint significance of all variables except plag and klag
library(car)
linearHypothesis(profit_rf, c("g = 0", "tx = 0", "w2 = 0", "time = 0", "elag = 0"))

# Load the broom package for augment function
library(broom)
# Use augment to create a dataframe with fitted values and residuals
augmented_data <- augment(profit_rf, data = klein)
View(augmented_data)

#c: Hausman test for endogeneity using augmented data
hausman_test <- lm(i ~ p + plag + klag + .resid, data = augmented_data)
summary(hausman_test)

#d: 2SLS estimation using all instruments
library(AER)
investment_2sls <- ivreg(i ~ p + plag + klag | plag + klag + g + tx + w2 + time + elag, 
                         data = klein)
summary(investment_2sls)


#e: Manual 2SLS using fitted values from augmented data
manual_2sls <- lm(i ~ .fitted + plag + klag, data = augmented_data)
summary(manual_2sls)

# Extract full coefficient information
manual_coef <- summary(manual_2sls)$coefficients
auto_coef <- summary(investment_2sls)$coefficients


#f: Sargan test for instrument validity

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
sargan_stat
# Degrees of freedom = number of "surplus" instruments
# We have L = 5 external instruments (g, tx, w2, time, elag) 
# and B = 1 right-hand side endogenous variable (p)
# So df = L - B = 5 - 1 = 4
df <- 5 - 1

# Critical value from chi-square distribution (95th percentile)
critical_value <- qchisq(0.95, df)
critical_value

