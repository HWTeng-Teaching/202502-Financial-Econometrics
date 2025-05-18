library(AER)       
library(ggplot2)  
library(dplyr)     
library(readr)     
library(POE5Rdata)

data('truffles')

# (b)
demand_2sls <- ivreg(p ~ q + ps + di | ps + di + pf, data = truffles)
supply_2sls <- ivreg(p ~ q + pf     | ps + di + pf, data = truffles)
summary(demand_2sls)
summary(supply_2sls)

# (c)
q_bar <- mean(truffles$q)
p_bar <- mean(truffles$p)
alpha1 <- coef(demand_2sls)["q"]

elasticity_demand <- (1 / alpha1) * (p_bar / q_bar)
cat("Price elasticity of demand at means:", elasticity_demand, "\n")

# (d)
di_star <- 3.5
pf_star <- 23
ps_star <- 22
q_seq <- seq(min(truffles$q), max(truffles$q), length.out = 100)
demand_pred <- coef(demand_2sls)["(Intercept)"] +
  coef(demand_2sls)["q"] * q_seq +
  coef(demand_2sls)["ps"] * ps_star +
  coef(demand_2sls)["di"] * di_star
supply_pred <- coef(supply_2sls)["(Intercept)"] +
  coef(supply_2sls)["q"] * q_seq +
  coef(supply_2sls)["pf"] * pf_star
plot(q_seq, demand_pred, type = "l", col = "blue", lwd = 2,
     ylab = "Price (P)", xlab = "Quantity (Q)", main = "Supply and Demand")
lines(q_seq, supply_pred, col = "red", lwd = 2)
legend("topright", legend = c("Demand", "Supply"), col = c("blue", "red"), lwd = 2)

# (e)
a0 <- coef(demand_2sls)["(Intercept)"] + coef(demand_2sls)["ps"] * ps_star + coef(demand_2sls)["di"] * di_star
a1 <- coef(demand_2sls)["q"]
b0 <- coef(supply_2sls)["(Intercept)"] + coef(supply_2sls)["pf"] * pf_star
b1 <- coef(supply_2sls)["q"]
q_eq <- (b0 - a0) / (a1 - b1)
p_eq <- a0 + a1 * q_eq
cat("Equilibrium Q:", q_eq, "\n")
cat("Equilibrium P:", p_eq, "\n")

# (f)
demand_ols <- lm(p ~ q + ps + di, data = truffles)
supply_ols <- lm(p ~ q + pf, data = truffles)
summary(demand_ols)
summary(supply_ols)


