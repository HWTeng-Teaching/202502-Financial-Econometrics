truffles <- read.csv("truffles.csv")

## a. Rewriting the system with P on the left-hand side:
# Demand: P = a0 + a1*Q + a2*DI + u1   (expect a1 > 0, a2 < 0)
# Supply: P = b0 + b1*Q + b2*PS + b3*PF + u2 (expect b1 > 0, b2 > 0, b3 < 0)

## b. Estimate with 2SLS
# First stage: instrument Q using exogenous variables (DI, PS, PF)
demand_iv <- ivreg(P ~ Q + DI | DI + PS + PF, data = truffles)
supply_iv <- ivreg(P ~ Q + PS + PF | DI + PS + PF, data = truffles)

summary(demand_iv)
summary(supply_iv)

## c. Price elasticity of demand at the means
mean_Q <- mean(truffles$Q)
mean_P <- mean(truffles$P)
demand_coef <- coef(demand_iv)
price_elasticity <- demand_coef["Q"] * (mean_Q / mean_P)
cat("Price elasticity of demand:", price_elasticity, "\n")

## d. Plot supply and demand curves using fitted model
DI_star <- 3.5
PF_star <- 23
PS_star <- 22

Q_vals <- seq(min(truffles$Q), max(truffles$Q), length.out = 100)

# Demand: P = a0 + a1*Q + a2*DI
demand_P <- coef(demand_iv)[1] + coef(demand_iv)["Q"] * Q_vals + coef(demand_iv)["DI"] * DI_star

# Supply: P = b0 + b1*Q + b2*PS + b3*PF
supply_P <- coef(supply_iv)[1] + coef(supply_iv)["Q"] * Q_vals + 
             coef(supply_iv)["PS"] * PS_star + coef(supply_iv)["PF"] * PF_star

plot_data <- data.frame(Q = Q_vals, Demand = demand_P, Supply = supply_P)

ggplot(plot_data, aes(x = Q)) +
  geom_line(aes(y = Demand, color = "Demand"), size = 1.2) +
  geom_line(aes(y = Supply, color = "Supply"), size = 1.2) +
  labs(y = "Price (P)", x = "Quantity (Q)", title = "Estimated Demand and Supply Curves") +
  scale_color_manual(values = c("Demand" = "blue", "Supply" = "red")) +
  theme_minimal()

## e. Find equilibrium where Demand = Supply
# Find intersection of predicted demand_P and supply_P
equilibrium_index <- which.min(abs(demand_P - supply_P))
P_eq <- demand_P[equilibrium_index]
Q_eq <- Q_vals[equilibrium_index]
cat("Equilibrium P:", P_eq, "\n")
cat("Equilibrium Q:", Q_eq, "\n")

## f. Estimate both equations with OLS
demand_ols <- lm(P ~ Q + DI, data = truffles)
supply_ols <- lm(P ~ Q + PS + PF, data = truffles)

summary(demand_ols)
summary(supply_ols)
