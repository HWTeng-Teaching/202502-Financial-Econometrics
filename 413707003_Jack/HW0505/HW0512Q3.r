# 11.28
# d

DI_star <- 3.5
PF_star <- 23
PS_star <- 22

q_vals <- seq(min(truffles$q), max(truffles$q), length.out=100)

demand_curve <- coef(demand_2sls)["(Intercept)"] +
  coef(demand_2sls)["q"] * q_vals +
  coef(demand_2sls)["di"] * DI_star +
  coef(demand_2sls)["ps"] * PS_star

supply_curve <- coef(supply_2sls)["(Intercept)"] +
  coef(supply_2sls)["q"] * q_vals +
  coef(supply_2sls)["pf"] * PF_star

plot(q_vals, demand_curve, type="l", col="blue", ylim=range(c(demand_curve, supply_curve)),
     ylab="Price", xlab="Quantity", main="Demand and Supply Curves")
lines(q_vals, supply_curve, col="red")
legend("topright", legend=c("Demand", "Supply"), col=c("blue", "red"), lty=1)


# e

eq <- function(q) {
  Pd <- coef(demand_2sls)["(Intercept)"] + coef(demand_2sls)["q"] * q + coef(demand_2sls)["di"] * DI_star + coef(demand_2sls)["ps"] * PS_star
  Ps <- coef(supply_2sls)["(Intercept)"] + coef(supply_2sls)["q"] * q + coef(supply_2sls)["pf"] * PF_star
  Pd - Ps
}

equilibrium <- uniroot(eq, lower=1, upper=200)
Q_eq <- equilibrium$root
P_eq <- coef(demand_2sls)["(Intercept)"] + coef(demand_2sls)["q"] * Q_eq + coef(demand_2sls)["di"] * DI_star + coef(demand_2sls)["ps"] * PS_star
Q_eq; P_eq

# 18.25021

# 62.84257 











