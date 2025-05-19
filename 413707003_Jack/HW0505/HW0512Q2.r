# 11.28
# c

means <- colMeans(truffles[, c("p", "q")])
P_bar <- means["p"]
Q_bar <- means["q"]


b1 <- coef(demand_2sls)["q"]


elasticity_demand <- b1 * (Q_bar / P_bar)
elasticity_demand

# -0.7858767 









