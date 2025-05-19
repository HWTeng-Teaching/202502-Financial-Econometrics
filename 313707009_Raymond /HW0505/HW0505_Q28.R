if (!requireNamespace("remotes", quietly = TRUE)) install.packages("remotes")
remotes::install_github("ccolonescu/POE5Rdata", force = TRUE)
install.packages("AER")  
library(AER)
library(POE5Rdata)
data('truffles')

# (b) 2SLS
demand_2sls <- ivreg(p ~ q + ps + di | ps + di + pf, data = truffles)
summary(demand_2sls)

supply_2sls <- ivreg(p ~ q + pf | pf + ps + di, data = truffles)
summary(supply_2sls)

# (c) 平均的點彈性
mean_q <- mean(truffles$q)
mean_p <- mean(truffles$p)

gamma_2 <- coef(demand_2sls)["q"]
dq_dp <- 1 / gamma_2
elasticity <- dq_dp * (mean_p / mean_q)
cat("需求的價格彈性（at the means）為：", round(elasticity, 3), "\n")

# (d) 供給與需求曲線
ps_star <- 22
di_star <- 3.5
pf_star <- 23

q_vals <- seq(min(truffles$q), max(truffles$q), length.out = 100)

coef_d <- coef(demand_2sls)
coef_s <- coef(supply_2sls)

# 需求曲線的預測 p 值
p_demand <- coef_d["(Intercept)"] +
  coef_d["q"] * q_vals +
  coef_d["ps"] * ps_star +
  coef_d["di"] * di_star

# 供給曲線的預測 p 值
p_supply <- coef_s["(Intercept)"] +
  coef_s["q"] * q_vals +
  coef_s["pf"] * pf_star
# 繪圖
plot(q_vals, p_demand, type = "l", col = "blue", lwd = 2,
     xlab = "Quantity (Q)", ylab = "Price (P)",
     main = "Estimated Supply and Demand Curves")
lines(q_vals, p_supply, col = "red", lwd = 2)
legend("topright", legend = c("Demand", "Supply"),
       col = c("blue", "red"), lwd = 2)

# (e) 需求與供給均衡
gamma0 <- coef(demand_2sls)["(Intercept)"]
gamma1 <- coef(demand_2sls)["q"]
gamma2 <- coef(demand_2sls)["ps"]
gamma3 <- coef(demand_2sls)["di"]

delta0 <- coef(supply_2sls)["(Intercept)"]
delta1 <- coef(supply_2sls)["q"]
delta2 <- coef(supply_2sls)["pf"]

# 聯立解 q: p_demand = p_supply

numerator <- delta0 - gamma0 + delta2 * pf_star - gamma2 * ps_star - gamma3 * di_star
denominator <- gamma1 - delta1

q_eq <- numerator / denominator
p_eq <- gamma0 + gamma1 * q_eq + gamma2 * ps_star + gamma3 * di_star

cat("結構模型下的均衡數量 Q* =", round(q_eq, 3), "\n")
cat("結構模型下的均衡價格 P* =", round(p_eq, 3), "\n")

# (f) OLS之估計值
demand_ols <- lm(p ~ q + ps + di, data = truffles)
summary(demand_ols)

supply_ols <- lm(p ~ q + pf, data = truffles)
summary(supply_ols)

