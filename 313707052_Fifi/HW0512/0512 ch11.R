library(POE5Rdata)
data("truffles", package = "POE5Rdata")

names(truffles)

#b.

library(AER)  

demand_2sls <- ivreg(p ~ q + ps + di | pf + ps + di, data = truffles)
summary(demand_2sls)

supply_2sls <- ivreg(p ~ q + pf | di + ps + pf, data = truffles)
summary(supply_2sls)

#c.

mean_p <- mean(truffles$p)
mean_q <- mean(truffles$q)

coef_demand <- coef(demand_2sls)
dq_dp <- 1 / coef_demand["q"]  

elasticity <- dq_dp * (mean_p / mean_q)
elasticity

#ans c.q -1.272464

#d.

# 建構一組 Q 值範圍
q_vals <- seq(min(truffles$q), max(truffles$q), length.out = 100)

# 根據 2SLS 模型建立 demand & supply P 值
ps_val <- 22
di_val <- 3.5
pf_val <- 23

demand_price <- coef(demand_2sls)["(Intercept)"] +
  coef(demand_2sls)["q"] * q_vals +
  coef(demand_2sls)["ps"] * ps_val +
  coef(demand_2sls)["di"] * di_val

supply_price <- coef(supply_2sls)["(Intercept)"] +
  coef(supply_2sls)["q"] * q_vals +
  coef(supply_2sls)["pf"] * pf_val

# 畫圖
plot(q_vals, demand_price, type = "l", col = "blue", ylim = range(demand_price, supply_price),
     xlab = "Quantity (Q)", ylab = "Price (P)", main = "Supply and Demand")
lines(q_vals, supply_price, col = "red")
legend("topright", legend = c("Demand", "Supply"), col = c("blue", "red"), lty = 1)

#e.

a0 <- coef(demand_2sls)["(Intercept)"] + coef(demand_2sls)["ps"] * ps_val + coef(demand_2sls)["di"] * di_val
a1 <- coef(demand_2sls)["q"]

b0 <- coef(supply_2sls)["(Intercept)"] + coef(supply_2sls)["pf"] * pf_val
b1 <- coef(supply_2sls)["q"]

q_eq <- (a0 - b0) / (b1 - a1)
p_eq <- a0 + a1 * q_eq

c(q_eq = q_eq, p_eq = p_eq)

#ans e.q_eq.(Intercept) p_eq.(Intercept) 18.25021         62.84257 

#f.

ols_demand <- lm(p ~ q + ps + di, data = truffles)
summary(ols_demand)

ols_supply <- lm(p ~ q + pf, data = truffles)
summary(ols_supply)

#11.28

data("klein", package = "POE5Rdata")

names(klein)

#a.

ols_model <- lm(i ~ p + plag + klag, data = klein)
summary(ols_model)

#b.


exog_predet <- c("cn", "w1", "w2", "g", "tx", "elag", "time", "y")

rf_formula <- as.formula(paste("p ~", paste(exog_predet, collapse = " + ")))
rf_model <- lm(rf_formula, data = klein, na.action = na.exclude)
summary(rf_model)


klein$p_hat <- fitted(rf_model)
klein$v_hat <- resid(rf_model)

#c.


hausman_model <- lm(i ~ p + plag + klag + v_hat, data = klein)
summary(hausman_model)

#d.

library(AER)

# 2SLS 結構式
iv_model <- ivreg(i ~ p + plag + klag | cn + w1 + w2 + g + tx + elag + time + y + plag + klag, data = klein)
summary(iv_model)

#e.

# OLS with fitted value of p
stage2_model <- lm(i ~ p_hat + plag + klag, data = klein, na.action = na.exclude)
summary(stage2_model)

#f.

# 殘差
klein$e2_hat <- resid(stage2_model)

# 使用所有外生與預定變數回歸殘差
sargan_formula <- as.formula(paste("e2_hat ~", paste(exog_predet, collapse = " + ")))
sargan_model <- lm(sargan_formula, data = klein, na.action = na.exclude)
summary(sargan_model)

# 取 R² 與檢定統計量 TR²
R2 <- summary(sargan_model)$r.squared
N <- nrow(klein)
TR2 <- N * R2

# 比較 TR² 和臨界值（卡方分佈，自由度 = L - B = 5 - 1 = 4）
critical_value <- qchisq(0.95, df = 4)

cat("TR² =", TR2, "\n95th percentile chi²(4) =", critical_value, "\n")
if (TR2 < critical_value) {
  cat("✅ 工具變數有效（不拒絕 H0）\n")
} else {
  cat("❌ 工具變數可能無效（拒絕 H0）\n")
}

#ansf.工具變數可能無效（拒絕 H0）