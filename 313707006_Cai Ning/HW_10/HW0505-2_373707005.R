#11.28
library(POE5Rdata)
data("truffles")

#(b)
install.packages("AER")
library(AER)

demand_iv <- ivreg(p ~ q + ps + di | ps + di + pf, data = truffles)
summary(demand_iv)

supply_iv <- ivreg(p ~ q + pf | ps + di + pf, data = truffles)
summary(supply_iv)

#(c)
coef_q <- coef(demand_iv)["q"]         # 這是 dP/dQ
mean_p <- mean(truffles$p)
mean_q <- mean(truffles$q)

# 反推彈性
elasticity <- (1 / coef_q) * (mean_p / mean_q)
elasticity

#(d)
library(AER)
data(truffles)

# 重新估計需求與供給
demand_iv <- ivreg(p ~ q + ps + di | ps + di + pf, data = truffles)
supply_iv <- ivreg(p ~ q + pf | ps + di + pf, data = truffles)

# 取出係數
b_d <- coef(demand_iv)  # demand: intercept, q, ps, di
b_s <- coef(supply_iv)  # supply: intercept, q, pf

# 固定外生變數
ps_star <- 22
di_star <- 3.5
pf_star <- 23

# 建立 Q 序列
q_range <- seq(min(truffles$q), max(truffles$q), length.out = 100)

# 根據估計式建立 P_hat（固定外生變數）
p_demand <- b_d[1] + b_d["q"] * q_range + b_d["ps"] * ps_star + b_d["di"] * di_star
p_supply <- b_s[1] + b_s["q"] * q_range + b_s["pf"] * pf_star

# 畫圖
plot(q_range, p_demand, type = "l", col = "blue", lwd = 2,
     xlab = "Quantity (Q)", ylab = "Price (P)", ylim = range(c(p_demand, p_supply)))
lines(q_range, p_supply, col = "red", lwd = 2)
legend("topright", legend = c("Demand", "Supply"),
       col = c("blue", "red"), lwd = 2)

#(e)
# 假設這是估計出來的係數：
b_d <- coef(demand_iv)  # p = a0 + a1*q + a2*ps + a3*di
b_s <- coef(supply_iv)  # p = b0 + b1*q + b2*pf

# 固定外生變數
ps_star <- 22
di_star <- 3.5
pf_star <- 23

# 建立聯立方程：解 demand = supply
# a0 + a1*q + a2*ps + a3*di = b0 + b1*q + b2*pf
a0 <- b_d[1]; a1 <- b_d["q"]; a2 <- b_d["ps"]; a3 <- b_d["di"]
b0 <- b_s[1]; b1 <- b_s["q"]; b2 <- b_s["pf"]

# 解聯立方程，先算 Q*
q_star <- (b0 + b2*pf_star - a0 - a2*ps_star - a3*di_star) / (a1 - b1)

# 再代回 demand 或 supply 任一式算 P*
p_star <- a0 + a1*q_star + a2*ps_star + a3*di_star

# 結果
q_star
p_star

# Reduced-form 回歸
rf_Q <- lm(q ~ ps + di + pf, data = truffles)
rf_P <- lm(p ~ ps + di + pf, data = truffles)

# 指定外生變數值
newX <- data.frame(ps = ps_star, di = di_star, pf = pf_star)

# 代入預測
Q_hat <- predict(rf_Q, newdata = newX)
P_hat <- predict(rf_P, newdata = newX)

# 回傳 reduced-form 預測值
c(Q_hat = Q_hat, P_hat = P_hat)

#(f)
# 需求方程 OLS
demand_ols <- lm(p ~ q + ps + di, data = truffles)
summary(demand_ols)

# 供給方程 OLS
supply_ols <- lm(p ~ q + pf, data = truffles)
summary(supply_ols)

ols_d <- summary(demand_ols)$coefficients
iv_d  <- summary(demand_iv)$coefficients

ols_s <- summary(supply_ols)$coefficients
iv_s  <- summary(supply_iv)$coefficients

# 整理 demand 比較表
compare_demand <- data.frame(
  Variable      = rownames(ols_d),
  OLS_Estimate  = round(ols_d[, 1], 4),
  OLS_StdError  = round(ols_d[, 2], 4),
  OLS_pValue    = round(ols_d[, 4], 4),
  IV_Estimate   = round(iv_d[, 1], 4),
  IV_StdError   = round(iv_d[, 2], 4),
  IV_pValue     = round(iv_d[, 4], 4)
)

# 整理 supply 比較表
compare_supply <- data.frame(
  Variable      = rownames(ols_s),
  OLS_Estimate  = round(ols_s[, 1], 4),
  OLS_StdError  = round(ols_s[, 2], 4),
  OLS_pValue    = round(ols_s[, 4], 4),
  IV_Estimate   = round(iv_s[, 1], 4),
  IV_StdError   = round(iv_s[, 2], 4),
  IV_pValue     = round(iv_s[, 4], 4)
)

# 顯示
cat("=== Demand Function Comparison ===\n")
print(compare_demand)

cat("\n=== Supply Function Comparison ===\n")
print(compare_supply)

#11.30
library(POE5Rdata)
data("klein")

#(a)
install.packages("dynlm")

library(dynlm)
data("klein")

summary(klein)
ols_inv <- lm(i ~ p + plag + klag, data = klein)
cat("\n(a)  OLS estimates of 11.18:\n")
summary(ols_inv)

#(b)
library(car)  
library(dplyr)

rf_P <- lm(p ~ g + w2 + tx + time + plag + klag + elag, data = klein)
summary(rf_P)


joint_test <- linearHypothesis(
  rf_P,
  c("g = 0", "w2 = 0", "tx = 0", "time = 0", "elag = 0")
)
cat("\nJoint F-test (g, w2, tx, time, elag):\n")
print(joint_test, digits = 3)

F_stat <- joint_test$F[2]       
df1    <- joint_test$Df[2]      
df2    <- joint_test$Res.Df[2]  
F_crit <- qf(0.95, df1, df2)   

cat("F statistic =", round(F_stat, 3), "\n")
cat("Critical F(5,13;0.95) =", round(F_crit, 3), "\n")

df <- klein |>
  as.data.frame() |>
  select(year, cn, i, p, plag, klag, e, elag, w2, g, tx, time) |>
  na.omit()                     
nrow(df)  

rf_P  <- lm(p ~ g + w2 + tx + time + plag + klag + elag, data = df)
df$phat <- fitted(rf_P)
df$vhat <- resid(rf_P)
df$phat

#(c)
hausman <- lm(i ~ p + plag + klag + vhat, data = df)
cat("\n(c)  Hausman test (t-stat of vhat):\n")
print(coeftest(hausman, vcov. = vcovHC(hausman, type = "HC1"))["vhat", ])
summary(hausman)

#(d)
iv_2sls <- ivreg(i ~ p + plag + klag | g + w2 + tx + time + elag + plag + klag, data = df)

# 顯示結果
cat("(d) 2SLS estimation of the investment equation:\n")
summary(iv_2sls)

library(broom)

ols_model <- lm(i ~ p + plag + klag, data = df)

compare <- cbind(
  OLS  = tidy(ols_model)[, c("estimate", "p.value")],
  `2SLS` = tidy(iv_2sls)[, c("estimate", "p.value")]
)
print(compare, row.names = FALSE)

#(e)
stage2 <- lm(i ~ phat + plag + klag, data = df)

cat("(e) Manual second-stage OLS using predicted profits:\n")
summary(stage2)

# (f) Sargan test
e2hat <- resid(iv_2sls)  # 正確使用前面建立的 iv_2sls 模型
sargan_aux <- lm(e2hat ~ g + w2 + tx + time + elag + plag + klag, data = df)
R2  <- summary(sargan_aux)$r.squared
TR2 <- nrow(df) * R2
df_sargan <- 4                      # L - B = 7 instruments - 3 endogenous vars = 4
crit95 <- qchisq(0.95, df_sargan)

cat("\n(f)  Sargan test:\n")
cat("   TR^2  =", round(TR2, 3), "\n")
cat("   χ²_0.95(df=4) =", round(crit95, 3), "\n")
if (TR2 < crit95) {
  cat("   → Fail to reject H0: surplus instruments appear valid.\n")
} else {
  cat("   → Reject H0: at least one surplus instrument may be invalid.\n")
}
