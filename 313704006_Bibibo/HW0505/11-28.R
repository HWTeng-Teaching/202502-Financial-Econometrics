#install.packages("systemfit")
library(systemfit)
library(POE5Rdata)
data("truffles")
p = truffles$p
q = truffles$q
ps = truffles$ps
di = truffles$di
pf = truffles$pf

#b
truffle.d = p ~ q + ps + di
truffle.s = p ~ q + pf
truffle.eqs = list(truffle.d, truffle.s)
truffle.ivs = ~ ps+di+pf
truffle.sys = systemfit(truffle.eqs, method = "2SLS", 
                        inst = truffle.ivs, data = truffles)
summary(truffle.sys)

#c
coef_demand <- coef(truffle.sys$eq[[1]])
coef_supply <- coef(truffle.sys$eq[[2]])

q_mean <- mean(truffles$q)
ps_mean <- mean(truffles$ps)
di_mean <- mean(truffles$di)
#demand elasticity
m = truffle.sys$coefficients[2]
fitted_p =  coef_demand["(Intercept)"] + coef_demand["q"] * q_mean +
            coef_demand["ps"] * ps_mean + coef_demand["di"] * di_mean
demand_elasticity = m * q_mean / fitted_p

#d
di_star = 3.5
pf_star = 23
ps_star = 22

# 建立一系列的 Q 值（橫軸）
q_seq = seq(min(truffles$q), max(truffles$q), length.out = 100)

# 對應的 demand P 值
p_demand = coef_demand["(Intercept)"] + coef_demand["q"] * q_seq +
            coef_demand["ps"] * ps_star + coef_demand["di"] * di_star

# 對應的 supply P 值
p_supply = coef_supply["(Intercept)"] + coef_supply["q"] * q_seq +
           coef_supply["pf"] * pf_star

# 畫圖
plot(q_seq, p_demand, type = "l", col = "blue", lwd = 2,
     xlab = "Quantity (Q)", ylab = "Price (P)",
     main = "Demand and Supply Curves")
lines(q_seq, p_supply, col = "red", lwd = 2)
legend("bottomleft", legend = c("Demand", "Supply"),
       col = c("blue", "red"), lwd = 2)

#e
#demand: p = a + b*q
#supply: p = c + d*q
a <- coef_demand["(Intercept)"] + coef_demand["ps"] * ps_star + coef_demand["di"] * di_star
b <- coef_demand["q"]
c <- coef_supply["(Intercept)"] + coef_supply["pf"] * pf_star
d <- coef_supply["q"]
q_star = (c - a)/(b - d)
p_star = a + b * q_star
cat("Equilibrium (from structural model):\n")
cat(sprintf("Q* = %.4f\n", q_star))
cat(sprintf("P* = %.4f\n", p_star))

# Reduced-form Q: Q = π0 + π1*ps + π2*di + π3*pf
reduced_q <- lm(q ~ ps + di + pf, data = truffles)
reduced_p <- lm(p ~ ps + di + pf, data = truffles)

# 帶入外生變數值
exog <- data.frame(ps = ps_star, di = di_star, pf = pf_star)
q_hat <- predict(reduced_q, newdata = exog)
p_hat <- predict(reduced_p, newdata = exog)

cat("redicted values from reduced-form:\n")
cat(sprintf("Q_hat = %.4f\n", q_hat))
cat(sprintf("P_hat = %.4f\n", p_hat))

# 建立比較表格
comparison_table <- data.frame(
  Method = c("Structural model (2SLS)", "Reduced-form (OLS)"),
  Quantity = c(q_star, q_hat),
  Price = c(p_star, p_hat)
)

# 顯示表格
print(comparison_table, row.names = FALSE)

#f
demand_ols = lm(p ~ q+ps+di, data = truffles)
supply_ols = lm(p ~ q+pf, data = truffles)
summary(demand_ols)
summary(supply_ols)

# 提取 OLS 和 2SLS 的 summary 結果
ols_demand_sum <- summary(demand_ols)
ols_supply_sum <- summary(supply_ols)
tsls_demand_sum <- summary(truffle.sys$eq[[1]])
tsls_supply_sum <- summary(truffle.sys$eq[[2]])

# 把每個模型的係數資料整理成 data.frame
get_coef_df <- function(model_summary, method, equation) {
  coef_mat <- coef(model_summary)
  data.frame(
    Method = method,
    Equation = equation,
    Term = rownames(coef_mat),
    Estimate = round(coef_mat[, 1], 4),
    StdError = round(coef_mat[, 2], 4),
    tValue = round(coef_mat[, 3], 2),
    pValue = round(coef_mat[, 4], 4),
    row.names = NULL
  )
}

# 建立每個模型的係數表
df_ols_demand <- get_coef_df(ols_demand_sum, "OLS", "Demand")
df_ols_supply <- get_coef_df(ols_supply_sum, "OLS", "Supply")
df_tsls_demand <- get_coef_df(tsls_demand_sum, "2SLS", "Demand")
df_tsls_supply <- get_coef_df(tsls_supply_sum, "2SLS", "Supply")

# 合併所有資料
comparison_table <- rbind(df_ols_demand, df_tsls_demand,
                          df_ols_supply, df_tsls_supply)

# 顯示比較表
print(comparison_table)
