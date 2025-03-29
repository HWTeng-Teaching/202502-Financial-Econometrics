#CH5Q03
# 已知的回歸結果
b1 <- 1.4515
se_b1 <- 2.2019
t_b1 <- b1 / se_b1  # 已知

b2 <- 2.7648
t_b2 <- 5.7103  # 已知
se_b2 <- b2 / t_b2  # 計算標準誤差

t_b3 <- -3.9376  # 已知
se_b3 <- 0.3695  # 已知
b3 <- t_b3 * se_b3  # 計算 b3

b4 <- -0.1503
se_b4 <- 0.0235
t_b4 <- -6.4019  # 已知


# 計算 95% 信賴區間 for β4
alpha <- 0.05
df <- 1200 - 4  # 自由度 = 樣本數 - 變數數量
t_critical <- qt(1 - alpha/2, df)  # 取得 t 臨界值
ci_b4_lower <- b4 - t_critical * se_b4
ci_b4_upper <- b4 + t_critical * se_b4
ci <- c(ci_b4_lower, ci_b4_upper)
print(ci)

# 假設檢定: H0: β3 = -2 vs. H1: β3 ≠ -2
H0_b3 <- -2
t_test_b3 <- (b3 - H0_b3) / se_b3
p_value_b3 <- 2 * (1 - pt(abs(t_test_b3), df))
print(t_test_b3)


#CH5Q23
library(POE5Rdata)
data(cocaine)

# (b)執行線性回歸
model <- lm(price ~ quant + qual + trend, data = cocaine)

# 顯示回歸結果
summary(model)

# (c)提取 R²
r_squared <- summary(model)$r.squared
cat("R-squared:", r_squared, "\n")

# (d)提取 QUANT 的 t 值和 p 值
quant_coef <- summary(model)$coefficients["quant", ]
t_quant <- quant_coef["t value"]
p_quant <- quant_coef["Pr(>|t|)"]

cat("t-statistic for QUANT:", t_quant, "\n")
cat("p-value for QUANT:", p_quant, "\n")

# 檢查 p 值是否小於顯著水準 0.05
if (p_quant < 0.05) {
  cat("拒絕 H0，QUANT 對價格有顯著影響，較大交易量確實可能導致較低價格。\n")
} else {
  cat("無法拒絕 H0，數量對價格的影響不顯著。\n")
}

# (e)提取 QUAL 的 t 值和 p 值
qual_coef <- summary(model)$coefficients["qual", ]
t_qual <- qual_coef["t value"]
p_qual <- qual_coef["Pr(>|t|)"]

cat("t-statistic for QUAL:", t_qual, "\n")
cat("p-value for QUAL:", p_qual, "\n")

# 檢查 p 值是否小於顯著水準 0.05
if (p_qual < 0.05) {
  cat("拒絕 H0，QUAL 對價格有顯著影響，較高純度的可卡因確實可能有價格溢價。\n")
} else {
  cat("無法拒絕 H0，純度對價格的影響不顯著。\n")
}

# (f)提取 TREND 的估計係數
beta_4 <- summary(model)$coefficients["trend", "Estimate"]
cat("每年的平均價格變動:", beta_4, "\n")

