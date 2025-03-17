#CH3_19
#(a)
library(ggplot2)

ggplot(motel, aes(x = time)) +
  geom_line(aes(y = motel_pct, color = "MOTEL_PCT"), size = 1) +
  geom_line(aes(y = comp_pct, color = "COMP_PCT"), size = 1, linetype = "dashed") +
  labs(title = "MOTEL_PCT and COMP_PCT Over Time",
       x = "time", y = "Occupancy Rate (%)",
       color = "Legend") +
  theme_minimal()

reg_model <- lm(motel_pct ~ comp_pct, data = motel)

summary(reg_model)
confint(reg_model, level = 0.95)

#(b)
reg_model <- lm(motel_pct ~ comp_pct, data = motel)

comp_pct_value <- 70

pred <- predict(reg_model, newdata = data.frame(comp_pct = comp_pct_value), interval = "confidence", level = 0.90)

cat("90% Confidence Interval for MOTEL_PCT when COMP_PCT = 70: (", round(pred[1,2], 2), ",", round(pred[1,3], 2), ")\n")
#(c)
# 執行線性回歸
reg_model <- lm(motel_pct ~ comp_pct, data = motel)

# 顯示回歸結果
summary(reg_model)

# 提取 beta2 估計值與標準誤
beta2_hat <- coef(reg_model)[2]  # COMP_PCT 的迴歸係數
se_beta2 <- summary(reg_model)$coefficients[2,2]  # beta2 的標準誤

# 計算 t 統計量
t_stat <- beta2_hat / se_beta2

# 計算 1% 顯著水準的 t 臨界值
df <- nrow(motel) - 2  # 自由度
t_critical <- qt(0.99, df)  # 右尾檢定 t 值

# 顯示結果
cat("t 統計量:", round(t_stat, 4), "\n")
cat("t 臨界值 (α = 0.01):", round(t_critical, 4), "\n")

# 決策
if (t_stat > t_critical) {
  cat("拒絕 H0：COMP_PCT 顯著影響 MOTEL_PCT。\n")
} else {
  cat("無法拒絕 H0：沒有足夠證據證明 COMP_PCT 影響 MOTEL_PCT。\n")
}
#(d)
beta2_hat <- coef(reg_model)[2]  # Estimated coefficient for COMP_PCT
se_beta2 <- summary(reg_model)$coefficients[2,2]  # Standard error of beta2

# Compute the t-statistic for H0: beta2 = 1
t_stat <- (beta2_hat - 1) / se_beta2

# Compute the critical t-value for a two-tailed test at α = 0.01
df <- nrow(motel) - 2  # Degrees of freedom
t_critical <- qt(0.995, df)  # Two-tailed test, so we use α/2 = 0.005

# Display results
cat("t-statistic:", round(t_stat, 4), "\n")
cat("Critical t-value (α = 0.01, two-tailed): ±", round(t_critical, 4), "\n")

# Decision Rule
if (abs(t_stat) > t_critical) {
  cat("Reject H0: COMP_PCT does NOT move one-for-one with MOTEL_PCT.\n")
} else {
  cat("Fail to reject H0: COMP_PCT may move one-for-one with MOTEL_PCT.\n")
}
#(d)
motel$residuals <- residuals(reg_model)

# 4. 繪製殘差 vs 時間 (TIME)
library(ggplot2)
ggplot(motel, aes(x = time, y = residuals)) +
  geom_line(color = "blue", size = 1) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  labs(title = "Residuals vs. Time", x = "Time", y = "Residuals") +
  theme_minimal()
