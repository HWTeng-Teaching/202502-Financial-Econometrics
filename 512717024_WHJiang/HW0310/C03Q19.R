### C03Q19

# 載入必要套件
library(ggplot2)
library(tidyr)

# 讀取資料 (假設檔案為 motel.csv)
motel <- read.csv("motel.csv")

# ----------------------------
# (a) 繪製 MOTEL_PCT 與 COMP_PCT 隨 TIME 的變化圖
# ----------------------------
# 將資料轉換成長格式以便同一圖中繪製多條曲線
motel_long <- pivot_longer(motel, cols = c("MOTEL_PCT", "COMP_PCT"),
                           names_to = "Variable", values_to = "Occupancy")

ggplot(motel_long, aes(x = TIME, y = Occupancy, color = Variable)) +
  geom_line(size = 1) +
  labs(title = "Occupancy Rates Over Time",
       x = "Time",
       y = "Occupancy Percentage") +
  theme_minimal()

# 從圖形觀察兩者趨勢是否相似，並比較哪一個系列較高

# ----------------------------
# 估計迴歸模型： MOTEL_PCT = β₁ + β₂ * COMP_PCT + e
# ----------------------------
model <- lm(MOTEL_PCT ~ COMP_PCT, data = motel)
summary(model)

# 取得 β₂ 的 95% 信賴區間
conf_beta2 <- confint(model, "COMP_PCT", level = 0.95)
cat("95% Confidence Interval for β₂:\n")
print(conf_beta2)
# 若區間較窄，表示 β₂ 的估計較精確

# ----------------------------
# (b) 當 COMP_PCT = 70 時，建立 MOTEL_PCT 的 90% 信賴區間
# ----------------------------
pred_90 <- predict(model, newdata = data.frame(COMP_PCT = 70),
                     interval = "confidence", level = 0.90)
cat("90% Confidence Interval for expected MOTEL_PCT when COMP_PCT = 70:\n")
print(pred_90)

# ----------------------------
# (c) 檢驗 H₀: β₂ ≦ 0  vs. Hₐ: β₂ > 0 (α = 0.01)
# ----------------------------
# 取出估計值與標準誤
beta2_est <- coef(model)["COMP_PCT"]
se_beta2 <- coef(summary(model))["COMP_PCT", "Std. Error"]

# 計算 t 統計量
t_stat_c <- (beta2_est - 0) / se_beta2
df_model <- model$df.residual
# 單尾檢定下的臨界值 (α=0.01)
t_crit_c <- qt(0.99, df_model)
# 一尾 p 值
p_value_c <- 1 - pt(t_stat_c, df_model)

cat("\nPart (c) - Test of H0: β₂ ≦ 0\n")
cat("t-statistic =", round(t_stat_c, 3), "\n")
cat("Critical t-value (α = 0.01) =", round(t_crit_c, 3), "\n")
cat("One-tailed p-value =", round(p_value_c, 4), "\n")
if (t_stat_c > t_crit_c) {
  cat("Conclusion: Reject H0. There is significant evidence that β₂ > 0.\n")
} else {
  cat("Conclusion: Fail to reject H0.\n")
}

# ----------------------------
# (d) 檢驗 H₀: β₂ = 1  vs. Hₐ: β₂ ≠ 1 (α = 0.01)
# ----------------------------
t_stat_d <- (beta2_est - 1) / se_beta2
# 兩尾檢定臨界值 (0.5%在每尾)
t_crit_d <- qt(0.995, df_model)
p_value_d <- 2 * (1 - pt(abs(t_stat_d), df_model))

cat("\nPart (d) - Test of H0: β₂ = 1\n")
cat("t-statistic =", round(t_stat_d, 3), "\n")
cat("Critical t-value (two-tailed, α = 0.01) =", round(t_crit_d, 3), "\n")
cat("Two-tailed p-value =", round(p_value_d, 4), "\n")
if (abs(t_stat_d) > t_crit_d) {
  cat("Conclusion: Reject H0. The relationship significantly differs from a one-for-one association.\n")
} else {
  cat("Conclusion: Fail to reject H0. The data are consistent with a one-for-one occupancy relationship.\n")
}
# 若 H₀ 為真 (β₂ = 1)，則表示當 COMP_PCT 增加 1 個百分點時，MOTEL_PCT 也增加 1 個百分點.

# ----------------------------
# (e) 計算殘差並繪製殘差圖 (殘差 vs. TIME)
# ----------------------------
motel$residuals <- resid(model)

ggplot(motel, aes(x = TIME, y = residuals)) +
  geom_point(size = 2) +
  geom_line() +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  labs(title = "Residuals vs Time",
       x = "Time",
       y = "Residuals") +
  theme_minimal()

# 檢查 TIME 介於 17 至 23 的期間 (2004年7月至2005年1月)
subset_period <- subset(motel, TIME >= 17 & TIME <= 23)
cat("\nSummary of residuals for TIME 17-23:\n")
print(summary(subset_period$residuals))
cat("Sign counts for TIME 17-23:\n")
print(table(sign(subset_period$residuals)))
# 觀察這期間殘差是以正值或負值為主，可用來推斷該期間實際入住率相對於預測值的偏差。
