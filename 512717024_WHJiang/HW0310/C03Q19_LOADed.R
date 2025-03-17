### C03Q19
rm(list=ls()) # Caution: this clears the Environment

library(devtools)
# install_github("ccolonescu/POE5Rdata")  # Fixed by TA. Need to use R version 4.2.2 
library(POE5Rdata) 
library(ggplot2)
library(tidyr)

# 載入 Rdata 資料檔案
# temp_file <- tempfile(fileext = ".rdata")
# download.file(
#  url = "https://www.principlesofeconometrics.com/poe5/data/rdata/motel.rdata",
#  destfile = temp_file, 
#  mode = "wb"
# )
# load(temp_file)

######### 檢查資料結構 #####################
str(motel)
summary(motel)
head(motel)
tail(motel)
nrow(motel)


# ----------------------------
# (a) 繪製 motel_pct 與 comp_pct 隨 time 的變化圖
# ----------------------------
# 將資料轉換成長格式以便同一圖中繪製多條曲線
motel_long <- pivot_longer(motel, cols = c("motel_pct", "comp_pct"),
                           names_to = "Variable", values_to = "occupancy")

ggplot(motel_long, aes(x = time, y = occupancy, color = Variable)) +
  geom_line(size = 1) +
  labs(title = "occupancy Rates Over time",
       x = "time",
       y = "occupancy Percentage") +
  theme_minimal()

# 從圖形觀察兩者趨勢是否相似，並比較哪一個系列較高

# ----------------------------
# 估計迴歸模型： motel_pct = β₁ + β₂ * comp_pct + e
# ----------------------------
model <- lm(motel_pct ~ comp_pct, data = motel)
summary(model)

# 取得 β₂ 的 95% 信賴區間
conf_beta2 <- confint(model, "comp_pct", level = 0.95)
cat("95% Confidence Interval for β₂:\n")
print(conf_beta2)
# 若區間較窄，表示 β₂ 的估計較精確

# ----------------------------
# (b) 當 comp_pct = 70 時，建立 motel_pct 的 90% 信賴區間
# ----------------------------
pred_90 <- predict(model, newdata = data.frame(comp_pct = 70),
                     interval = "confidence", level = 0.90)
cat("90% Confidence Interval for expected motel_pct when comp_pct = 70:\n")
print(pred_90)

# ----------------------------
# (c) 檢驗 H₀: β₂ ≦ 0  vs. Hₐ: β₂ > 0 (α = 0.01)
# ----------------------------
# 取出估計值與標準誤
beta2_est <- coef(model)["comp_pct"]
se_beta2 <- coef(summary(model))["comp_pct", "Std. Error"]

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
# 若 H₀ 為真 (β₂ = 1)，則表示當 comp_pct 增加 1 個百分點時，motel_pct 也增加 1 個百分點.

# ----------------------------
# (e) 計算殘差並繪製殘差圖 (殘差 vs. time)
# ----------------------------
motel$residuals <- resid(model)

ggplot(motel, aes(x = time, y = residuals)) +
  geom_point(size = 2) +
  geom_line() +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  labs(title = "Residuals vs time",
       x = "time",
       y = "Residuals") +
  theme_minimal()

# 檢查 time 介於 17 至 23 的期間 (2004年7月至2005年1月)
subset_period <- subset(motel, time >= 17 & time <= 23)
cat("\nSummary of residuals for time 17-23:\n")
print(summary(subset_period$residuals))
cat("Sign counts for time 17-23:\n")
print(table(sign(subset_period$residuals)))
# 觀察這期間殘差是以正值或負值為主，可用來推斷該期間實際入住率相對於預測值的偏差。

