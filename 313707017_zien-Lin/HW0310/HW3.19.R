# 載入數據集
library(POE5Rdata)
data("motel")

# 繪製 FOODAWAY 的直方圖
library(ggplot2)

# a.
# 繪製折線圖
print(
  ggplot(motel, aes(x = time)) +
  geom_line(aes(y = motel_pct, color = "Motel Occupancy")) +
  geom_line(aes(y = comp_pct, color = "Competitor Occupancy")) +
  labs(title = "Motel vs. Competitor Occupancy Over Time",
       x = "Time",
       y = "Occupancy Percentage") +
  scale_color_manual(values = c("Motel Occupancy" = "blue", "Competitor Occupancy" = "red")) +
  theme_minimal()
)

# 執行線性回歸
model <- lm(motel_pct ~ comp_pct, data = motel)

# 顯示回歸結果
print(summary(model))
print(confint(model, level = 0.95))

# b.
# 設定給定的 COMP_PCT 值
new_data <- data.frame(comp_pct = 70)
pred_result <- predict(model, newdata = new_data, interval = "confidence", level = 0.90)

print(pred_result)

# 計算樣本點數量
n_samples <- nrow(model$model)
print(n_samples)

alpha <- 0.01
df <- 23  # 自由度
t_critical <- qt(1 - alpha, df)  # 單尾檢定臨界值
print(t_critical)

alpha <- 0.01
df <- 23  # 自由度
t_critical <- qt(1 - alpha/2, df)  # 雙尾檢定臨界值
print(t_critical)

# e.
# 計算回歸模型的殘差
residuals <- residuals(model)

# 繪製殘差與時間的散點圖
print(ggplot(data = motel, aes(x = time, y = residuals)) +
  geom_point(color = "blue") +
  labs(title = "Residuals vs Time",
       x = "Time",
       y = "Residuals") +
  theme_minimal()
)

