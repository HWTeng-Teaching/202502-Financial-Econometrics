library(POE5Rdata)
data("motel", package = "POE5Rdata")

names(motel)

#a.

#plot the graph

library(ggplot2)

ggplot(motel, aes(x = time)) +
  geom_line(aes(y = motel_pct, color = "Motel Occupancy")) +
  geom_line(aes(y = comp_pct, color = "Competitor Occupancy")) +
  labs(title = "Occupancy Rates Over Time",
       x = "Time",
       y = "Occupancy Rate (%)",
       color = "Legend") +
  theme_minimal()

#linear regression

model <- lm(motel_pct ~ comp_pct, data = motel)
summary(model)

# 95% CI

confint(model, "comp_pct", level = 0.95)

#B.

new_data <- data.frame(comp_pct = 70)
predict(model, new_data, interval = "confidence", level = 0.90)

#c.

# 提取 beta2 估計值與標準誤
beta2 <- coef(summary(model))["comp_pct", "Estimate"]
se_beta2 <- coef(summary(model))["comp_pct", "Std. Error"]

# 計算 t 統計量
t_stat <- beta2 / se_beta2

# 計算單尾 p 值
p_value <- pt(t_stat, df = model$df.residual, lower.tail = FALSE)

# 顯示結果
t_stat
p_value  # 若 p_value < 0.01 則拒絕 H0

#D.

# 計算 t 統計量 (測試 beta_2 是否等於 1)
t_stat_1 <- (beta2 - 1) / se_beta2

# 計算雙尾 p 值
p_value_1 <- 2 * pt(-abs(t_stat_1), df = model$df.residual)

# 顯示結果
t_stat_1
p_value_1  # 若 p_value_1 < 0.01 則拒絕 H0

#E.

# 計算殘差
motel$residuals <- resid(model)

# 繪製殘差 vs 時間的散點圖
ggplot(motel, aes(x = time, y = residuals)) +
  geom_point() +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  labs(title = "Residuals vs Time", x = "Time", y = "Residuals") +
  theme_minimal()

# 觀察 17 到 23 時段的殘差符號
subset(motel, time >= 17 & time <= 23, select = residuals)



