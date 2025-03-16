rm(list=ls())  
library(POE5Rdata)  
data("motel")  

#(a)
library(ggplot2)

ggplot(motel, aes(x = time)) +
  geom_line(aes(y = motel_pct, color = "motel")) +
  geom_line(aes(y = comp_pct, color = "competitor")) +
  labs(title = "Occupancy Rates Over Time",
       x = "Time",
       y = "Occupancy Percentage") +
  scale_color_manual(values = c("motel" = "blue", "competitor" = "red")) +
  theme_minimal()


model <- lm(motel_pct ~ comp_pct, data = motel)
summary(model)


confint(model, level = 0.95)





#(b)
# 預測值和90%置信區間
new_data <- data.frame(comp_pct = 70)
predict(model, new_data, interval = "confidence", level = 0.90)





#(c)
# 重新執行回歸模型
model <- lm(motel_pct ~ comp_pct, data = motel)
summary(model)

# 計算 90% 置信區間，當 COMP_PCT = 70
new_data <- data.frame(comp_pct = 70)
predict(model, new_data, interval = "confidence", level = 0.90)

# 計算 t 值
t_value <- coef(summary(model))["comp_pct", "t value"]
t_value

# 計算 1% 顯著水準的臨界值
critical_t <- qt(0.99, df = model$df.residual)
critical_t

# 比較結果
if (t_value > critical_t) {
  print("拒絕 H0，COMP_PCT 顯著影響 MOTEL_PCT")
} else {
  print("無法拒絕 H0，COMP_PCT 無顯著影響")
}





#(d)
# 重新執行回歸
model <- lm(motel_pct ~ comp_pct, data = motel)
summary(model)

# 提取 β2 和標準誤
beta_2 <- coef(model)["comp_pct"]
se_beta_2 <- coef(summary(model))["comp_pct", "Std. Error"]

# 計算 t 統計量
t_value <- (beta_2 - 1) / se_beta_2
t_value

# 計算雙尾檢定的臨界值 (α = 0.01, 雙尾)
alpha <- 0.01
df <- model$df.residual  # 自由度
critical_t <- qt(1 - alpha / 2, df)  # 1 - α/2 取得雙尾區域
critical_t

# 檢查是否拒絕 H0
if (abs(t_value) > critical_t) {
  print("拒絕 H0，β2 顯著不同於 1")
} else {
  print("無法拒絕 H0，β2 可能等於 1")
}





#(e)
# 計算殘差
motel$residuals <- resid(model)

# 繪製殘差對 TIME 的圖
library(ggplot2)

ggplot(motel, aes(x = time, y = residuals)) +
  geom_line(color = "blue") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  labs(title = "Residuals vs Time",
       x = "Time",
       y = "Residuals") +
  theme_minimal()


# 過濾 17-23 時間區間的殘差
subset_residuals <- motel[motel$time >= 17 & motel$time <= 23, "residuals"]

# 計算該區間的殘差符號
signs <- ifelse(subset_residuals > 0, "Positive", "Negative")
table(signs)  # 計算正負數量
