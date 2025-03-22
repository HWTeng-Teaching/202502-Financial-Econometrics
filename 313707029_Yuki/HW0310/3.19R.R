# 清除環境變數（可選）
rm(list=ls())

library(POE5Rdata)
library(dplyr)
library(ggplot2)

data("motel")
motel <- motel  # 明確將資料集賦值給變數


# 繪製入住率隨時間變化的折線圖
ggplot(motel, aes(x = time)) +
  geom_line(aes(y = motel_pct, color = "Motel 入住率"), size = 1) +
  geom_line(aes(y = comp_pct, color = "競爭者入住率"), size = 1) +
  labs(title = "MOTEL_PCT 和 COMP_PCT 隨時間變化", 
       x = "時間", y = "入住率 (%)") +
  scale_color_manual(name = "變數", 
                     values = c("Motel 入住率" = "blue", "競爭者入住率" = "red")) +
  theme_minimal()

# 建立線性回歸模型
mod1 <- lm(motel_pct ~ comp_pct, data = motel)
summary(mod1)

# 計算 95% 信賴區間
alpha <- 0.05
b2 <- coef(mod1)[[2]]
df <- df.residual(mod1)
seb2 <- summary(mod1)$coefficients[2, 2]  # 取第 2 個變數的標準誤
tc <-qt(1-alpha/2,df)
lowb <- b2-tc*seb2
upb <-b2+tc*seb2
ci <-confint(mod1)
print(ci)
print(lowb_b2 <- ci[2,1])
print(upb_b2 <-ci[2,2])

# 進行預測
new_data <- data.frame(comp_pct = 70)
pred <- predict(mod1, new_data, interval = "confidence", level = 0.90)
print(pred)

# 計算 t 統計量
t_stat <- b2 / seb2
print(t_stat)

# 計算臨界值（單尾檢定）
alpha <- 0.01
t_critical <- qt(1 - alpha, df)

# 顯示結果
cat("t 統計量 =", t_stat, "\n")
cat("臨界值 =", t_critical, "\n")
cat("是否拒絕 H₀:", t_stat > t_critical, "\n")

# 計算 t 統計量
t_stat_2 <- (b2 - 1) / seb2

# 計算雙尾檢定的臨界值
t_critical_2 <- qt(1 - alpha/2, df)

# 顯示結果
cat("t 統計量 =", t_stat_2, "\n")
cat("臨界值 =", t_critical_2, "\n")
cat("是否拒絕 H₀:", abs(t_stat_2) > t_critical_2, "\n")

# 計算模型的殘差
residuals_mod1 <- resid(mod1)

# 繪製殘差圖
ggplot(motel, aes(x = time, y = residuals_mod1)) +  # 確保 TIME 的大小寫正確
  geom_line() +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +  # 繪製水平虛線
  geom_vline(xintercept = c(18, 23), linetype = "dashed", color = "blue") +  # 繪製兩條垂直線
  labs(title = "殘差與時間的關係", x = "時間", y = "殘差") +
  theme_minimal()

