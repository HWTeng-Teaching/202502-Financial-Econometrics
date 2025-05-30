
# install package and Read dataset
install.packages("remotes")  # 確保 remotes 套件已安裝
remotes::install_github("ccolonescu/POE5Rdata")  # 需確認 GitHub Repo 位置
library(POE5Rdata)
data(motel)
data(comp)
motel_data <- motel
#comp_data <- comp

head(motel_data)  # 顯示前幾筆資料
str(motel_data)   # 檢查變數名稱與型態
colnames(motel_data)  # 列出所有變數名稱


# 載入必要的套件
library(ggplot2)

# 繪製趨勢圖
ggplot(motel_data, aes(x = time)) +  # time 是小寫的
  geom_line(aes(y = motel_pct, color = "Motel Occupancy")) +  # 使用 motel_pct
  geom_line(aes(y = comp_pct, color = "Competitor Occupancy")) +  # 使用 comp_pct
  labs(title = "Occupancy Rates Over Time", x = "Time", y = "Occupancy Rate (%)") +
  scale_color_manual(values = c("Motel Occupancy" = "blue", "Competitor Occupancy" = "red")) +
  theme_minimal()

# 建立回歸模型
model <- lm(motel_pct ~ comp_pct, data = motel_data)

# 顯示回歸結果
summary(model)

# 計算 90% 信賴區間
pred <- predict(model, newdata = data.frame(comp_pct = 70), interval = "confidence", level = 0.90)

# 顯示結果
pred

#C03Q19c.

# 提取斜率（β2）和標準誤
beta_2 <- coef(summary(model))["comp_pct", "Estimate"]
se_beta_2 <- coef(summary(model))["comp_pct", "Std. Error"]

# 計算 t-統計量
t_value <- beta_2 / se_beta_2

# 計算單尾檢定的 p 值（H0: β2 ≤ 0, HA: β2 > 0）
p_value <- pt(t_value, df = nrow(motel_data) - 2, lower.tail = FALSE)

# 顯示結果
cat("t-statistic:", t_value, "\n")
cat("p-value:", p_value, "\n")

# 設定 α = 0.01，檢查是否拒絕 H0
alpha <- 0.01
critical_value <- qt(1 - alpha, df = nrow(motel_data) - 2)
cat("Critical value (t_0.01):", critical_value, "\n")

# 判斷是否拒絕 H0
if (t_value > critical_value) {
  cat("結論: 拒絕 H0，β2 顯著大於 0。\n")
} else {
  cat("結論: 無法拒絕 H0，β2 不顯著大於 0。\n")
}

#C03Q19 d.

# 設定虛無假設 H0: β2 = 1
beta_2_null <- 1

# 提取斜率（β2）和標準誤
beta_2 <- coef(summary(model))["comp_pct", "Estimate"]
se_beta_2 <- coef(summary(model))["comp_pct", "Std. Error"]

# 計算 t-統計量
t_value <- (beta_2 - beta_2_null) / se_beta_2

# 計算雙尾檢定的 p 值（H0: β2 = 1, HA: β2 ≠ 1）
p_value <- 2 * pt(-abs(t_value), df = nrow(motel_data) - 2)

# 顯示結果
cat("t-statistic:", t_value, "\n")
cat("p-value:", p_value, "\n")

# 設定 α = 0.01，計算臨界值
alpha <- 0.01
critical_value <- qt(1 - alpha / 2, df = nrow(motel_data) - 2)
cat("Critical value (t_0.005):", critical_value, "\n")

# 判斷是否拒絕 H0
if (abs(t_value) > critical_value) {
  cat("結論: 拒絕 H0，β2 顯著不同於 1。\n")
} else {
  cat("結論: 無法拒絕 H0，β2 不顯著不同於 1。\n")
}


#C03Q19 e.

# 計算殘差
motel_data$residuals <- residuals(model)

# 繪製殘差對 TIME 的圖
library(ggplot2)
ggplot(motel_data, aes(x = time, y = residuals)) +
  geom_line(color = "blue") +
  geom_point(color = "red") +
  labs(title = "Residuals vs. Time",
       x = "Time",
       y = "Residuals") +
  theme_minimal()

# 篩選 17-23 時間區間的殘差
subset_residuals <- motel_data$residuals[motel_data$time >= 17 & motel_data$time <= 23]

# 計算平均值，判斷殘差的主要符號
mean_residual <- mean(subset_residuals)
cat("Mean residuals for time 17-23:", mean_residual, "\n")

# 判斷殘差的主要符號
if (mean_residual > 0) {
  cat("結論: 這段時間的殘差主要為 **正值**，表示模型低估了 MOTEL_PCT。\n")
} else {
  cat("結論: 這段時間的殘差主要為 **負值**，表示模型高估了 MOTEL_PCT。\n")
}


