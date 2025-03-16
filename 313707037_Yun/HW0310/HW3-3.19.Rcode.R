#a小題
# 讀取 .dat 檔案
url <- "https://www.principlesofeconometrics.com/poe5/data/ascii/motel.dat"
download.file(url, destfile = "motel.dat")
data <- read.table("motel.dat", header = FALSE)
colnames(data) <- c("TIME","DAYS", "MOTEL_RATE", "COMP_RATE", "MOTEL_PCT", "COMP_PCT", "REPAIR", "RELPRICE")
head(data)  # 查看前幾行
# 繪製時間序列圖
plot(data$TIME, data$MOTEL_PCT, type = "l", col = "lightblue", ylim = range(c(data$MOTEL_PCT, data$COMP_PCT)),
     xlab = "Time", ylab = "Occupancy Rate", main = "Motel vs Competitor Occupancy Rate")
lines(data$TIME, data$COMP_PCT, col = "red")
legend("topright", legend = c("Motel", "Competitor"), col = c("lightblue", "red"), lty = 1)

# 迴歸模型
model <- lm(MOTEL_PCT ~ COMP_PCT, data = data)
summary(model)

# 計算 95% 信賴區間
confint(model, level = 0.95)

#b小題 
# 預測值及其標準誤
new_data <- data.frame(COMP_PCT = 70)
predict(model, new_data, interval = "confidence", level = 0.90)

#c小題
# 提取迴歸係數與標準誤
beta2_hat <- coef(model)["COMP_PCT"]
se_beta2 <- summary(model)$coefficients["COMP_PCT", "Std. Error"]

# 計算 t 統計量
n <- 25
t_stat <- beta2_hat / se_beta2
t_critical <- qt(0.99, df = n - 2)  # 單尾檢定，α = 0.01

# 顯示結果
cat("t-statistic:", t_stat, "\nCritical t-value:", t_critical, "\n")

# 結論
if (t_stat > t_critical) {cat("Reject H0: There is sufficient evidence that β2 > 0 at 1% significance level.\n")
} else {cat("Fail to reject H0: No sufficient evidence that β2 > 0.\n")
}

#d小題
# 計算 t 統計量
n <- 25
t_stat <- (beta2_hat - 1) / se_beta2
t_critical <- qt(0.995, df = n - 2)  # 雙尾檢定，α = 0.01

# 顯示結果
cat("t-statistic:", t_stat, "\nCritical t-value:", t_critical, "\n")

# 結論
if (abs(t_stat) > t_critical) {
  cat("Reject H0: β2 is significantly different from 1 at 1% significance level.\n")
} else {
  cat("Fail to reject H0: No sufficient evidence that β2 ≠ 1.\n")
}

#e小題
# 計算殘差
data$residuals <- residuals(model)

# 繪製殘差對時間的圖
plot(data$TIME, data$residuals, type = "b", col = "purple",
     xlab = "Time", ylab = "Residuals", main = "Residuals vs Time")
abline(h = 0, lty = 2, col = "gray")

# 檢查 17-23 期間的殘差趨勢
subset(data, TIME >= 17 & TIME <= 23)