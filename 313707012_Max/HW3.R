library(POE5Rdata)
data = data("motel")

#a.
# Plot MOTEL_PCT and COMP_PCT vs TIME
plot(motel$time, motel$motel_pct, type = "l", col = "blue", ylim = c(0, 100),
     xlab = "Time (Months)", ylab = "Occupancy Rate (%)", 
     main = "MOTEL_PCT and COMP_PCT vs TIME")
lines(motel$time, motel$comp_pct, col = "red")
legend("bottomleft", legend = c("MOTEL_PCT", "COMP_PCT"), col = c("blue", "red"), lty = 1)
# linear regression model
model <- lm(motel$motel_pct ~ motel$comp_pct, data = motel)
summary(model)
print(model)
# 95% confidence interval for beta2
confint(model, "motel$comp_pct" , level = 0.95)


#b.
# Predict MOTEL_PCT with a 90% confidence interval for the mean
beta1 <- coef(model)[1]  # 截距
beta2 <- coef(model)[2]  # 斜率

# 計算預測值（COMP_PCT = 70）
comp_pct_new <- 70
motel_pct_pred <- beta1 + beta2 * comp_pct_new

n <- nrow(motel)  # 樣本量

x_mean <- mean(motel$comp_pct)  # COMP_PCT 的均值和離差平方和
x_ss <- sum((motel$comp_pct - x_mean)^2)
sigma <- summary(model)$sigma  # 模型的殘差標準誤
se_yhat <- sqrt(sigma^2 * (1/n + (comp_pct_new - x_mean)^2 / x_ss)) # 預測值的標準誤

ci_lower <- motel_pct_pred - qt(0.95, n - 2) * se_yhat
ci_upper <- motel_pct_pred + qt(0.95, n - 2) * se_yhat
cat(sprintf("90%% Confidence Interval: (%.2f, %.2f)\n", ci_lower, ci_upper))


#c.
model_summary <- summary(model)
# 提取 beta2 的估計值、標準誤和 t 值
beta2 <- coef(model_summary)[2, "Estimate"]
se_beta2 <- coef(model_summary)[2, "Std. Error"]
t_stat_c <- coef(model_summary)[2, "t value"]

n <- nrow(motel)
df <- n - 2

alpha <- 0.01
t_c <- qt(1 - alpha, df)  # 計算臨界值（右尾檢定，alpha = 0.01）

cat("Test Statistic (t):", t_stat_c, "\n")
cat("Critical Value (t at alpha = 0.01, df =", df, "):", t_c, "\n")

cat("Rejection Region: t >", t_c, "\n")  # 拒絕域

if (t_stat_c > t_c) {
  cat("Reject H0 at alpha = 0.01.\n")
  cat("This suggests a positive relationship between COMP_PCT and MOTEL_PCT.\n")
} else {
  cat("Fail to reject H0 at alpha = 0.01.\n")
}  # 檢定結論


#d.
alpha <- 0.01
t_d <- qt(1 - alpha/2, df)  # 雙尾檢定的臨界值
t_stat_d <- (beta2 - 1) / se_beta2

cat("Test Statistic (t):", t_stat_d, "\n")
cat("Critical Values (t_critical at alpha = 0.01, df =", df, "): ±", t_d, "\n")

cat("Rejection Region: t <", -t_d, "or t >", t_d, "\n")  # 拒絕域

if (abs(t_stat_d) > t_d) {
  cat("Reject H0 at alpha = 0.01.\n")
} else {
  cat("Fail to reject H0 at alpha = 0.01.\n")
}  # 檢定結論


#e.
residuals <- residuals(model)

# 繪製殘差對 TIME 的圖
plot(motel$time, residuals, type = "b", pch = 20, col = "blue",
     xlab = "Time (Months)", ylab = "Residuals",
     main = "Residuals of MOTEL_PCT vs TIME")
abline(h = 0, lty = 2, col = "red")  # 添加 y = 0 線作為參考

# 分析時間段 17–23 的殘差符號
# 假設 TIME 從 2003 年 1 月開始，17–23 對應 July 2004 至 January 2005
residuals_17_23 <- residuals[17:23]
signs <- sign(residuals_17_23)
predominant_sign <- ifelse(sum(signs > 0) > sum(signs < 0), "Positive", "Negative")

# 輸出時間段 17–23 的殘差符號分析
cat("\nResiduals for time periods 17–23 (July 2004 to January 2005):\n")
print(data.frame(Time = motel$time[17:23], Residuals = residuals_17_23, Sign = signs))
cat("Predominant sign of residuals during time periods 17–23:", predominant_sign, "\n")
