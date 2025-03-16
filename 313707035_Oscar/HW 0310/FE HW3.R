# 載入數據集
library(POE5Rdata)
data("motel")

# 檢查數據結構
str(motel)
summary(motel)

(a)
# 繪製時間序列圖
plot(motel$time, motel$motel_pct, type="l", col="blue",
     ylim=range(motel$motel_pct, motel$comp_pct, na.rm=TRUE), 
     ylab="Occupancy Rate (%)", xlab="Time", main="MOTEL_PCT and COMP_PCT over Time")

lines(motel$time, motel$comp_pct, col="red")
legend("topright", legend=c("Motel Occupancy (motel_pct)", "Competitor Occupancy (comp_pct)"), 
       col=c("blue", "red"), lty=1)

# 估計回歸模型
model <- lm(motel_pct ~ comp_pct, data=motel)
summary(model)

# 提取斜率 β2 的 95% 信賴區間
confint(model, level=0.95)

(b)
# 預測 motel_pct 當 comp_pct = 70
new_data <- data.frame(comp_pct = 70)
pred <- predict(model, new_data, interval="confidence", level=0.90)

# 顯示預測值和信賴區間
print(pred)

(c)
# 提取 β2 和標準誤
beta2 <- coef(summary(model))["comp_pct", "Estimate"]
se_beta2 <- coef(summary(model))["comp_pct", "Std. Error"]

# 計算 t 值
t_value <- beta2 / se_beta2

# 計算 p 值
df <- nrow(motel) - 2
p_value <- pt(t_value, df, lower.tail=FALSE)

# 顯示結果
cat("t-value:", t_value, "\n")
cat("p-value:", p_value, "\n")

(d)
# 計算 t 統計量
t_value2 <- (beta2 - 1) / se_beta2

# 計算雙尾 p 值
p_value2 <- 2 * pt(-abs(t_value2), df)

# 顯示結果
cat("t-value:", t_value2, "\n")
cat("p-value:", p_value2, "\n")

(e)
# 計算殘差
residuals <- resid(model)

# 繪製殘差圖
plot(motel$time, residuals, type="b", col="blue", xlab="Time", ylab="Residuals", 
     main="Residuals of Regression Model")
abline(h=0, col="red", lty=2)

# 檢查 17-23 期的殘差
subset_residuals <- residuals[17:23]
print(subset_residuals)

