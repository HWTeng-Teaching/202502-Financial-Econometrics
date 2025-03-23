install.packages("moments")  # 用於 Jarque-Bera 檢定
library(moments)
library(dplyr)
library(POE5Rdata)
data("cex5_small")
#(a)r計算統計量
stats <- sapply(cex5_small[, c("food", "income")], function(x) c(
  Mean = mean(x, na.rm = TRUE),
  Median = median(x, na.rm = TRUE),
  Min = min(x, na.rm = TRUE),
  Max = max(x, na.rm = TRUE),
  SD = sd(x, na.rm = TRUE)
))
print(stats)
par(mfrow = c(1, 2))  # 讓 food 和 income 直方圖並排顯示
# 繪製 food 直方圖
hist(cex5_small$food, 
     breaks = 30, 
     col = "lightblue", 
     main = "Histogram of Food Expenditure",
     xlab = "Food Expenditure", 
     probability = TRUE)
# 加入均值和中位數標示
abline(v = mean(cex5_small$food, na.rm = TRUE), col = "red", lwd = 2, lty = 2)  # 均值
abline(v = median(cex5_small$food, na.rm = TRUE), col = "blue", lwd = 2, lty = 2)  # 中位數
legend("topright", legend = c("Mean", "Median"), col = c("red", "blue"), lwd = 2, lty = 2)
# 繪製 income 直方圖
hist(cex5_small$income, 
     breaks = 30, 
     col = "lightgreen", 
     main = "Histogram of Income",
     xlab = "Household Income", 
     probability = TRUE)
# 加入均值和中位數標示
abline(v = mean(cex5_small$income, na.rm = TRUE), col = "red", lwd = 2, lty = 2)  # 均值
abline(v = median(cex5_small$income, na.rm = TRUE), col = "blue", lwd = 2, lty = 2)  # 中位數
legend("topright", legend = c("Mean", "Median"), col = c("red", "blue"), lwd = 2, lty = 2)
# Jarque–Bera 檢定
jb_food <- jarque.test(cex5_small$food)
print(jb_food)

jb_income <- jarque.test(cex5_small$income)
print(jb_income)

# (b) linear model
lm_model <- lm(food ~ income, data = cex5_small)
summary(lm_model)
plot(cex5_small$income, cex5_small$food, 
     main = "Scatterplot of Food vs. Income", 
     xlab = "Household Income", 
     ylab = "Food Expenditure", 
     col = "blue", pch = 16)
abline(lm_model, col = "red", lwd = 2)
confint(lm_model, level = 0.95)
# (c)檢測linear Model殘差是否為常態分配
residuals_lm <- resid(lm_model)
plot(cex5_small$income, residuals_lm, 
     main = "Residuals vs. Income", 
     xlab = "Household Income", 
     ylab = "Residuals", 
     col = "blue", pch = 16)
abline(h = 0, col = "red", lwd = 2, lty = 2)  # 加入 y=0 的紅色虛線
hist(residuals_lm, 
     breaks = 30, 
     col = "lightblue", 
     main = "Histogram of Residuals", 
     xlab = "Residuals", 
     probability = TRUE)
# Jarque–Bera 正態性檢定
jarque.test(residuals_lm)
# (d)檢定特定值估計效率與信賴區間
income_values <- c(19, 65, 160)

beta1 <- coef(lm_model)[1]  # 截距項
beta2 <- coef(lm_model)[2]  # income 的影響

food_hat <- beta1 + beta2 * income_values

elasticity <- beta2 * (income_values / food_hat)

se_beta2 <- summary(lm_model)$coefficients["income", "Std. Error"]

se_elasticity <- se_beta2 * (income_values / food_hat)

lower_bound <- elasticity - 1.96 * se_elasticity
upper_bound <- elasticity + 1.96 * se_elasticity

result <- data.frame(income_values, food_hat, elasticity, lower_bound, upper_bound)
print(result)

#(e) log-log model
cex5_small$log_food <- log(cex5_small$food)
cex5_small$log_income <- log(cex5_small$income)

log_log_model <- lm(log_food ~ log_income, data = cex5_small)
summary(log_log_model)
# 繪製曲線
beta1_log <- coef(log_log_model)[1] 
beta2_log <- coef(log_log_model)[2]  
income_seq <- seq(min(cex5_small$income), max(cex5_small$income), length.out = 100)
food_pred <- exp(beta1_log) * income_seq^beta2_log  # 轉換回原始尺度
plot(cex5_small$income, cex5_small$food, 
     main = "Log-Log Model in Original Scale", 
     xlab = "Income", 
     ylab = "Food Expenditure", 
     col = "blue", pch = 16)
lines(income_seq, food_pred, col = "red", lwd = 2)

r2_log_log <- summary(log_log_model)$r.squared
r2_linear <- summary(lm_model)$r.squared  # 從原線性模型取得 R²

n <- nrow(cex5_small)
generalized_r2_log_log <- 1 - exp((log(1 - r2_log_log) * n) / (n - 1))

cat("Linear Model R²: ", r2_linear, "\n")
cat("Log-Log Model R²: ", r2_log_log, "\n")
cat("Generalized R² for Log-Log Model: ", generalized_r2_log_log, "\n")

# (f) log-log Model彈性效果
beta2_hat <- coef(log_log_model)[2]  
se_beta2 <- summary(log_log_model)$coefficients[2, 2]

CI_lower <- beta2_hat - 1.96 * se_beta2
CI_upper <- beta2_hat + 1.96 * se_beta2


cat("Point Estimate of Elasticity: ", beta2_hat, "\n")
cat("95% Confidence Interval: (", CI_lower, ",", CI_upper, ")\n")

# (g) log-log的殘差
residuals_loglog <- resid(log_log_model)  # 取出 log-log 模型的殘差
ln_income <- log(cex5_small$income)  # 轉換 income 為對數

plot(ln_income, residuals_loglog,
     main = "Residuals vs. ln(INCOME)",
     xlab = "ln(INCOME)",
     ylab = "Residuals",
     col = "blue", pch = 16)
abline(h = 0, col = "red", lwd = 2)  # 添加 y=0 的參考線

hist(residuals_loglog, 
     main = "Histogram of Residuals",
     xlab = "Residuals",
     col = "lightblue", border = "black", probability = TRUE)
lines(density(residuals_loglog), col = "red", lwd = 2)  # 添加密度曲線

jb_test <- jarque.test(residuals_loglog)

cat("Jarque–Bera Test Statistic: ", jb_test$statistic, "\n")
cat("p-value: ", jb_test$p.value, "\n")

if (jb_test$p.value < 0.05) {
  cat("Conclusion: Residuals are NOT normally distributed.\n")
} else {
  cat("Conclusion: Residuals are normally distributed.\n")
}

# (h) linear-log
linear_log_model <- lm(food ~ log(income), data = cex5_small)

summary(linear_log_model)

plot(log(cex5_small$income), cex5_small$food,
     main = "FOOD vs. ln(INCOME)",
     xlab = "ln(INCOME)", ylab = "FOOD",
     col = "blue", pch = 16)
abline(linear_log_model, col = "red", lwd = 2)  # 添加回歸線

r2_linear_log <- summary(linear_log_model)$r.squared
r2_linear <- summary(lm_model)$r.squared  # 來自(b)的線性模型
r2_log_log <- summary(log_log_model)$r.squared  # 來自(e)的log-log模型

cat("R-squared for Linear Model: ", r2_linear, "\n")
cat("R-squared for Log-Log Model: ", r2_log_log, "\n")
cat("R-squared for Linear-Log Model: ", r2_linear_log, "\n")

# (i) linear-log之彈性
beta2 <- coef(summary(linear_log_model))["log(income)", "Estimate"]
se_beta2 <- coef(summary(linear_log_model))["log(income)", "Std. Error"]

income_values <- c(19, 65, 160)

elasticities <- beta2 / income_values  # 計算彈性
lower_bounds <- elasticities - 1.96 * (se_beta2 / income_values)  # 計算下界
upper_bounds <- elasticities + 1.96 * (se_beta2 / income_values)  # 計算上界

elasticity_table <- data.frame(
  Income = income_values,
  Elasticity_Estimate = elasticities,
  Lower_95_CI = lower_bounds,
  Upper_95_CI = upper_bounds
)
print(elasticity_table)

# (j) linear-log之殘差檢定
residuals_linear_log <- residuals(linear_log_model)

plot(log(cex5_small$income), residuals_linear_log,
     main = "Residuals vs. ln(INCOME)",
     xlab = "ln(INCOME)", ylab = "Residuals",
     col = "blue", pch = 16)
abline(h = 0, col = "red", lwd = 2)  # 添加基準線
hist(residuals_linear_log, breaks = 20, probability = TRUE,
     main = "Histogram of Residuals",
     xlab = "Residuals", col = "lightblue", border = "black")

jb_test <- jarque.test(residuals_linear_log)
print(jb_test)