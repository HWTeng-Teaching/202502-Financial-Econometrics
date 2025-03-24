#載入資料
load("cex5_small.rdata")
ls()

# 計算 summary statistics
summary_stats <- function(x) {
  c(
    Mean = mean(x),
    Median = median(x),
    Min = min(x),
    Max = max(x),
    SD = sd(x)
  )
}

data <- cex5_small

# 計算 FOOD 與 INCOME 的統計
food_stats <- summary_stats(data$food)
income_stats <- summary_stats(data$income)

# 顯示統計表
summary_table <- rbind(Food = food_stats, Income = income_stats)
print(round(summary_table, 2))

# a
# 繪製直方圖（加上平均數與中位數）
par(mfrow = c(1, 2))  # 兩張圖並排

# FOOD
hist(data$food, main = "Histogram of FOOD", xlab = "FOOD", col = "skyblue", breaks = 20)
abline(v = mean(data$food), col = "red", lwd = 2, lty = 2)     # 平均數
abline(v = median(data$food), col = "darkgreen", lwd = 2, lty = 3)  # 中位數
legend("topright", legend = c("Mean", "Median"), col = c("red", "darkgreen"),
       lty = c(2,3), lwd = 2, bty = "n")

# INCOME
hist(data$income, main = "Histogram of INCOME", xlab = "INCOME", col = "orange", breaks = 20)
abline(v = mean(data$income), col = "red", lwd = 2, lty = 2)
abline(v = median(data$income), col = "darkgreen", lwd = 2, lty = 3)
legend("topright", legend = c("Mean", "Median"), col = c("red", "darkgreen"),
       lty = c(2,3), lwd = 2, bty = "n")

# Jarque-Bera test for normality
jb_food <- jarque.bera.test(data$food)
jb_income <- jarque.bera.test(data$income)

cat("\nJarque-Bera Test Results:\n")
cat("FOOD: p-value =", jb_food$p.value, "\n")
cat("INCOME: p-value =", jb_income$p.value, "\n")

# b
# 建立線性模型：FOOD ~ INCOME
model_linear <- lm(food ~ income, data = data)

# 顯示回歸結果摘要
summary(model_linear)

# 建立 95% 信賴區間（針對 beta₂，也就是 income 的係數）
confint(model_linear, level = 0.95)

# 繪製散佈圖 + 最小平方法回歸線
plot(data$income, data$food,
     main = "Scatter plot of FOOD vs INCOME",
     xlab = "INCOME", ylab = "FOOD",
     pch = 16, col = "steelblue")

# 加上回歸線
abline(model_linear, col = "red", lwd = 2)

# c 
# 殘差
res <- resid(model_linear)

# 殘差 vs INCOME 圖
plot(data$income, res,
     main = "Residuals vs INCOME",
     xlab = "INCOME", ylab = "Residuals",
     pch = 16, col = "purple")
abline(h = 0, col = "gray")

# 殘差直方圖
hist(res, breaks = 20, col = "lightblue",
     main = "Histogram of Residuals", xlab = "Residuals")

# Jarque-Bera 檢定（檢查殘差常態性）
if (!require(tseries)) install.packages("tseries")
library(tseries)
jarque.bera.test(res)

# 要計算的收入值
income_vals <- c(19, 65, 160)

# 擬合值（fitted FOOD）與 beta1
fitted_food <- predict(model_linear, newdata = data.frame(income = income_vals))
beta1 <- coef(model_linear)["income"]

# 彈性點估計
elasticity <- beta1 * income_vals / fitted_food

# 計算 beta1 的 95% 信賴區間
ci <- confint(model_linear, "income", level = 0.95)
beta1_lower <- ci[1]
beta1_upper <- ci[2]

# 彈性區間估計（固定 fitted 值）
elasticity_lower <- beta1_lower * income_vals / fitted_food
elasticity_upper <- beta1_upper * income_vals / fitted_food

# 建表格顯示結果
elasticity_table <- data.frame(
  INCOME = income_vals,
  Fitted_Food = round(fitted_food, 3),
  Elasticity = round(elasticity, 3),
  Lower_95 = round(elasticity_lower, 3),
  Upper_95 = round(elasticity_upper, 3)
)

print(elasticity_table)

# 加入對數變數
data$log_food <- log(data$food)
data$log_income <- log(data$income)

# 建立 log-log 模型
model_loglog <- lm(log_food ~ log_income, data = data)

# 顯示模型摘要
summary(model_loglog)

# 畫 log-log 散佈圖 + fitted line
plot(data$log_income, data$log_food,
     main = "log(FOOD) vs log(INCOME)",
     xlab = "log(INCOME)", ylab = "log(FOOD)",
     pch = 16, col = "darkgreen")
abline(model_loglog, col = "blue", lwd = 2)

# 提取 R² 值比較
r2_linear <- summary(model_linear)$r.squared
r2_loglog <- summary(model_loglog)$r.squared

cat("R² (Linear Model):", r2_linear, "\n")
cat("R² (Log-Log Model):", r2_loglog, "\n")

# 查看 log-log 模型的回歸摘要
summary(model_loglog)

# 建立 95% 信賴區間（對 log_income 的係數 gamma_2）
confint(model_loglog, "log_income", level = 0.95)

# 訓練 log-log 模型
# model_loglog <- lm(log(food) ~ log(income), data = data)

# 取出點估計與信賴區間
elasticity_point <- coef(model_loglog)["log_income"]
elasticity_ci <- confint(model_loglog, "log_income", level = 0.95)

# 設定要預測的 INCOME 值
income_vals <- c(19, 65, 160)

# 計算對應的 log(INCOME)
log_income_vals <- log(income_vals)

# 預測 log(FOOD)
log_food_pred <- predict(model_loglog, newdata = data.frame(log_income = log_income_vals))

# 還原成 FOOD（用 exp）
fitted_food <- exp(log_food_pred)

# 建立表格：彈性為常數，每列都一樣
elasticity_table <- data.frame(
  Income = income_vals,
  Fitted_Food = fitted_food,
  Elasticity = rep(elasticity_point, length(income_vals)),
  Lower_CI = rep(elasticity_ci[1], length(income_vals)),
  Upper_CI = rep(elasticity_ci[2], length(income_vals))
)

# 顯示結果
print(elasticity_table)

# 取出殘差與 log_income
residuals_loglog <- resid(model_loglog)
log_income_vals <- log(data$income)

# 殘差 vs ln(INCOME) 散佈圖
plot(log_income_vals, residuals_loglog,
     main = "Residuals vs log(INCOME)",
     xlab = "log(INCOME)", ylab = "Residuals",
     pch = 16, col = "purple")
abline(h = 0, col = "gray")

# 殘差直方圖
hist(residuals_loglog, breaks = 25, col = "lightblue",
     main = "Histogram of Residuals (Log-Log Model)",
     xlab = "Residuals")

# Jarque-Bera 常態性檢定
if (!require(tseries)) install.packages("tseries")
library(tseries)
jb_loglog <- jarque.bera.test(residuals_loglog)
print(jb_loglog)

# 確保 log_income 已經建立
data$log_income <- log(data$income)

# 建立線性-log 模型
model_linlog <- lm(food ~ log_income, data = data)

# 顯示模型摘要
summary(model_linlog)

# 畫散佈圖 + fitted 線
plot(data$log_income, data$food,
     main = "FOOD vs log(INCOME)",
     xlab = "log(INCOME)", ylab = "FOOD",
     pch = 16, col = "steelblue")
abline(model_linlog, col = "red", lwd = 2)

# 提取 R² 值比較
r2_linear <- summary(model_linear)$r.squared        # from (b)
r2_loglog <- summary(model_loglog)$r.squared        # from (e)
r2_linlog <- summary(model_linlog)$r.squared        # current

# 整理成表格
r2_table <- data.frame(
  Model = c("Linear", "Log-Log", "Linear-Log"),
  R2 = c(r2_linear, r2_loglog, r2_linlog)
)
print(r2_table)

# 需要預測的 INCOME 值
income_vals <- c(19, 65, 160)
log_income_vals <- log(income_vals)

# 使用 linear-log 模型建立預測的 FOOD
fitted_food <- predict(model_linlog, newdata = data.frame(log_income = log_income_vals))

# 係數估計與信賴區間
alpha2_hat <- coef(model_linlog)["log_income"]
alpha2_ci <- confint(model_linlog, "log_income", level = 0.95)

# 彈性點估計
elasticity <- alpha2_hat * income_vals / fitted_food

# 彈性上下限
elasticity_lower <- alpha2_ci[1] * income_vals / fitted_food
elasticity_upper <- alpha2_ci[2] * income_vals / fitted_food

# 建表格
elasticity_table_linlog <- data.frame(
  INCOME = income_vals,
  Fitted_Food = round(fitted_food, 3),
  Elasticity = round(elasticity, 3),
  Lower_95 = round(elasticity_lower, 3),
  Upper_95 = round(elasticity_upper, 3)
)

print(elasticity_table_linlog)

# 殘差
res_linlog <- resid(model_linlog)

# log(INCOME)（橫軸用）
log_income_vals <- log(data$income)

# 殘差 vs log(INCOME) 散佈圖
plot(log_income_vals, res_linlog,
     main = "Residuals vs log(INCOME) (Linear-Log Model)",
     xlab = "log(INCOME)", ylab = "Residuals",
     pch = 16, col = "darkorange")
abline(h = 0, col = "gray")

# 殘差直方圖
hist(res_linlog, breaks = 25, col = "lightgreen",
     main = "Histogram of Residuals (Linear-Log Model)",
     xlab = "Residuals")

# Jarque-Bera 檢定
if (!require(tseries)) install.packages("tseries")
library(tseries)
jb_linlog <- jarque.bera.test(res_linlog)
print(jb_linlog)

# 殘差對 log(INCOME) 圖（使用 linear-log 模型）
plot(log(data$income), resid(model_linlog),
     main = "Residuals vs Income",
     xlab = "log(income) ($)", ylab = "Residuals",
     pch = 16, col = "darkgreen")

# 加上 y=0 參考線
abline(h = 0, col = "red", lty = 2, lwd = 2)

