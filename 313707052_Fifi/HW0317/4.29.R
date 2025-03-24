library(POE5Rdata)
data("cex5_small", package = "POE5Rdata")

names(cex5_small)

#a.

# Summary statistics
summary(cex5_small$food)
summary(cex5_small$income)

sd(cex5_small$food, na.rm = TRUE)
sd(cex5_small$income, na.rm = TRUE)

#直方圖

hist(cex5_small$food,
     main = "Food expenditure histogram",  # 圖標題
     xlab = "Food expenditure ($)",        # X 軸標籤
     col = "khaki",                        # 長條顏色
     breaks = 35)                          # 長條數量（越多越細）
abline(v = mean(cex5_small$food, na.rm = TRUE), 
       col = "red", lwd = 2, lty = 2)
abline(v = median(cex5_small$food, na.rm = TRUE), 
       col = "blue", lwd = 2, lty = 2)

hist(cex5_small$income,
     main = "Income histogram",
     xlab = "household monthly income during past year, $100 units",
     col = "khaki",
     breaks = 50,
     freq = FALSE)

abline(v = mean(cex5_small$income, na.rm = TRUE), 
       col = "red", lwd = 2, lty = 2)

abline(v = median(cex5_small$income, na.rm = TRUE), 
       col = "blue", lwd = 2, lty = 2)

#檢定是否常態分配

install.packages("tseries")  
library(tseries)  
jarque.bera.test(cex5_small$food)
jarque.bera.test(cex5_small$income)

#b.

#兩變數關係圖

plot(cex5_small$income, cex5_small$food,
     main = "Income vs Food Expenditure",
     xlab = "Household Monthly Income ($100 units)",
     ylab = "Monthly Food Expenditure ($)",
     pch = 20,
     col = "gray")

# 線性回歸
model <- lm(food ~ income, data = cex5_small)
summary(model)
abline(model, col = "blue", lwd = 2)

#  95% 信賴區間
# 預測值 + 區間
newx <- data.frame(income = seq(min(cex5_small$income, na.rm = TRUE),
                                max(cex5_small$income, na.rm = TRUE),
                                length.out = 100))

pred <- predict(model, newdata = newx, interval = "confidence")
lines(newx$income, pred[, "lwr"], col = "blue", lty = 2)
lines(newx$income, pred[, "upr"], col = "blue", lty = 2)

confint(model, "income", level = 0.95)

#c.

# 取出殘差
res <- residuals(model)

# 畫殘差 vs income
plot(cex5_small$income, res,
     main = "Residuals vs Income",
     xlab = "Income",
     ylab = "Residuals",
     pch = 20,
     col = "gray")

abline(h = 0, col = "red", lty = 2)

# 畫殘差 histogram
hist(res,
     main = "Histogram of Residuals",
     xlab = "Residuals",
     col = "khaki",
     breaks = 40)

# Jarque-Bera test for residuals
library(tseries)
jarque.bera.test(res)

#d.

# 回歸模型
model <- lm(food ~ income, data = cex5_small)

# 回歸係數
b2 <- coef(model)["income"]
summary(model)  # 如果你要看到 beta2 的 95% CI

# 指定的 income 水準
income_vals <- c(19, 65, 160)

# 用模型預測對應的 food 值
predicted_food <- predict(model, newdata = data.frame(income = income_vals))

# 計算彈性
elasticity <- b2 * income_vals / predicted_food

# 計算 95% 信賴區間
# 用 delta method 簡單估算上下限
se_b2 <- summary(model)$coefficients["income", "Std. Error"]
b2_ci <- b2 + c(-1.96, 1.96) * se_b2

# 對每個 income 點計算上下限的彈性區間
elasticity_ci <- t(sapply(1:3, function(i) {
  b2_ci * income_vals[i] / predicted_food[i]
}))

# 整理成表格顯示
result <- data.frame(
  INCOME = income_vals,
  Predicted_FOOD = round(predicted_food, 2),
  Elasticity = round(elasticity, 4),
  CI_Lower = round(elasticity_ci[,1], 4),
  CI_Upper = round(elasticity_ci[,2], 4)
)

print(result)

#e.

# 加入 log 轉換欄位
cex5_small$log_food <- log(cex5_small$food)
cex5_small$log_income <- log(cex5_small$income)

# 建立 log-log 模型
log_model <- lm(log_food ~ log_income, data = cex5_small)

# 查看係數
summary(log_model)

# 畫圖：散佈圖 + 回歸線
plot(cex5_small$log_income, cex5_small$log_food,
     main = "Log(Food) vs Log(Income)",
     xlab = "log(Income)",
     ylab = "log(Food)",
     pch = 20,
     col = "gray")

abline(log_model, col = "blue", lwd = 2)

#f.

# log-log 回歸模型
model_loglog <- lm(log(food) ~ log(income), data = cex5_small)

# 取得係數與信賴區間
summary(model_loglog)$coefficients
confint(model_loglog, level = 0.95)

# 指定的 income 水準
income_vals <- c(19, 65, 160)

# 用模型預測對應的 food 值
predicted_food <- predict(model_loglog, newdata = data.frame(income = income_vals))

# 抓出 γ₂ 的估計值與標準誤
gamma2 <- coef(model_loglog)["log(income)"]
se_gamma2 <- summary(model_loglog)$coefficients["log(income)", "Std. Error"]

# 算出信賴區間
ci <- gamma2 + c(-1.96, 1.96) * se_gamma2

# 整理出來
cat("Estimated elasticity:", round(gamma2, 4), "\n")
cat("95% confidence interval: [", round(ci[1], 4), ",", round(ci[2], 4), "]\n")

#g.

# 訓練 log-log 模型
model_loglog <- lm(log(food) ~ log(income), data = cex5_small)

# 殘差與 ln(INCOME)
resid_loglog <- residuals(model_loglog)
log_income <- log(cex5_small$income)

# 殘差 vs ln(INCOME)
plot(log_income, resid_loglog,
     main = "Residuals vs log(INCOME)",
     xlab = "log(INCOME)",
     ylab = "Residuals",
     pch = 20,
     col = "gray")
abline(h = 0, col = "red", lty = 2)

# 殘差直方圖
hist(resid_loglog,
     main = "Histogram of Residuals (log-log model)",
     xlab = "Residuals",
     col = "khaki",
     breaks = 40)

# Jarque-Bera 常態檢定
library(tseries)
jarque.bera.test(resid_loglog)

#h.

# 建立 linear-log 模型
model_linlog <- lm(food ~ log(income), data = cex5_small)

# 散佈圖：food vs log(income)
plot(log(cex5_small$income), cex5_small$food,
     main = "Food vs log(Income)",
     xlab = "log(Income)",
     ylab = "Food Expenditure",
     pch = 20,
     col = "gray")

# 回歸線
abline(model_linlog, col = "blue", lwd = 2)

# R-squared 值
summary(model_linlog)

#i.

# 建立 linear-log 模型
model_linlog <- lm(food ~ log(income), data = cex5_small)

# 抓出係數與標準誤
a2 <- coef(model_linlog)["log(income)"]
se_a2 <- summary(model_linlog)$coefficients["log(income)", "Std. Error"]

# income 值
income_vals <- c(19, 65, 160)

# 預測 food 值
predicted_food <- predict(model_linlog, newdata = data.frame(income = income_vals))

# 正確彈性公式：a2 / food * income
elasticity <- a2 / predicted_food 

# 信賴區間上下限 for a2
ci_a2 <- a2 + c(-1.96, 1.96) * se_a2

# 對每個點套入上下限計算彈性範圍
elasticity_ci <- t(sapply(1:3, function(i) {
  ci_a2 / predicted_food[i] 
}))

# 整理成表格
result <- data.frame(
  INCOME = income_vals,
  Predicted_FOOD = round(predicted_food, 2),
  Elasticity = round(elasticity, 4),
  CI_Lower = round(elasticity_ci[,1], 4),
  CI_Upper = round(elasticity_ci[,2], 4)
)

print(result)

#j.

# 建立 linear-log 模型
model_linlog <- lm(food ~ log(income), data = cex5_small)

# 取出殘差與 log(income)
resid_linlog <- residuals(model_linlog)
log_income <- log(cex5_small$income)

# 殘差 vs log(INCOME)
plot(log_income, resid_linlog,
     main = "Residuals vs log(INCOME)",
     xlab = "log(INCOME)",
     ylab = "Residuals",
     pch = 20,
     col = "gray")
abline(h = 0, col = "red", lty = 2)

# 殘差直方圖
hist(resid_linlog,
     main = "Histogram of Residuals (linear-log model)",
     xlab = "Residuals",
     col = "khaki",
     breaks = 40)

# Jarque–Bera 檢定
library(tseries)
jarque.bera.test(resid_linlog)

