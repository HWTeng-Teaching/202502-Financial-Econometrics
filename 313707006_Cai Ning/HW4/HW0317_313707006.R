#4.4(a)

EXPER <- 0:30
Mode1 <- 64.289 + 0.990 * EXPER
plot(EXPER, Model1, type = "l", col = "blue",
     xlab = "EXPER", ylab = "RATING", 
     main = "Model 1")

#4.4(b)

EXPER <- 1:30
Mode2 <- 39.464 + 15.312 * log(EXPER)
plot(EXPER, Model2, type = "l", col = "blue",
     xlab = "EXPER", ylab = "RATING", 
     main = "Model 2")

#4.28(a)

library(POE5Rdata)
data("wa_wheat")
YIELD <- wa_wheat$northampton
TIME <- wa_wheat$time

#(i)
model1 <- lm(YIELD ~ TIME)
model2 <- lm(YIELD ~ log(TIME))
model3 <- lm(YIELD ~ I(TIME^2))
model4 <- lm(log(YIELD) ~ TIME)

par(mfrow = c(2, 2), mar = c(4, 4, 2, 1)) # 四張圖呈現再一起
y_range <- range(YIELD)  # 確保所有圖的 Y 軸範圍相同
plot(TIME, YIELD, main = "Linear Model", xlab = "TIME", ylab = "YIELD", ylim = y_range)
lines(TIME[order(TIME)], fitted(model1)[order(TIME)], col = "red")
plot(TIME, YIELD, main = "Linear-log Model", xlab = "TIME", ylab = "YIELD", ylim = y_range)
lines(TIME[order(TIME)], fitted(model2)[order(TIME)], col = "red")
plot(TIME, YIELD, main = "Quadratic Model", xlab = "TIME", ylab = "YIELD", ylim = y_range)
lines(TIME[order(TIME)], fitted(model3)[order(TIME)], col = "red")
plot(TIME, YIELD, main = "Log-linear Model", xlab = "TIME", ylab = "YIELD", ylim = y_range)
lines(TIME[order(TIME)], exp(fitted(model4))[order(TIME)], col = "red")  # 將回歸線還原成 YIELD

#(ii)
par(mfrow = c(2, 2), mar = c(4, 4, 2, 1))
plot(TIME, resid(model1), main = "Linear Model Residuals", xlab = "TIME", ylab = "Residuals")
abline(h = 0, col = "red", lty = 2)  
plot(TIME, resid(model2), main = "Linear-log Model Residuals", xlab = "TIME", ylab = "Residuals")
abline(h = 0, col = "red", lty = 2)
plot(TIME, resid(model3), main = "Quadratic Model Residuals", xlab = "TIME", ylab = "Residuals")
abline(h = 0, col = "red", lty = 2)
plot(TIME, resid(model4), main = "Log-linear Model Residuals", xlab = "TIME", ylab = "Residuals")
abline(h = 0, col = "red", lty = 2)

#(iii)
install.packages("tseries")  
library(tseries)

res1 <- resid(model1)
res2 <- resid(model2)
res3 <- resid(model3)
res4 <- resid(model4)

# 針對每個模型進行 Jarque-Bera 檢定
jb_test1 <- jarque.bera.test(res1)
jb_test2 <- jarque.bera.test(res2)
jb_test3 <- jarque.bera.test(res3)
jb_test4 <- jarque.bera.test(res4)

print(jb_test1)
print(jb_test2)
print(jb_test3)
print(jb_test4)

r2_1 <- summary(model1)$r.squared
r2_2 <- summary(model2)$r.squared
r2_3 <- summary(model3)$r.squared
r2_4 <- summary(model4)$r.squared

print(r2_1)
print(r2_2)
print(r2_3)
print(r2_4)

#4.28(b)

summary(model3)

#4.28(c)

# 計算診斷統計量
studentized_residuals <- rstudent(model3)
leverage <- hatvalues(model3)
dfbetas_values <- dfbetas(model3)
dffits_values <- dffits(model3)

# 取得樣本數與變數數
n <- length(studentized_residuals)
p <- length(coef(model3)) - 1  # 變數數量（不含截距）

# 設定閾值
leverage_threshold <- 2 * (p + 1) / n
dffits_threshold <- 2 * sqrt((p + 1) / n)
DFBETAS_threshold <- 2/sqrt(n)


par(mfrow = c(2, 2), mar = c(4, 4, 2, 1))
plot(1:n, studentized_residuals, xlab = 'Observations', main = 'Studentized Residuals')
abline(h = c(-2, 2), col = "red")
plot(1:n, leverage, xlab = 'Observations', main = 'LEVERAGE')
abline(h = leverage_threshold, col = "red")
plot(1:n, dfbetas_values[,2], xlab = 'Observations', main = 'DFBETAS')
abline(h = DFBETAS_threshold, col = "red", lwd = 2)
plot(1:n, dffits_values, xlab = 'Observations', main = "DFFITS")
abline(h = c(-dffits_threshold, dffits_threshold), col = "red", lwd = 2)

results <- data.frame(
  n = 1:n,
  studentized_residuals = studentized_residuals,
  leverage = leverage,
  dfbetas_TIME2 = dfbetas_values[,2],  # 取 TIME^2 變數的 DFBETA
  dffits = dffits_values
)

# 篩選異常觀測值
outliers <- results[
  abs(results$studentized_residuals) > 2 | 
    results$leverage > leverage_threshold | 
    abs(results$dfbetas_TIME2) > DFBETAS_threshold | 
    abs(results$dffits) > dffits_threshold, 
]

print(outliers)
print(leverage_threshold)
print(dffits_threshold)
print(DFBETAS_threshold)

#4.28(d)

data_1996 <- subset(wa_wheat, time <= 47)
model_1996 <- lm(YIELD ~ I(TIME^2), data = data_1996)
new_data <- data.frame(TIME = 48)
predicted_1997 <- predict(model_1996, newdata = new_data, interval = "prediction", level = 0.95)
print(predicted_1997)

true_1997 <- wa_wheat[48, "northampton"] 
print(true_1997)

#4.29 (a)

library(POE5Rdata)
data('cex5_small')

summary_stats <- data.frame(
  變數 = c("food", "income"),
  n = c(length(cex5_small$food),length(cex5_small$income)),
  mean = c(mean(cex5_small$food, na.rm = TRUE), mean(cex5_small$income, na.rm = TRUE)),
  median = c(median(cex5_small$food, na.rm = TRUE), median(cex5_small$income, na.rm = TRUE)),
  min = c(min(cex5_small$food, na.rm = TRUE), min(cex5_small$income, na.rm = TRUE)),
  max = c(max(cex5_small$food, na.rm = TRUE), max(cex5_small$income, na.rm = TRUE)),
  sd = c(sd(cex5_small$food, na.rm = TRUE), sd(cex5_small$income, na.rm = TRUE))
)
print(summary_stats)

par(mfrow = c(1, 2))

hist(cex5_small$food, main = " Histogram of food", xlab = "", col = "lightblue", 
     breaks = 20)
abline(v = mean(cex5_small$food, na.rm = TRUE), col = "red" )  # 均值線
abline(v = median(cex5_small$food, na.rm = TRUE), col = "blue")  # 中位數線
legend("topright", legend = c("均值", "中位數"), col = c("red", "blue"), lty = 1)

hist(cex5_small$income, main = "Histogram of income", xlab = "", col = "lightblue", 
     breaks = 20)
abline(v = mean(cex5_small$income, na.rm = TRUE), col = "red")  # 均值線
abline(v = median(cex5_small$income, na.rm = TRUE), col = "blue")  # 中位數線
legend("topright", legend = c("均值", "中位數"), col = c("red", "blue"), lty = 1)

library(tseries)
jb_test_food <- jarque.bera.test(cex5_small$food)
jb_test_income <- jarque.bera.test(cex5_small$income)
print(jb_test_food)
print(jb_test_income)

#4.29(b)

lm_model <- lm(food ~ income, data = cex5_small)
summary(lm_model)

plot(cex5_small$income, cex5_small$food, main = "Scatter Plot of FOOD vs INCOME", 
     xlab = "Income", ylab = "Food", col = "blue", pch = 20)
abline(lm_model, col = "red", lwd = 2)

confint(lm_model, "income", level = 0.95)

#4.29(c)

residuals <- resid(lm_model)

par(mfrow = c(1, 2))
plot(cex5_small$income, residuals, main = "Residuals vs Income", xlab = "Income", ylab = "Residuals", col = "blue", pch = 20)
hist(residuals, main = "Histogram of Residuals", xlab = "Residuals", col = "lightblue", breaks = 20)

library(tseries)
jb_test_residuals <- jarque.bera.test(residuals)
print(jb_test_residuals)

#4.29(d)

income_levels <- c(19, 65, 160)
beta1_hat <- coef(lm_model)["income"]
food_hat <- predict(lm_model, newdata = data.frame(income = income_levels))
elasticities <- beta1_hat * (income_levels / food_hat)

conf_beta1 <- confint(lm_model, "income", level = 0.95)
beta1_lower <- conf_beta1[1]
beta1_upper <- conf_beta1[2]

elasticity_CI_lower <- beta1_lower * (income_levels / food_hat)
elasticity_CI_upper <- beta1_upper * (income_levels / food_hat)

elasticity_results <- data.frame(
  Income = income_levels,
  Food_Hat = food_hat,
  Elasticity_Estimate = elasticities,
  CI_Lower = elasticity_CI_lower,
  CI_Upper = elasticity_CI_upper
)

print(elasticity_results)

#4.29(e)

cex5_small$ln_food <- log(cex5_small$food)
cex5_small$ln_income <- log(cex5_small$income)
log_log_model <- lm(ln_food ~ ln_income, data = cex5_small)
summary(log_log_model)

plot(cex5_small$ln_income, cex5_small$ln_food, 
     main = "Scatter Plot of ln(FOOD) vs ln(INCOME)", 
     xlab = "ln(INCOME)", ylab = "ln(FOOD)")
abline(log_log_model, col = "red", lwd = 2)

summary(lm_model)$r.squared
summary(log_log_model)$r.squared

#4.29(f)

gamma2_hat <- coef(log_log_model)["ln_income"]
gamma2_CI <- confint(log_log_model, "ln_income", level = 0.95)
print(gamma2_hat)
print(gamma2_CI)

#4.29(g)

log_log_residuals <- residuals(log_log_model)

par(mfrow = c(1, 2))
plot(log(cex5_small$income), log_log_residuals, 
     main = "Residuals vs ln(INCOME)", 
     xlab = "ln(INCOME)", ylab = "Residuals",
     col = "blue", pch = 20)
abline(h = 0, col = "red", lwd = 2)  
hist(log_log_residuals, 
     main = "Histogram of Residuals", 
     xlab = "Residuals", 
     col = "lightblue", 
     breaks = 20)

library(tseries)
jb_test_log_log <- jarque.bera.test(log_log_residuals)
print(jb_test_log_log)

#4.19(h)

linear_log_model <- lm(food ~ ln_income, data = cex5_small)
summary(linear_log_model)

plot(cex5_small$ln_income, cex5_small$food, 
     main = "Scatter Plot of FOOD vs ln(INCOME)", 
     xlab = "ln(INCOME)", ylab = "FOOD", col = "blue", pch = 20)
abline(linear_log_model, col = "red", lwd = 2)

r2_linear <- summary(lm_model)$r.squared  
r2_log_log <- summary(log_log_model)$r.squared  
r2_linear_log <- summary(linear_log_model)$r.squared

r2_values <- data.frame(
  Model = c("Linear", "Log-Log", "Linear-Log"),
  R_Squared = c(r2_linear, r2_log_log, r2_linear_log)
)

print(r2_values)

#4.29(i)

alpha2_hat <- coef(linear_log_model)["ln_income"]
conf_alpha2 <- confint(linear_log_model, "ln_income", level = 0.95)

income_levels <- c(19, 65, 160)

food_hat <- predict(linear_log_model, newdata = data.frame(ln_income = log(income_levels)))

elasticities_linear_log <- alpha2_hat / food_hat

elasticity_linear_log_CI_lower <- conf_alpha2[1] / food_hat
elasticity_linear_log_CI_upper <- conf_alpha2[2] / food_hat

elasticity_linear_log_results <- data.frame(
  Income = income_levels,
  Food_Hat = food_hat,
  Elasticity_Estimate = elasticities,
  CI_Lower = elasticity_CI_lower,
  CI_Upper = elasticity_CI_upper
)

print(elasticity_linear_log_results)

#4.29(j)

linear_log_residuals <- residuals(linear_log_model)

par(mfrow = c(1, 2))  
plot(cex5_small$ln_income, linear_log_residuals, 
     main = "Residuals vs ln(INCOME)", 
     xlab = "ln(INCOME)", ylab = "Residuals", 
     col = "blue", pch = 20)
abline(h = 0, col = "red", lwd = 2)  

hist(linear_log_residuals, 
     main = "Histogram of Residuals", 
     xlab = "Residuals", col = "lightblue", breaks = 20)

jb_test_linear_log <- jarque.bera.test(linear_log_residuals)
print(jb_test_linear_log)

