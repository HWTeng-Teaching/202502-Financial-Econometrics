#4.4
# a. 繪製模型 1 的擬合值（EXPER = 0 至 30）
exper <- seq(0, 30, by = 1)
rating_model1 <- 64.289 + 0.99 * exper

# 繪製圖表
plot(exper, rating_model1, type = "l", col = "blue", lwd = 2,
     xlab = "Experience (Years)", ylab = "Predicted RATING",
     main = "Model 1: RATING vs EXPER (0 to 30 Years)",
     ylim = c(60, 100))  
grid()  
points(exper[seq(1, length(exper), by = 10)], rating_model1[seq(1, length(exper), by = 10)], 
       pch = 19, col = "red")  # 標記 EXPER = 0, 10, 20, 30 的點
text(exper[seq(1, length(exper), by = 10)], rating_model1[seq(1, length(exper), by = 10)], 
     labels = round(rating_model1[seq(1, length(exper), by = 10)], 2), pos = 3, col = "black")  # 添加數值標籤

# b. 繪製模型 2 的擬合值（EXPER = 1 至 30）
exper <- seq(1, 30, by = 1)
rating_model2 <- 39.464 + 15.312 * log(exper)

# 繪製圖表
plot(exper, rating_model2, type = "l", col = "blue", lwd = 2,
     xlab = "Experience (Years)", ylab = "Predicted RATING",
     main = "Model 2: RATING vs EXPER (1 to 30 Years)",
     ylim = c(30, 100)) 
grid()  
points(exper[seq(1, length(exper), by = 10)], rating_model2[seq(1, length(exper), by = 10)], 
       pch = 19, col = "red")  # 標記 EXPER = 1, 11, 21 的點
text(exper[seq(1, length(exper), by = 10)], rating_model2[seq(1, length(exper), by = 10)], 
     labels = round(rating_model2[seq(1, length(exper), by = 10)], 2), pos = 3, col = "black")  # 添加數值標籤

#-------------------分割線-----------------------

#4.28
library(POE5Rdata)
data = data("wa_wheat")
# 估計四個模型
# 模型 1
model1 <- lm(wa_wheat$northampton ~ wa_wheat$time, data = wa_wheat)
# 模型 2
model2 <- lm(wa_wheat$northampton ~ log(wa_wheat$time), data = wa_wheat)
# 模型 3
model3 <- lm(wa_wheat$northampton ~ I(wa_wheat$time^2), data = wa_wheat)
# 模型 4
model4 <- lm(log(wa_wheat$northampton) ~ wa_wheat$time, data = wa_wheat)

# (i) 繪製擬合值圖
par(mfrow = c(2, 2))  
# 模型 1
plot(wa_wheat$time, wa_wheat$northampton, pch = 20, col = "black", xlab = "Time (1950-1997)", ylab = "YIELD", main = "Model 1: YIELD vs TIME")
lines(wa_wheat$time, fitted(model1), col = "blue", lwd = 2)
# 模型 2
plot(wa_wheat$time, wa_wheat$northampton, pch = 20, col = "black", xlab = "Time (1950-1997)", ylab = "YIELD", main = "Model 2: YIELD vs ln(TIME)")
lines(wa_wheat$time, fitted(model2), col = "blue", lwd = 2)
# 模型 3
plot(wa_wheat$time, wa_wheat$northampton, pch = 20, col = "black", xlab = "Time (1950-1997)", ylab = "YIELD", main = "Model 3: YIELD vs TIME^2")
lines(wa_wheat$time, fitted(model3), col = "blue", lwd = 2)
# 模型 4
plot(wa_wheat$time, wa_wheat$northampton, pch = 20, col = "black", xlab = "Time (1950-1997)", ylab = "YIELD", main = "Model 4: ln(YIELD) vs TIME")
lines(wa_wheat$time, exp(fitted(model4)), col = "blue", lwd = 2)  # 轉回 YIELD 尺度
par(mfrow = c(1, 1))

# (ii) 繪製殘差圖
par(mfrow = c(2, 2))
plot(wa_wheat$time, residuals(model1), type = "b", pch = 20, col = "blue", xlab = "Time", ylab = "Residuals", main = "Model 1 Residuals")
abline(h = 0, lty = 2, col = "red")
plot(wa_wheat$time, residuals(model2), type = "b", pch = 20, col = "blue", xlab = "Time", ylab = "Residuals", main = "Model 2 Residuals")
abline(h = 0, lty = 2, col = "red")
plot(wa_wheat$time, residuals(model3), type = "b", pch = 20, col = "blue", xlab = "Time", ylab = "Residuals", main = "Model 3 Residuals")
abline(h = 0, lty = 2, col = "red")
plot(wa_wheat$time, residuals(model4), type = "b", pch = 20, col = "blue", xlab = "Time", ylab = "Residuals", main = "Model 4 Residuals")
abline(h = 0, lty = 2, col = "red")
par(mfrow = c(1, 1))

# (iii) 誤差正態性檢定（Shapiro-Wilk 檢定）
cat("Shapiro-Wilk Normality Test for Residuals:\n")
cat("Model 1: p-value =", shapiro.test(residuals(model1))$p.value, "\n")
cat("Model 2: p-value =", shapiro.test(residuals(model2))$p.value, "\n")
cat("Model 3: p-value =", shapiro.test(residuals(model3))$p.value, "\n")
cat("Model 4: p-value =", shapiro.test(residuals(model4))$p.value, "\n")

# (iv) 提取 R^2 值
r2_model1 <- summary(model1)$r.squared
r2_model2 <- summary(model2)$r.squared
r2_model3 <- summary(model3)$r.squared
r2_model4 <- summary(model4)$r.squared
cat("\nR^2 Values:\n")
cat("Model 1:", r2_model1, "\n")
cat("Model 2:", r2_model2, "\n")
cat("Model 3:", r2_model3, "\n")
cat("Model 4:", r2_model4, "\n")

# 選擇最佳模型
cat("\nModel Selection:\n")
cat("Based on R^2, residual patterns, and normality tests, we choose the model with the highest R^2, random residuals, and residuals closest to normality.\n")

#b
beta1 <- coef(model3)["I(wa_wheat$time^2)"]
cat("Coefficient of TIME^2 in Model 3:", beta1, "\n")
cat("Interpretation: The rate of change of YIELD with respect to TIME is 2 *", beta1, "* TIME units per year.\n")
cat("For example, at TIME = 48 (1997), YIELD increases by approximately", 2 * beta1 * 48, "units per additional year.\n")

#c.
# 計算學生化殘差
stud_res <- rstudent(model3)

# 識別異常值
outliers <- which(abs(stud_res) > 2)
cat("Outliers (based on studentized residuals > 2):\n")
print(data.frame(Time = wa_wheat$time[outliers], Year = 1949 + wa_wheat$time[outliers], Studentized_Residual = stud_res[outliers]))

# 計算 LEVERAGE、DFBETAS 和 DFFITS
leverage <- hatvalues(model3)
dfbetas <- dfbetas(model3)
dffits <- dffits(model3)

# 識別高影響點
p <- 1  # 模型 1 有一個預測變數 (TIME)
n <- nrow(wa_wheat)
leverage_threshold <- 2 * (p + 1) / n
dfbetas_threshold <- 2 / sqrt(n)
dffits_threshold <- 2 * sqrt(p / n)

high_leverage <- which(leverage > leverage_threshold)
high_dfbetas <- which(abs(dfbetas[, "I(wa_wheat$time^2)"]) > dfbetas_threshold)
high_dffits <- which(abs(dffits) > dffits_threshold)

cat("\nHigh Leverage Points:\n")
print(data.frame(Time = wa_wheat$time[high_leverage], Year = 1949 + wa_wheat$time[high_leverage], Leverage = leverage[high_leverage]))
cat("\nHigh DFBETAS Points (for TIME coefficient):\n")
print(data.frame(Time = wa_wheat$time[high_dfbetas], Year = 1949 + wa_wheat$time[high_dfbetas], DFBETAS_TIME = dfbetas[high_dfbetas, "I(wa_wheat$time^2)"]))
cat("\nHigh DFFITS Points:\n")
print(data.frame(Time = wa_wheat$time[high_dffits], Year = 1949 + wa_wheat$time[high_dffits], DFFITS = dffits[high_dffits]))

#d.
# 1997 年的 TIME = 48
new_data <- data.frame(time = 48)

# 預測 1997 年的 YIELD（95% 預測區間）
pred <- predict(model3, newdata = new_data, interval = "prediction", level = 0.95)
cat("\nPrediction for 1997 (TIME = 48):\n")
print(pred[48,])

# 提取 1997 年的真實 YIELD
true_yield_1997 <- wa_wheat$northampton[wa_wheat$time == 48]
cat("True YIELD in 1997:", true_yield_1997, "\n")

# 檢查真實值是否在預測區間內
in_interval <- true_yield_1997 >= pred[48, "lwr"] && true_yield_1997 <= pred[48, "upr"]
cat("Does the prediction interval contain the true value?", in_interval, "\n")


#-------------------分割線-----------------------


#4.29
#a.
library(POE5Rdata)
data = data("cex5_small")

# FOOD summary
food_stats <- c(
  mean = mean(cex5_small$food, na.rm = TRUE),
  median = median(cex5_small$food, na.rm = TRUE),
  min = min(cex5_small$food, na.rm = TRUE),
  max = max(cex5_small$food, na.rm = TRUE),
  sd = sd(cex5_small$food, na.rm = TRUE))
print(food_stats)

# INCOME summary
income_stats <- c(
  mean = mean(cex5_small$income, na.rm = TRUE),
  median = median(cex5_small$income, na.rm = TRUE),
  min = min(cex5_small$income, na.rm = TRUE),
  max = max(cex5_small$income, na.rm = TRUE),
  sd = sd(cex5_small$income, na.rm = TRUE))
print(income_stats)

# FOOD 和 INCOME 的直方圖
par(mfrow = c(1, 2))  # 設置畫布為 1 行 2 列
hist(cex5_small$food, main = "FOOD Histogram", xlab = "FOOD", col = "lightblue", prob = TRUE)
curve(dnorm(x, mean = mean(cex5_small$food, na.rm = TRUE), sd = sd(cex5_small$food, na.rm = TRUE)), 
      add = TRUE, col = "red", lwd = 2)  # 添加正態分佈曲線
hist(cex5_small$income, main = "INCOME Histogram", xlab = "INCOME", col = "lightgreen", prob = TRUE)
curve(dnorm(x, mean = mean(cex5_small$income, na.rm = TRUE), sd = sd(cex5_small$income, na.rm = TRUE)), 
      add = TRUE, col = "red", lwd = 2)  # 添加正態分佈曲線

# 檢查是否為 "bell-shaped"：目視檢查直方圖是否接近正態分佈
# 進行 Jarque-Bera 檢驗
library(moments)
food_jb <- jarque.test(cex5_small$food)
income_jb <- jarque.test(cex5_small$income)
print("FOOD Jarque-Bera 檢驗:")
print(food_jb)
print("INCOME Jarque-Bera 檢驗:")
print(income_jb)

# b. 估計線性回歸模型 FOOD = β₀ + β₁*INCOME + ε
model <- lm(cex5_small$food ~ cex5_small$income, data = cex5_small)
summary(model)

# 繪製散點圖和回歸線
plot(cex5_small$income, cex5_small$food, main = "Scatter Plot of FOOD vs INCOME", 
     xlab = "INCOME", ylab = "FOOD", pch = 19, col = "blue")
abline(model, col = "red", lwd = 2)  # 添加回歸線
confint(model, "cex5_small$income", level = 0.95)

#c. 提取殘差並繪製殘差圖
residuals <- resid(model)
plot(cex5_small$income, residuals, main = "Residuals vs INCOME", 
     xlab = "INCOME", ylab = "Residuals", pch = 19, col = "purple")
abline(h = 0, col = "black", lty = 2)  # 添加水平線 y = 0

hist(residuals,main = "殘差直方圖", xlab = "殘差", col = "lightgray", prob = TRUE)
curve(dnorm(x, mean = mean(residuals), sd = sd(residuals)), add = TRUE, col = "red", lwd = 2)  # 疊加正態分佈曲線

# 對殘差進行 Jarque-Bera 檢驗
residuals_jb <- jarque.test(residuals)
print("殘差的 Jarque-Bera 檢驗:")
print(residuals_jb)

# d. 計算隨機點估計和 95% 置信區間，並計算收入彈性
# 隨機點估計：INCOME = 19.65 和 160
income_values <- c(19,65, 160)
predictions <- predict(model, newdata = data.frame(income = income_values), 
                       interval = "confidence", level = 0.95)
print("FOOD 的點估計和 95% 置信區間 (INCOME = 19.65 和 160):")
print(predictions[19,])
print(predictions[65,])
print(predictions[160,])
# 計算收入彈性：彈性 = β₁ * (INCOME / FOOD)
beta_1 <- coef(model)["cex5_small$income"]
elasticity_19 <- beta_1 * (19 / predictions[19, "fit"])
elasticity_65 <- beta_1 * (65 / predictions[65, "fit"])
elasticity_160 <- beta_1 * (160 / predictions[160, "fit"])
print("收入彈性 (INCOME = 19.65):")
print(elasticity_19)
print("收入彈性 (INCOME = 65):")
print(elasticity_65)
print("收入彈性 (INCOME = 160):")
print(elasticity_160)

#e.
# 估計對數-對數模型
loglog_model <- lm(log(cex5_small$food) ~ log(cex5_small$income), data = cex5_small)
print(summary(loglog_model))
# 線性模型
linear_model <- lm(cex5_small$food ~ cex5_small$income, data = cex5_small)

# 比較
par(mfrow = c(1, 2))
# 線性模型圖（b 小題）
plot(cex5_small$income, cex5_small$food, pch = 20, col = "black",
     xlab = "INCOME", ylab = "FOOD",
     main = "Linear Model: FOOD vs INCOME")
abline(linear_model, col = "blue", lwd = 2)

# 對數-對數模型圖
plot(log(cex5_small$income), log(cex5_small$food), pch = 20, col = "black",
     xlab = "ln(INCOME)", ylab = "ln(FOOD)",
     main = "Log-Log Model: ln(FOOD) vs ln(INCOME)")
abline(loglog_model, col = "blue", lwd = 2)
par(mfrow = c(1, 1))

# 計算廣義 R^2
residuals_loglog <- resid(loglog_model)
sigma2_loglog <- var(residuals_loglog)  # 殘差方差
food_pred_loglog <- exp(fitted(loglog_model) + 0.5 * sigma2_loglog)
ssr_loglog <- sum((food_actual - food_pred_loglog)^2)  # 殘差平方和
sst_loglog <- sum((food_actual - mean(food_actual))^2)  # 總平方和
generalized_r2_loglog <- 1 - ssr_loglog / sst_loglog
cat("對數-對數模型的 generalized R^2：", generalized_r2_loglog, "\n")


# 線性模型的 R^2
r2_linear <- summary(linear_model)$r.squared
cat("R^2 for Linear Model:", r2_linear, "\n")

r2_loglog <- summary(loglog_model)$r.squared
cat("R^2 for loglog Model:", r2_loglog, "\n")

#f.
gamma2 <- coef(loglog_model)["log(cex5_small$income)"]
se_gamma2 <- summary(loglog_model)$coefficients["log(cex5_small$income)", "Std. Error"]
ci_lower <- gamma2 - qt(0.975, df = nrow(cex5_small) - 2) * se_gamma2
ci_upper <- gamma2 + qt(0.975, df = nrow(cex5_small) - 2) * se_gamma2

beta1 <- coef(linear_model)["cex5_small$income"]
mean_income <- mean(cex5_small$income)
mean_food <- mean(cex5_small$food)
elasticity_linear <- beta1 * (mean_income / mean_food)
se_beta1 <- summary(linear_model)$coefficients["cex5_small$income", "Std. Error"]
se_elasticity_linear <- se_beta1 * (mean_income / mean_food)
ci_lower_linear <- elasticity_linear - qt(0.975, df = nrow(cex5_small) - 2) * se_elasticity_linear
ci_upper_linear <- elasticity_linear + qt(0.975, df = nrow(cex5_small) - 2) * se_elasticity_linear

cat("Point Estimate of Elasticity (Log-Log Model):", gamma2, "\n")
cat("95% Confidence Interval for Elasticity:", ci_lower, "to", ci_upper, "\n")
cat("Elasticity from Linear Model (at mean):", elasticity_linear, "\n")
cat("95% Confidence Interval for Linear Model Elasticity:", ci_lower_linear, "to", ci_upper_linear, "\n")

t_stat <- (gamma2 - elasticity_linear) / sqrt(se_gamma2^2 + se_elasticity_linear^2)
p_value <- 2 * (1 - pt(abs(t_stat), df = nrow(cex5_small) - 2))
cat("t-statistic for difference in elasticities:", t_stat, "\n")
cat("p-value:", p_value, "\n")
if (p_value < 0.05) {
  cat("There's statistical evidence that the elasticities are different\n")
} else {
  cat("There's no statistical evidence that the elasticities are different\n")
}

# g 小題：殘差分析
residuals_loglog <- residuals(loglog_model)
plot(log(cex5_small$income), residuals_loglog, type = "p", pch = 20, col = "blue",
     xlab = "ln(INCOME)", ylab = "Residuals",
     main = "Log-Log Model: Residuals vs ln(INCOME)")
abline(h = 0, lty = 2, col = "red")

hist(residuals, breaks = 20, prob = TRUE, 
     xlab = "Residuals", main = "Histogram of Residuals",col = "lightblue")
curve(dnorm(x, mean = mean(residuals), sd = sd(residuals)), add = TRUE, 
      col = "red", lwd = 2)
library(tseries)
jb_test <- jarque.bera.test(residuals_loglog)
cat("\nJarque-Bera Test for Normality:\n")
cat("Test Statistic:", jb_test$statistic, "\n")
cat("p-value:", jb_test$p.value, "\n")
if (jb_test$p.value < 0.05) {
  cat("Conclusion: Reject the null hypothesis of normality (p < 0.05). The regression errors are not normally distributed.\n")
} else {
  cat("Conclusion: Fail to reject the null hypothesis of normality (p >= 0.05). The regression errors are approximately normally distributed.\n")
}

# h 小題：線性-對數模型
linlog_model <- lm(cex5_small$food ~ log(cex5_small$income), data = cex5_small)
print(summary(linlog_model))

par(mfrow = c(1, 3))
plot(cex5_small$income, cex5_small$food, pch = 20, col = "black",
     xlab = "INCOME", ylab = "FOOD",
     main = "Linear Model: FOOD vs INCOME")
abline(linear_model, col = "blue", lwd = 2)

plot(log(cex5_small$income), log(cex5_small$food), pch = 20, col = "black",
     xlab = "ln(INCOME)", ylab = "ln(FOOD)",
     main = "Log-Log Model: ln(FOOD) vs ln(INCOME)")
abline(loglog_model, col = "blue", lwd = 2)

plot(log(cex5_small$income), cex5_small$food, pch = 20, col = "black",
     xlab = "ln(INCOME)", ylab = "FOOD",
     main = "Linear-Log Model: FOOD vs ln(INCOME)")
abline(linlog_model, col = "blue", lwd = 2)
par(mfrow = c(1, 1))


r2_linear <- summary(linear_model)$r.squared
r2_loglog <- cor(fitted(loglog_model), log(cex5_small$food))^2
r2_linlog <- summary(linlog_model)$r.squared

cat("\nR^2 Comparison:\n")
cat("Linear Model R^2:", r2_linear, "\n")
cat("Log-Log Model Generalized R^2:", r2_loglog, "\n")
cat("Linear-Log Model R^2:", r2_linlog, "\n")

#i
income_values <- c(19,65, 160)
predictions <- predict(linlog_model, newdata = data.frame(income = income_values), 
                       interval = "confidence", level = 0.95)
print("FOOD 的點估計和 95% 置信區間 (INCOME = 19,65 和 160):")
print(predictions[19,])
print(predictions[65,])
print(predictions[160,])
# 計算收入彈性：彈性 = β₁ * (INCOME / FOOD)
beta_1 <- coef(linlog_model)["log(cex5_small$income)"]
elasticity_19 <- beta_1 * (19 / predictions[19, "fit"])
elasticity_65 <- beta_1 * (65 / predictions[65, "fit"])
elasticity_160 <- beta_1 * (160 / predictions[160, "fit"])
print("收入彈性 (INCOME = 19):")
print(elasticity_19)
print("收入彈性 (INCOME = 65):")
print(elasticity_65)
print("收入彈性 (INCOME = 160):")
print(elasticity_160)

#j.
residuals_linlog <- resid(linlog_model)
plot(log(cex5_small$income), residuals_linlog, main = "線性-對數模型：殘差 vs ln(INCOME)", 
     xlab = "ln(INCOME)", ylab = "殘差", pch = 19, col = "purple")
abline(h = 0, col = "black", lty = 2)  # 添加 y = 0 的水平線

# 繪製殘差直方圖並疊加正態分佈曲線
hist(residuals_linlog, main = "線性-對數模型殘差直方圖", 
     xlab = "殘差", col = "lightgray", prob = TRUE, breaks = 20)
curve(dnorm(x, mean = mean(residuals_linlog), sd = sd(residuals_linlog)), 
      add = TRUE, col = "red", lwd = 2)  # 疊加正態分佈曲線

# 對殘差進行 Jarque-Bera 檢驗
linlog_residuals_jb <- jarque.test(residuals_linlog)
print("線性-對數模型殘差的 Jarque-Bera 檢驗：")
print(linlog_residuals_jb)