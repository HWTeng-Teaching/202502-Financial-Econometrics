#CH04Q4
#(a) 定義 Model 1 的參數
exper <- 0:30
rating_model1 <- 64.289 + 0.990 * exper

# 繪圖
plot(exper, rating_model1, type = "l", col = "blue", lwd = 2,
     xlab = "Years of Experience (EXPER)", ylab = "Predicted RATING",
     main = "Fitted Values from Model 1")

#(b) 定義 Model 2 的參數
exper2 <- 1:30  # ln(EXPER) 無法計算 EXPER = 0
rating_model2 <- 39.464 + 15.312 * log(exper2)

# 繪圖
plot(exper2, rating_model2, type = "l", col = "red", lwd = 2,
     xlab = "Years of Experience (EXPER)", ylab = "Predicted RATING",
     main = "Fitted Values from Model 2")

# (c)Model 1: RATING = 64.289 + 0.990 * EXPER
marginal_effect_model1 <- 0.990  # 邊際影響為係數本身

cat("Model 1 的邊際影響: \n")
cat("10 年經驗時，每增加一年經驗的影響: ", marginal_effect_model1, "\n")
cat("20 年經驗時，每增加一年經驗的影響: ", marginal_effect_model1, "\n")

#(d) Model 2: RATING = 39.464 + 15.312 * ln(EXPER)
# 邊際影響 = 15.312 / EXPER
marginal_effect_10 <- 15.312 / 10
marginal_effect_20 <- 15.312 / 20

cat("Model 2 的邊際影響: \n")
cat("10 年經驗時，每增加一年經驗的影響: ", marginal_effect_10, "\n")
cat("20 年經驗時，每增加一年經驗的影響: ", marginal_effect_20, "\n")

#(d)
R2_model1 <- 0.3793
R2_model2 <- 0.6414
R2_model1_subset <- 0.4858  # Model 1 在僅使用有經驗者時的 R²

cat("Model 1 原始 R²:", R2_model1, "\n")
cat("Model 1 (去除 EXPER = 0) R²:", R2_model1_subset, "\n")
cat("Model 2 R²:", R2_model2, "\n")

#CH04Q28
# 載入數據
library(POE5Rdata)
data(wa_wheat)
summary(wa_wheat)
# 估計四個模型
# (1) 線性模型
model1 <- lm(northampton ~ time, data=wa_wheat)

# (2) linear-log
model2 <- lm(northampton ~ log(time), data=wa_wheat)

# (3) quadratic model
model3 <- lm(northampton ~ I(time^2), data=wa_wheat)

# (4) log-linear model
model4 <- lm(log(northampton) ~ time, data=wa_wheat)


par(mfrow = c(2, 2))
plot(wa_wheat$time, wa_wheat$northampton, main="Linear", xlab="TIME", ylab="YIELD")
abline(model1, col="blue")

plot(wa_wheat$time, wa_wheat$northampton, main="Linear-Log", xlab="TIME", ylab="YIELD")
lines(wa_wheat$time, fitted(model2), col="blue")

plot(wa_wheat$time, wa_wheat$northampton, main="Quadratic", xlab="TIME", ylab="YIELD")
lines(wa_wheat$time, fitted(model3), col="blue")

plot(wa_wheat$time, log(wa_wheat$northampton), main="Log-Linear", xlab="TIME", ylab="ln(YIELD)")
lines(wa_wheat$time, fitted(model4), col="blue")

par(mfrow = c(2, 2)) #將畫面分成 2x2

# 線性模型殘差圖
plot(wa_wheat$time, resid(model1), 
     main = "Residuals (Linear Model)", 
     xlab = "TIME", ylab = "Residuals")
abline(h = 0, col = "red")

# Log-linear 模型殘差圖
plot(wa_wheat$time, resid(model2), 
     main = "Residuals (Linear-Log Model)", 
     xlab = "TIME", ylab = "Residuals")
abline(h = 0, col = "red")

# Quadratic 模型殘差圖
plot(wa_wheat$time, resid(model3), 
     main = "Residuals (Quadratic Model)", 
     xlab = "TIME", ylab = "Residuals")
abline(h = 0, col = "red")

# Log-lin 模型殘差圖
plot(wa_wheat$time, resid(model4), 
     main = "Residuals (Log-Linear Model)", 
     xlab = "TIME", ylab = "Residuals")
abline(h = 0, col = "red")

install.packages("tseries")
library(tseries)
jarque.bera.test(resid(model1))
jarque.bera.test(resid(model2))
jarque.bera.test(resid(model3))
jarque.bera.test(resid(model4))

summary(model1)  # 線性
summary(model2)  # linear-log
summary(model3)  # quadratic
summary(model4)  # log-linear

#(c)
install.packages("car")
library(car)

# 計算異常值指標
leverage <- hatvalues(model3)  # 杠杆值
dfbetas <- dfbetas(model3)  # DFBetas
dffits <- dffits(model3)  # DFFITS

# 計算 studentized residuals
stud_resid <- rstudent(model3)

# 找出異常值
outlier_leverage <- which(leverage > 2 * mean(leverage))
outlier_dfbetas <- which(abs(dfbetas[, 2]) > 2 / sqrt(length(wa_wheat$northampton)))
outlier_dffits <- which(abs(dffits) > 2 * sqrt(mean(dffits^2)))
outlier_stud_resid <- which(abs(stud_resid) > 2)  # Studentized residuals

cat("High leverage points: ", outlier_leverage, "\n")
cat("High DFBetas points: ", outlier_dfbetas, "\n")
cat("High DFFITS points: ", outlier_dffits, "\n")
cat("High Studentized Residuals points: ", outlier_stud_resid, "\n")

# 繪製 Studentized Residuals 圖表
par(mfrow = c(1, 1))
plot(stud_resid, main = "Studentized Residuals", ylab = "Residuals", xlab = "Observation Index")
abline(h = c(-2, 2), col = "red", lty = 2)  # 加上異常值閾值線

# (d)設定訓練集 (1950-1996)
data_1996 <- subset(wa_wheat, time < 47)

# 重新估計模型（假設選擇線性模型）
model_final <- lm(northampton ~ I(time^2), data=data_1996)

# 找出 1997 年的 time 值
time_1997 <- data.frame(time = 47)

# 1997 年的預測
pred_1997 <- predict(model_final, newdata = data.frame(time_1997), interval = "prediction", level = 0.95)

# 顯示結果
print(pred_1997)

# 檢查真實值是否落在區間內
true_value_1997 <- wa_wheat$northampton[wa_wheat$time == 47]
in_interval <- (true_value_1997 >= pred_1997[,"lwr"]) & (true_value_1997 <= pred_1997[,"upr"])
cat("True value is within the interval:", in_interval, "\n")


#CH04Q29
library(POE5Rdata)
data(cex5_small)

# (a)計算描述統計量
summary_food <- summary(cex5_small$food)
summary_income <- summary(cex5_small$income)
sd_food <- sd(cex5_small$food)
sd_income <- sd(cex5_small$income)

# 輸出結果
cat("FOOD Summary:\n")
print(summary_food)
cat("FOOD Standard Deviation:", sd_food, "\n")
cat("INCOME Summary:\n")
print(summary_income)
cat("INCOME Standard Deviation:", sd_income, "\n")

# 繪製直方圖
par(mfrow=c(1,2))  # 將圖形分成1行2列
hist(cex5_small$food, main="Histogram of FOOD", xlab="FOOD", col="lightblue")
abline(v=mean(cex5_small$food), col="red", lwd=2, lty=2)  # 均值
abline(v=median(cex5_small$food), col="blue", lwd=2, lty=2)  # 中位數
hist(cex5_small$income, main="Histogram of INCOME", xlab="INCOME", col="lightgreen")
abline(v=mean(cex5_small$income), col="red", lwd=2, lty=2)
abline(v=median(cex5_small$income), col="blue", lwd=2, lty=2)

# 安裝並載入moments包進行Jarque-Bera檢驗
if (!require(moments)) install.packages("moments")
library(moments)

# Jarque-Bera檢驗
jb_food <- jarque.test(cex5_small$food)
jb_income <- jarque.test(cex5_small$income)
cat("Jarque-Bera Test for FOOD:\n")
print(jb_food)
cat("Jarque-Bera Test for INCOME:\n")
print(jb_income)

# (b)線性回歸
model_linear <- lm(food ~ income, data=cex5_small)

# 模型摘要
summary(model_linear)

# β₂的95%置信區間
confint_beta2 <- confint(model_linear, "income", level=0.95)
cat("95% Confidence Interval for β₂:\n")
print(confint_beta2)

# 繪製散點圖並添加回歸線
par(mfrow=c(1,1)) 
plot(cex5_small$income, cex5_small$food, main="FOOD vs INCOME", xlab="INCOME", ylab="FOOD", pch=16)
abline(model_linear, col="red", lwd=2)

# (c)
#獲取殘差
residuals_linear <- residuals(model_linear)

# 殘差對INCOME的圖
plot(cex5_small$income, residuals_linear, main="Residuals vs INCOME", xlab="INCOME", ylab="Residuals", pch=16)
abline(h=0, col="red", lwd=2)

# 殘差直方圖
hist(residuals_linear, main="Histogram of Residuals", xlab="Residuals", col="lightgray")

# Jarque-Bera檢驗
jb_residuals <- jarque.test(residuals_linear)
cat("Jarque-Bera Test for Residuals:\n")
print(jb_residuals)


#(d)
# 指定INCOME值
income_values <- c(19, 65, 160)

# 預測FOOD值
food_pred <- predict(model_linear, newdata=data.frame(income=income_values))

# 計算彈性點估計：β₂ * (INCOME / FOOD)
beta2 <- coef(model_linear)["income"]
elasticity_point <- beta2 * (income_values / food_pred)
cat("Elasticity at INCOME = 19, 65, 160:\n")
print(elasticity_point)

# 簡化版95%置信區間（僅點估計範圍，完整需要delta法或bootstrap）
ci_beta2 <- confint(model_linear, "income", level=0.95)
elasticity_ci <- t(sapply(1:3, function(i) ci_beta2 * income_values[i] / food_pred[i]))
colnames(elasticity_ci) <- c("2.5%", "97.5%")
rownames(elasticity_ci) <- income_values
cat("Approximate 95% CI for Elasticity:\n")
print(elasticity_ci)


#(e)
# 創建對數變量
cex5_small$ln_FOOD <- log(cex5_small$food)
cex5_small$ln_INCOME <- log(cex5_small$income)

# log-log回歸
model_loglog <- lm(ln_FOOD ~ ln_INCOME, data=cex5_small)
summary(model_loglog)

# 散點圖與回歸線
plot(cex5_small$ln_INCOME, cex5_small$ln_FOOD, main="ln(FOOD) vs ln(INCOME)", xlab="ln(INCOME)", ylab="ln(FOOD)", pch=16)
abline(model_loglog, col="red", lwd=2)

#(f)
# log-log模型彈性為γ₂
gamma2 <- coef(model_loglog)["ln_INCOME"]
ci_gamma2 <- confint(model_loglog, "ln_INCOME", level=0.95)
cat("Elasticity (γ₂):", gamma2, "\n")
cat("95% CI for Elasticity:\n")
print(ci_gamma2)

#(g)
# 獲取殘差
residuals_loglog <- residuals(model_loglog)

# 殘差圖
plot(cex5_small$ln_INCOME, residuals_loglog, main="Residuals vs ln(INCOME)", xlab="ln(INCOME)", ylab="Residuals", pch=16)
abline(h=0, col="red", lwd=2)

# 殘差直方圖與檢驗
hist(residuals_loglog, main="Histogram of Residuals (Log-Log)", xlab="Residuals", col="lightgray")
jb_residuals_loglog <- jarque.test(residuals_loglog)
print(jb_residuals_loglog)

#(h)
# linear-log回歸
model_linearlog <- lm(food ~ ln_INCOME, data=cex5_small)
summary(model_linearlog)

# 散點圖
plot(cex5_small$ln_INCOME, cex5_small$food, main="FOOD vs ln(INCOME)", xlab="ln(INCOME)", ylab="FOOD", pch=16)
abline(model_linearlog, col="red", lwd=2)

# 比較R²
r2_linear <- summary(model_linear)$r.squared
r2_loglog <- summary(model_loglog)$r.squared
r2_linearlog <- summary(model_linearlog)$r.squared
cat("R² Linear:", r2_linear, "\nR² Log-Log:", r2_loglog, "\nR² Linear-Log:", r2_linearlog, "\n")

#(i)
# 預測FOOD
ln_income_values <- log(income_values)
food_pred_linearlog <- predict(model_linearlog, newdata=data.frame(ln_INCOME=ln_income_values))

# 彈性：α₂ / FOOD
alpha2 <- coef(model_linearlog)["ln_INCOME"]
elasticity_linearlog <- alpha2 / food_pred_linearlog
cat("Elasticity at INCOME = 19, 65, 160:\n")
print(elasticity_linearlog)

ci_alpha2 <- confint(model_linearlog, "ln_INCOME", level=0.95)
elasticity_ci_alpha2 <- t(sapply(1:3, function(i) ci_alpha2 / food_pred_linearlog[i]))
colnames(elasticity_ci_alpha2) <- c("2.5%", "97.5%")
rownames(elasticity_ci_alpha2) <- income_values
cat("Approximate 95% CI for Elasticity:\n")
print(elasticity_ci_alpha2)

#(j)
# 殘差分析
residuals_linearlog <- residuals(model_linearlog)
plot(cex5_small$ln_INCOME, residuals_linearlog, main="Residuals vs ln(INCOME)", xlab="ln(INCOME)", ylab="Residuals", pch=16)
abline(h=0, col="red", lwd=2)
hist(residuals_linearlog, main="Histogram of Residuals (Linear-Log)", xlab="Residuals", col="lightgray")
jb_residuals_linearlog <- jarque.test(residuals_linearlog)
print(jb_residuals_linearlog)
