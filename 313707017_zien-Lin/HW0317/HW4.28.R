library(POE5Rdata)
data("wa_wheat")

# 創建 TIME 變量
x <- wa_wheat$time
y <- wa_wheat$northampton

# 模型 1：YIELD = β0 + β1*TIME + e
model1 <- lm(y ~ x, data = wa_wheat)

# 模型 2：YIELD = α0 + α1*ln(TIME) + e
model2 <- lm(y ~ log(x), data = wa_wheat)

# 模型 3：YIELD = γ0 + γ1*TIME^2 + e
model3 <- lm(y ~ I(x^2), data = wa_wheat)

# 模型 4：ln(YIELD) = φ0 + φ1*TIME + e
# 確保 YIELD > 0，否則 ln(YIELD) 會報錯
if (any(y <= 0)) {
  stop("YIELD 包含非正值，無法計算 ln(YIELD)")
}
model4 <- lm(log(y) ~ x, data = wa_wheat)

# (i) 繪製擬合方程的圖形
# 為每個模型生成預測值
wa_wheat$pred1 <- predict(model1)
wa_wheat$pred2 <- predict(model2)
wa_wheat$pred3 <- predict(model3)
wa_wheat$pred4 <- exp(predict(model4))  # 模型 4 是 ln(YIELD)，需要轉回 YIELD

# 使用 ggplot2 繪製
library(ggplot2)

# 繪製實際值與擬合值的圖形
ggplot(wa_wheat, aes(x = x)) +
  geom_point(aes(y = y), color = "black", size = 2) +  # 實際值
  geom_line(aes(y = pred1, color = "Model 1 (Linear)"), size = 1) +
  geom_line(aes(y = pred2, color = "Model 2 (Log TIME)"), size = 1) +
  geom_line(aes(y = pred3, color = "Model 3 (Quadratic TIME)"), size = 1) +
  geom_line(aes(y = pred4, color = "Model 4 (Log YIELD)"), size = 1) +
  labs(title = "Fitted Equations for Wheat Yield (1950-1997)",
       x = "TIME (1 = 1950, 48 = 1997)",
       y = "YIELD",
       color = "Model") +
  theme_minimal()

# (ii) 繪製殘差圖
wa_wheat$resid1 <- residuals(model1)
wa_wheat$resid2 <- residuals(model2)
wa_wheat$resid3 <- residuals(model3)
wa_wheat$resid4 <- residuals(model4)  # 模型 4 的殘差是 ln(YIELD) 上的殘差

# 繪製 Model 1 的殘差圖
p1 <- ggplot(wa_wheat, aes(x = x, y = resid1)) +
  geom_point(color = "blue") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  labs(title = "Residuals of Model 1 (Linear)",
       x = "TIME",
       y = "Residuals") +
  theme_minimal()

# 繪製 Model 2 的殘差圖
p2 <- ggplot(wa_wheat, aes(x = x, y = resid2)) +
  geom_point(color = "blue") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  labs(title = "Residuals of Model 2 (Log TIME)",
       x = "TIME",
       y = "Residuals") +
  theme_minimal()

# 繪製 Model 3 的殘差圖
p3 <- ggplot(wa_wheat, aes(x = x, y = resid3)) +
  geom_point(color = "blue") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  labs(title = "Residuals of Model 3 (Quadratic TIME)",
       x = "TIME",
       y = "Residuals") +
  theme_minimal()

# 繪製 Model 4 的殘差圖
p4 <- ggplot(wa_wheat, aes(x = x, y = resid4)) +
  geom_point(color = "blue") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  labs(title = "Residuals of Model 4 (Log YIELD)",
       x = "TIME",
       y = "Residuals") +
  theme_minimal()

# 使用 par() 分割畫布，排列四張圖
par(mfrow = c(2, 2))  # 設置 2 行 2 列的畫布
print(p1)
print(p2)
print(p3)
print(p4)

# (iii) 誤差正態性檢驗（Shapiro-Wilk 檢驗）
shapiro_test1 <- shapiro.test(residuals(model1))
shapiro_test2 <- shapiro.test(residuals(model2))
shapiro_test3 <- shapiro.test(residuals(model3))
shapiro_test4 <- shapiro.test(residuals(model4))

# 顯示檢驗結果
cat("Shapiro-Wilk Test for Model 1: p-value =", shapiro_test1$p.value, "\n")
cat("Shapiro-Wilk Test for Model 2: p-value =", shapiro_test2$p.value, "\n")
cat("Shapiro-Wilk Test for Model 3: p-value =", shapiro_test3$p.value, "\n")
cat("Shapiro-Wilk Test for Model 4: p-value =", shapiro_test4$p.value, "\n")

# (iv) 提取 R^2 和調整後 R^2
r2_model1 <- summary(model1)$r.squared
r2_model2 <- summary(model2)$r.squared
r2_model3 <- summary(model3)$r.squared
r2_model4 <- summary(model4)$r.squared

adj_r2_model1 <- summary(model1)$adj.r.squared
adj_r2_model2 <- summary(model2)$adj.r.squared
adj_r2_model3 <- summary(model3)$adj.r.squared
adj_r2_model4 <- summary(model4)$adj.r.squared

# 顯示 R^2 和調整後 R^2
cat("Model 1 R^2:", r2_model1, "Adjusted R^2:", adj_r2_model1, "\n")
cat("Model 2 R^2:", r2_model2, "Adjusted R^2:", adj_r2_model2, "\n")
cat("Model 3 R^2:", r2_model3, "Adjusted R^2:", adj_r2_model3, "\n")
cat("Model 4 R^2:", r2_model4, "Adjusted R^2:", adj_r2_model4, "\n")

# b.-------------------------
# 擬合 Model 3
model3 <- lm(y ~ I(x^2), data = wa_wheat)
summary(model3)

# c.------------------------
# 擬合 Model 3
model3 <- lm(y ~ I(x^2), data = wa_wheat)

# 標準化殘差（studentized residuals）
studentized_resid <- rstudent(model3)

# 槓桿值（leverage）
leverage <- hatvalues(model3)

# DFBETAS
dfbetas_vals <- dfbetas(model3)

# DFFITS
dffits_vals <- dffits(model3)

# 將診斷統計量添加到數據框中
wa_wheat$studentized_resid <- studentized_resid
wa_wheat$leverage <- leverage
wa_wheat$dffits <- dffits_vals
wa_wheat$dfbetas_intercept <- dfbetas_vals[,1]  # 截距的 DFBETAS
wa_wheat$dfbetas_time2 <- dfbetas_vals[,2]       # TIME^2 的 DFBETAS

# 查看數據
head(wa_wheat)

# 設定閾值
n <- 48
k <- 1
threshold_studentized <- 2
threshold_leverage <- 2 * (k + 1) / n  # 0.0834
threshold_dfbetas <- 2 / sqrt(n)        # 0.2887
threshold_dffits <- 2 / sqrt(n / (k + 1))  # 0.4082

# 識別異常觀測值（使用基礎 R）
wa_wheat$unusual_studentized <- abs(wa_wheat$studentized_resid) > threshold_studentized
wa_wheat$unusual_leverage <- wa_wheat$leverage > threshold_leverage
wa_wheat$unusual_dfbetas_intercept <- abs(wa_wheat$dfbetas_intercept) > threshold_dfbetas
wa_wheat$unusual_dfbetas_time2 <- abs(wa_wheat$dfbetas_time2) > threshold_dfbetas
wa_wheat$unusual_dffits <- abs(wa_wheat$dffits) > threshold_dffits

# 篩選異常觀測值
unusual_obs <- wa_wheat[
  wa_wheat$unusual_studentized | 
    wa_wheat$unusual_leverage | 
    wa_wheat$unusual_dfbetas_intercept | 
    wa_wheat$unusual_dfbetas_time2 | 
    wa_wheat$unusual_dffits, 
]

# 顯示異常觀測值
print(unusual_obs)

# 繪製標準化殘差圖
p1 <- ggplot(wa_wheat, aes(x = time, y = studentized_resid)) +
  geom_point(color = "blue") +
  geom_hline(yintercept = c(-threshold_studentized, threshold_studentized), linetype = "dashed", color = "red") +
  labs(title = "Studentized Residuals", x = "TIME", y = "Studentized Residuals") +
  theme_minimal()

# 繪製槓桿值圖
p2 <- ggplot(wa_wheat, aes(x = time, y = leverage)) +
  geom_point(color = "blue") +
  geom_hline(yintercept = threshold_leverage, linetype = "dashed", color = "red") +
  labs(title = "Leverage Values", x = "TIME", y = "Leverage") +
  theme_minimal()

# 繪製 DFBETAS 圖（截距）
p3 <- ggplot(wa_wheat, aes(x = time, y = dfbetas_intercept)) +
  geom_point(color = "blue") +
  geom_hline(yintercept = c(-threshold_dfbetas, threshold_dfbetas), linetype = "dashed", color = "red") +
  labs(title = "DFBETAS for Intercept", x = "TIME", y = "DFBETAS (Intercept)") +
  theme_minimal()

# 繪製 DFBETAS 圖（TIME^2 係數）
p4 <- ggplot(wa_wheat, aes(x = time, y = dfbetas_time2)) +
  geom_point(color = "blue") +
  geom_hline(yintercept = c(-threshold_dfbetas, threshold_dfbetas), linetype = "dashed", color = "red") +
  labs(title = "DFBETAS for TIME^2 Coefficient", x = "TIME", y = "DFBETAS (TIME^2)") +
  theme_minimal()

# 繪製 DFFITS 圖
p5 <- ggplot(wa_wheat, aes(x = time, y = dffits)) +
  geom_point(color = "blue") +
  geom_hline(yintercept = c(-threshold_dffits, threshold_dffits), linetype = "dashed", color = "red") +
  labs(title = "DFFITS", x = "TIME", y = "DFFITS") +
  theme_minimal()

# 使用 par() 排列圖形（5 張圖，分成 3 行）
par(mfrow = c(3, 2))  # 3 行 2 列（最後一格會留空）
print(p1)
print(p2)
print(p3)
print(p4)
print(p5)

# d.-----------------
# 篩選 1950-1996 年的數據（TIME <= 47）
wa_wheat_1950_1996 <- wa_wheat[wa_wheat$time <= 47, ]

# 使用 1950-1996 年的數據擬合 Model 3
model3_1950_1996 <- lm(northampton ~ I(time^2), data = wa_wheat_1950_1996)

# 創建新數據框，包含 1997 年的 TIME 值
new_data <- data.frame(time = 48)

# 計算 1997 年的預測值和 95% 預測區間
prediction <- predict(model3_1950_1996, newdata = new_data, interval = "prediction", level = 0.95)

# 顯示預測結果
print(prediction)

# 提取 1997 年的真實 YIELD 值
true_yield_1997 <- wa_wheat$northampton[wa_wheat$time == 48]
print(true_yield_1997)
