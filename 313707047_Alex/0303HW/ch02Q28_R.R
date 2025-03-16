rm(list=ls())  
library(POE5Rdata)  
data("cps5_small")  
# 確保數據載入
cps5_data <- cps5_small  # 確保變數名稱一致

# 設定圖表排列方式：1 列 2 行
par(mfrow = c(1, 2))  

# 繪製 EDUC 直方圖
hist(cps5_data$educ, 
     main = "Histogram of EDUC", 
     xlab = "Years of Education", 
     col = "lightgreen", 
     border = "black")

# 繪製 WAGE 直方圖
hist(cps5_data$wage, 
     main = "Histogram of WAGE", 
     xlab = "Hourly Wage Rate", 
     col = "lightblue", 
     border = "black")

# 恢復預設單張圖表模式
par(mfrow = c(1, 1))


# 執行線性回歸
model_wage <- lm(wage ~ educ, data = cps5_data)

# 顯示回歸結果
summary(model_wage)




# 計算殘差
residuals_wage <- residuals(model_wage)

# 繪製殘差圖
plot(cps5_data$educ, residuals_wage, 
     xlab = "Years of Education", 
     ylab = "Residuals", 
     main = "Residual Plot: WAGE vs EDUC", 
     col = "blue", pch = 16)
abline(h = 0, col = "red", lwd = 2)




male_model <- lm(wage ~ educ, data = subset(cps5_small, female == 0))
female_model <- lm(wage ~ educ, data = subset(cps5_small, female == 1))

black_model <- lm(wage ~ educ, data = subset(cps5_small, black == 1))
white_model <- lm(wage ~ educ, data = subset(cps5_small, black == 0))

summary(male_model)
summary(female_model)
summary(black_model)
summary(white_model)



# 產生 EDUC² 變數
cps5_data$educ2 <- cps5_data$educ^2

# 執行線性回歸
model_linear <- lm(wage ~ educ, data = cps5_data)

# 執行二次回歸
model_quadratic <- lm(wage ~ educ + educ2, data = cps5_data)

# 繪製散點圖 (WAGE vs EDUC)
plot(cps5_data$educ, cps5_data$wage, 
     xlab = "Years of Education", 
     ylab = "Hourly Wage Rate", 
     main = "Fitted Linear & Quadratic Models", 
     col = "gray", pch = 16)

# 繪製線性模型的擬合線
abline(model_linear, col = "red", lwd = 2, lty = 2)

# 產生 EDUC 的範圍（用於繪製二次回歸曲線）
educ_range <- seq(min(cps5_data$educ), max(cps5_data$educ), length.out = 100)

# 計算二次回歸的擬合值
quadratic_fitted <- predict(model_quadratic, newdata = data.frame(educ = educ_range, educ2 = educ_range^2))

# 繪製二次回歸的擬合曲線
lines(educ_range, quadratic_fitted, col = "blue", lwd = 2)

# 加入圖例
legend("topleft", legend = c("Linear Fit", "Quadratic Fit"), 
       col = c("red", "blue"), lwd = 2, lty = c(2, 1))


# 建立 EDUC² 變數
cps5_data$educ2 <- cps5_data$educ^2

# 執行二次回歸
model_quadratic <- lm(wage ~ educ + educ2, data = cps5_data)

# 顯示回歸結果
summary(model_quadratic)

# 提取 EDUC² 係數 (α₂)
alpha2 <- coef(model_quadratic)["educ"]

# 計算在 EDUC = 12 時的邊際效應
marginal_effect_12 <- 2 * alpha2 * 12

# 計算在 EDUC = 16 時的邊際效應
marginal_effect_16 <- 2 * alpha2 * 16

# 顯示結果
marginal_effect_12
marginal_effect_16

