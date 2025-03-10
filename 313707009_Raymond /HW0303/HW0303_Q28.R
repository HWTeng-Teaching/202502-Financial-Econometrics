if (!requireNamespace("remotes", quietly = TRUE)) install.packages("remotes")
remotes::install_github("ccolonescu/POE5Rdata", force = TRUE)

library(POE5Rdata)
data("cps5_small")
data <- cps5_small

# 查看摘要統計量
summary(data$wage)
summary(data$educ)

# 畫直方圖
par(mfrow=c(1,2))  # 顯示兩個圖
hist(data$wage, main="Histogram of wage", xlab="wage")
hist(data$educ, main="Histogram of education", xlab="education")

# 線性回歸模型
model_linear <- lm(wage ~ educ, data=data)

# 顯示回歸結果
summary(model_linear)


# 計算殘差
residuals_linear <- residuals(model_linear)

# 繪製殘差圖
plot(data$educ, residuals_linear, main="Residuals vs education", xlab="education", ylab="Residuals")
abline(h=0, col="red")

# 檢查殘差圖中是否有模式
# 假設資料中有性別 (SEX) 和種族 (RACE) 變數
model_male <- lm(wage ~ educ, data=data[data$female == 0, ])
model_female <- lm(wage ~ educ, data=data[data$female == 1, ])
model_black <- lm(wage ~ educ, data=data[data$black == 1, ])
model_white <- lm(wage ~ educ, data=data[data$black == 0, ])

# 顯示回歸結果
summary(model_male)
summary(model_female)
summary(model_black)
summary(model_white)

# 估計二次回歸模型
model_quadratic <- lm(wage ~ educ + I(educ^2), data=data)

# 顯示回歸結果
summary(model_quadratic)

# 計算邊際效應
marginal_effect_12 <- 2 * coef(model_quadratic)[3] * 12  # 假設12年教育
marginal_effect_16 <- 2 * coef(model_quadratic)[3] * 16  # 假設16年教育

# 輸出邊際效應
cat("Marginal effect at 12 years of education:", marginal_effect_12, "\n")
cat("Marginal effect at 16 years of education:", marginal_effect_16, "\n")

# 繪製資料點
plot(data$educ, data$wage, main="Linear vs Quadratic Fit", xlab="EDUCATION", ylab="WAGE",col = "blue", pch = 16, ylim=c(0, 120))

# 線性回歸擬合線
abline(model_linear, col="red", lwd=2)

# 二次回歸擬合線
curve(coef(model_quadratic)[1] + coef(model_quadratic)[2] * x + coef(model_quadratic)[3] * x^2, 
      from = min(data$educ), to = max(data$educ), 
      col = "green", lwd = 2, add = TRUE)

# 顯示圖例
legend("topright", legend=c("Linear Model", "Quadratic Model"), col=c("red", "green"), lwd=2)


