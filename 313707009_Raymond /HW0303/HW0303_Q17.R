if (!requireNamespace("remotes", quietly = TRUE)) install.packages("remotes")
remotes::install_github("ccolonescu/POE5Rdata", force = TRUE)

library(POE5Rdata)
data("collegetown")
data <- collegetown
# (a) 畫出價格與房子的分布圖
plot(data$sqft, data$price, 
     main = "House Price vs House Size", 
     xlab = "House Size (hundreds of square feet)", 
     ylab = "Price (thousands of dollars)", 
     col = "blue", pch = 16)

# (b) OLS回歸模型
lm_model <- lm(price ~ sqft, data = data)
summary(lm_model)

# 畫出回歸線
abline(lm_model, col = "red", lwd = 2)

# (c) Quadratic 迴歸
quad_model <- lm(price ~ sqft + I(sqft^2), data = data)
summary(quad_model)

# 計算marginal effect
sqft_2000 <- 20  # 2000 平方英尺（以百為單位）
margin_effect <- coef(quad_model)[2] + 2 * coef(quad_model)[3] * sqft_2000
cat("邊際效應為:",margin_effect,"\n")

# (d)繪製Quadratic Regression line 
curve(coef(quad_model)[1] + coef(quad_model)[2] * x + coef(quad_model)[3] * x^2, 
      from = min(data$sqft), to = max(data$sqft), 
      col = "green", lwd = 2, add = TRUE)

# 繪製切線
tangent_slope <- margin_effect
x_2000 <- sqft_2000
y_2000 <- predict(quad_model, newdata = data.frame(sqft = x_2000))
abline(a = y_2000 - tangent_slope * x_2000, b = tangent_slope, col = "purple", lwd = 2, lty = 2)

# (e)計算彈性
price_2000 <- predict(quad_model, newdata = data.frame(sqft = sqft_2000))
elasticity <- (margin_effect * 100 / price_2000)
cat("Quadratic Regression 在2000平方英尺之彈性為",elasticity)

# (f)殘差分析
residuals_lm <- resid(lm_model)
residuals_quad <- resid(quad_model)

# 繪製線性回歸殘差圖
plot(data$sqft, residuals_lm, 
     main = "Residuals of Linear Regression Model", 
     xlab = "House Size (hundreds of square feet)", 
     ylab = "Residuals", 
     col = "red", pch = 16)
abline(h = 0, col = "black", lty = 2)

# 繪製二次回歸殘差圖
plot(data$sqft, residuals_quad, 
     main = "Residuals of Quadratic Regression Model", 
     xlab = "House Size (hundreds of square feet)", 
     ylab = "Residuals", 
     col = "green", pch = 16)
abline(h = 0, col = "black", lty = 2)

# 計算 SSE (g)
sse_lm <- sum(residuals_lm^2)
sse_quad <- sum(residuals_quad^2)
sse_lm
sse_quad

# 比較 SSE，SSE 較小的模型較適合
better_model <- ifelse(sse_lm < sse_quad, "Linear Model", "Quadratic Model")
better_model