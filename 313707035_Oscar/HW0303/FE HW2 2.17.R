library(POE5Rdata)
data("collegetown")

# 確保 price 和 sqft 為數值型
collegetown$price <- as.numeric(as.character(collegetown$price))
collegetown$sqft <- as.numeric(as.character(collegetown$sqft))

# (a) 繪製散佈圖
install.packages("ggplot2")
library(ggplot2)

plot(collegetown$sqft, collegetown$price, 
     main = "Scatter Plot of House Price vs House Size", 
     xlab = "House Size (Hundreds of Square Feet)", 
     ylab = "House Price (Thousands of Dollars)", 
     col = "blue", pch = 16)

# (b) 線性回歸模型
lm_linear <- lm(price ~ sqft, data=collegetown)
summary(lm_linear)

# 繪製線性回歸擬合線
ggplot(collegetown, aes(x = sqft, y = price)) +
  geom_point(color = "blue") +
  geom_smooth(method = "lm", color = "red") +
  labs(title = "Linear Regression: House Price vs Size", 
       x = "House Size (Hundreds of Square Feet)", 
       y = "House Price (Thousands of Dollars)") +
  theme_minimal()

# (c) 二次回歸模型
collegetown$sqft2 <- collegetown$sqft^2
lm_quad <- lm(price ~ sqft + sqft2, data=collegetown)
summary(lm_quad)

# 計算邊際效應（額外 100 sqft 對價格的影響）
sqft_2000 <- 20  # 2000 square feet -> 20 in hundreds
marginal_effect_100 <- coef(lm_quad)["sqft"] + 2 * coef(lm_quad)["sqft2"] * sqft_2000
cat("Marginal effect of additional 100 sqft at 2000 sqft:", marginal_effect_100, "\n")

# (d) 繪製二次回歸擬合曲線
ggplot(collegetown, aes(x = sqft, y = price)) +
  geom_point(color = "blue") +
  geom_smooth(method = "lm", formula = y ~ poly(x, 2), color = "red") +
  labs(title = "Quadratic Regression: House Price vs Size", 
       x = "House Size (Hundreds of Square Feet)", 
       y = "House Price (Thousands of Dollars)") +
  theme_minimal()

# (e) 計算價格的彈性 (elasticity) 在 sqft = 2000
elasticity_2000 <- (coef(lm_quad)["sqft"] + 2 * coef(lm_quad)["sqft2"] * sqft_2000) * (sqft_2000 / predict(lm_quad, newdata = data.frame(sqft = sqft_2000, sqft2 = sqft_2000^2)))
cat("Elasticity of price with respect to sqft at 2000 sqft:", elasticity_2000, "\n")

# (f) 殘差分析與異方差檢查
residuals_linear <- resid(lm_linear)
residuals_quad <- resid(lm_quad)

# 繪製殘差 vs sqft
par(mfrow=c(1,2))
plot(collegetown$sqft, residuals_linear, 
     main="Residuals vs SQFT (Linear Model)", 
     xlab="House Size", ylab="Residuals", col="blue", pch=16)
abline(h=0, col="red")

plot(collegetown$sqft, residuals_quad, 
     main="Residuals vs SQFT (Quadratic Model)", 
     xlab="House Size", ylab="Residuals", col="green", pch=16)
abline(h=0, col="red")

# (g) 計算 SSE 並比較
sse_linear <- sum(residuals_linear^2)
sse_quad <- sum(residuals_quad^2)

cat("SSE (Linear Model):", sse_linear, "\n")
cat("SSE (Quadratic Model):", sse_quad, "\n")

if (sse_quad < sse_linear) {
  cat("Quadratic model fits better as it has a lower SSE.\n")
} else {
  cat("Linear model fits better as it has a lower SSE.\n")
}
