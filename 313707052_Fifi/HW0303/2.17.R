# 載入數據
library(POE5Rdata)
data("collegetown", package = "POE5Rdata")

# 檢查變數名稱
names(collegetown)

#a.

library(ggplot2)

ggplot(collegetown, aes(x = sqft, y = price)) +
  geom_point(color = "blue", alpha = 0.6) +
  labs(title = "Scatter Plot of House Price vs. House Size",
       x = "House Size (hundreds of square feet)",
       y = "Price (thousands of dollars)") +
  theme_minimal()

#b.

# 建立線性回歸模型
linear_model <- lm(price ~ sqft, data = collegetown)

# 顯示回歸結果
summary(linear_model)

# 繪製散點圖與回歸線
ggplot(collegetown, aes(x = sqft, y = price)) +
  geom_point(color = "blue", alpha = 0.6) +
  geom_smooth(method = "lm", color = "red", se = FALSE) +
  labs(title = "Linear Regression: House Price vs. House Size",
       x = "House Size (hundreds of square feet)",
       y = "Price (thousands of dollars)") +
  theme_minimal()

#c.

# 建立二次回歸模型
quad_model <- lm(price ~ I(sqft^2), data = collegetown)

# 顯示回歸結果
summary(quad_model)

# 計算 2000 平方英尺房屋的邊際影響
sqft_2000 <- 20  # 2000 平方英尺 = 20 * 100
marginal_effect <- 2 * coef(quad_model)[2] * sqft_2000
print(marginal_effect)

#d.

library(ggplot2)

# 擬合二次回歸模型
quad_model <- lm(price ~ poly(sqft, 2, raw = TRUE), data = collegetown)

# 2000 平方英尺 (20 * 100)
sqft_2000 <- 20 

# 計算 2000 平方英尺處的斜率
coef_quad <- coef(quad_model)
slope_2000 <- coef_quad[2] + 2 * coef_quad[3] * sqft_2000

# 計算 2000 平方英尺的預測價格
predicted_price_2000 <- predict(quad_model, newdata = data.frame(sqft = sqft_2000))

# 計算切線的截距
intercept_2000 <- predicted_price_2000 - slope_2000 * sqft_2000

# 原始圖 + 切線
ggplot(collegetown, aes(x = sqft, y = price)) +
  geom_point(color = "blue", alpha = 0.6) +  # 散點圖
  stat_smooth(method = "lm", formula = y ~ poly(x, 2, raw = TRUE), color = "red", se = FALSE) +  # 二次回歸曲線
  geom_abline(slope = slope_2000, intercept = intercept_2000, color = "green", linetype = "dashed", size = 1) +  # 切線
  labs(title = "Quadratic Regression with Tangent at 2000 sqft",
       x = "House Size (hundreds of square feet)",
       y = "Price (thousands of dollars)") +
  theme_minimal()

#f.

# 計算殘差
linear_residuals <- resid(linear_model)
quad_residuals <- resid(quad_model)

# 線性回歸殘差圖
ggplot(collegetown, aes(x = sqft, y = linear_residuals)) +
  geom_point(color = "blue", alpha = 0.6) +
  geom_hline(yintercept = 0, linetype = "dashed") +  # 添加 y=0 的虛線
  labs(title = "Residuals of Linear Regression",
       x = "House Size (hundreds of square feet)",
       y = "Residuals") +
  theme_minimal()

# 二次回歸殘差圖
ggplot(collegetown, aes(x = sqft, y = quad_residuals)) +
  geom_point(color = "red", alpha = 0.6) +
  geom_hline(yintercept = 0, linetype = "dashed") +  # 添加 y=0 的虛線
  labs(title = "Residuals of Quadratic Regression",
       x = "House Size (hundreds of square feet)",
       y = "Residuals") +
  theme_minimal()

#g.

# 計算 SSE
sse_linear <- sum(linear_residuals^2)
sse_quad <- sum(quad_residuals^2)

print(paste("SSE of linear model:", sse_linear))
print(paste("SSE of quadratic model:", sse_quad))

