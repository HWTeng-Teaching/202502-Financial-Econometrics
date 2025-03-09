#(a)
library(ggplot2)

ggplot(data = collegetown, aes(x = sqft, y = price)) +
  geom_point(color = "blue", alpha = 0.6, size = 2) +  
  labs(title = "Scatter Plot of House price and Size",
       x = "House Size (Hundreds of Square Feet)",
       y = "House Price (Thousands of $)") +
  theme_minimal(base_size = 12) +  # 設定基礎字型大小
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))


#(b)
# 估計線性回歸模型
model <- lm(price ~ sqft, data = collegetown)

# 顯示回歸結果
summary(model)

ggplot(data = collegetown, aes(x = sqft, y = price)) +
  geom_point(color = "blue", alpha = 0.6, size = 2) +  # 繪製散點
  geom_abline(intercept = coef(model)[1], slope = coef(model)[2], color = "red", linetype = "dashed", size = 1.2) + # 繪製回歸線
  labs(title = "House Price vs. House Size",
       x = "House Size (Hundreds of Square Feet)",
       y = "House Price (Thousands of Dollars)") +
  theme_minimal(base_size = 12) +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))

#(c)

# 估計二次回歸模型
model_quad <- lm(price ~ I(sqft^2), data = collegetown)
summary(model_quad)

# 計算邊際效果：當房屋面積為 2000 平方英尺（20 單位）時，額外增加 100 平方英尺對價格的影響
alpha2 <- coef(model_quad)["I(sqft^2)"]
sqft_value <- 20  # 2000 平方英尺（單位 100 平方英尺）
marginal_effect <- 2 * alpha2 * sqft_value
print(marginal_effect)  # 顯示邊際效果

# 繪製二次回歸曲線
ggplot(data = collegetown, aes(x = sqft, y = price)) +
  geom_point(color = "blue", alpha = 0.6, size = 2) +  # 繪製散點圖
  geom_smooth(method = "lm", formula = y ~ poly(x, 2), color = "red", linetype = "dashed", size = 1.2) +  # 二次回歸曲線
  labs(title = "Quadratic Regression: House Price vs. House Size",
       x = "House Size (Hundreds of Square Feet)",
       y = "House Price (Thousands of Dollars)") +
  theme_minimal(base_size = 12) +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))

#(d)

# **擴展切線範圍**（從 5 到 35，代表 500 - 3500 平方英尺）
tangent_df <- data.frame(
  sqft = seq(5, 35, length.out = 300),  # **擴大 X 軸範圍**
  price = price_2000sqft + tangent_slope * (seq(5, 35, length.out = 300) - sqft_value) # **切線方程**
)

# 繪製二次回歸曲線 + **延長的切線**
ggplot(data = collegetown, aes(x = sqft, y = price)) +
  geom_point(color = "blue", alpha = 0.6, size = 2) +  # 散點圖
  geom_smooth(method = "lm", formula = y ~ poly(x, 2), color = "red", linetype = "solid", size = 1.2) +  # 二次回歸曲線
  geom_line(data = tangent_df, aes(x = sqft, y = price), color = "green", linetype = "dashed", size = 1.2) +  # **更長的切線**
  geom_point(aes(x = sqft_value, y = price_2000sqft), color = "black", size = 3) +  # 切線的切點
  labs(title = "Quadratic Regression with Extended Tangent Line at 2000 sqft",
       x = "House Size (Hundreds of Square Feet)",
       y = "House Price (Thousands of Dollars)") +
  theme_minimal(base_size = 10) +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))

load("collegetown.rdata")


#(f)
install.packages("gridExtra")

# 估計線性回歸模型（b）：price ~ sqft
model_linear <- lm(price ~ sqft, data = collegetown)

# 估計二次回歸模型（c）：price ~ sqft^2
model_quad <- lm(price ~ I(sqft^2), data = collegetown)

# 計算殘差
collegetown$residuals_linear <- resid(model_linear)
collegetown$residuals_quad <- resid(model_quad)

# 繪製殘差圖
p1 <- ggplot(collegetown, aes(x = sqft, y = residuals_linear)) +
  geom_point(color = "blue", alpha = 0.6) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  labs(title = "Residual Plot for Linear Regression",
       x = "House Size (Hundreds of Square Feet)",
       y = "Residuals") +
  theme_minimal()

p2 <- ggplot(collegetown, aes(x = sqft, y = residuals_quad)) +
  geom_point(color = "green", alpha = 0.6) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  labs(title = "Residual Plot for Quadratic Regression",
       x = "House Size (Hundreds of Square Feet)",
       y = "Residuals") +
  theme_minimal()
# 顯示圖表
library(gridExtra)
grid.arrange(p1, p2, ncol = 2)

