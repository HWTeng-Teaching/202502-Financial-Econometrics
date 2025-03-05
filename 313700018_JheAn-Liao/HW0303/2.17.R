# 載入必要套件
if (!require(tidyverse)) install.packages("tidyverse")
library(tidyverse)

# 下載並載入 collegetown 資料集
temp_file <- tempfile(fileext = ".rdata")
download.file(url = "https://www.principlesofeconometrics.com/poe5/data/rdata/collegetown.rdata",
              destfile = temp_file, mode = "wb")
load(temp_file)

# 確保數據載入成功
colnames(collegetown)  # 查看變數名稱，確認是否為 sqft 和 price
collegetown <- as.data.frame(collegetown)  # 轉換為 data.frame

# A.B.估計線性回歸模型
model <- lm(price ~ sqft, data = collegetown)

# 提取迴歸係數
coef_intercept <- coef(model)[1]  # 截距 β1
coef_slope <- coef(model)[2]      # 斜率 β2

# 格式化成 y = ax + b 形式，並顯示到圖上
regression_eq <- paste0("price = ", round(coef_intercept, 2), " + ", round(coef_slope, 2), " * sqft")

# 繪製散點圖與回歸線，並標示公式
ggplot(collegetown, aes(x = sqft, y = price)) +
  geom_point(color = "blue", alpha = 0.6) +  # 散點圖
  geom_smooth(method = "lm", se = FALSE, color = "red", size = 1) +  # 迴歸線
  labs(
    title = "Scatter Plot of Price and Size with Regression Line",
    x = "House Size (hundreds of square feet)",
    y = "Price (thousands of dollars)"
  ) +
  annotate("text", x = max(collegetown$sqft) * 0.7, y = max(collegetown$price) * 0.9, 
           label = regression_eq, size = 5, color = "black", hjust = 0)

### C.D.建立平方估計式

# 建立平方項
collegetown$sqft2 <- collegetown$sqft^2

# 估計純二次回歸模型（不包含一次項）
quad_model <- lm(price ~ sqft2, data = collegetown)

# 取得回歸係數
coef_intercept <- coef(quad_model)[1]  # 截距 α1
coef_sqft2 <- coef(quad_model)[2]      # 平方項 α2

# 計算 X = 20 的價格
x0 <- 20
y0 <- coef_intercept + coef_sqft2 * x0^2  # 代入二次方程

# 計算 X = 20 的切線斜率
slope_tangent <- 2 * coef_sqft2 * x0

# 格式化回歸公式為 y = α1 + α2 * x^2
regression_eq <- paste0("price = ", 
                        round(coef_intercept, 2), " + ", 
                        round(coef_sqft2, 5), " * sqft²")

# 繪製散點圖與純二次回歸曲線，並標示公式 + 切線
ggplot(collegetown, aes(x = sqft, y = price)) +
  geom_point(color = "blue", alpha = 0.6) +  # 散點圖
  geom_smooth(method = "lm", formula = y ~ I(x^2), se = FALSE, color = "red", size = 1) +  # 純二次回歸線
  geom_abline(intercept = y0 - slope_tangent * x0, slope = slope_tangent, color = "blue", linetype = "dashed", size = 1) +  # 切線
  geom_point(aes(x = x0, y = y0), color = "black", size = 3) +  # X=20 的點
  labs(
    title = "Quadratic Regression: Price vs. Size (with Tangent at X=20)",
    x = "House Size (hundreds of square feet)",
    y = "Price (thousands of dollars)"
  ) +
  annotate("text", x = max(collegetown$sqft) * 0.6, y = max(collegetown$price) * 0.9, 
           label = regression_eq, size = 5, color = "black", hjust = 0)

# E.繪製殘差圖 (Linear Regression)
p1 <- ggplot(collegetown, aes(x = sqft, y = linear_resid)) +
  geom_point(color = "blue", alpha = 0.6) +  
  geom_hline(yintercept = 0, color = "red", linetype = "dashed") +  
  labs(
    title = "Residual Plot for Linear Regression (b)",
    x = "House Size (hundreds of square feet)",
    y = "Residuals"
  )

# 繪製殘差圖 (Linear Regression)
p1 <- ggplot(collegetown, aes(x = sqft, y = linear_resid)) +
  geom_point(color = "blue", alpha = 0.6) +  
  geom_hline(yintercept = 0, color = "red", linetype = "dashed") +  
  labs(
    title = "Residual Plot for Linear Regression (b)",
    x = "House Size (hundreds of square feet)",
    y = "Residuals"
  )

# 繪製殘差圖 (Quadratic Regression)
p2 <- ggplot(collegetown, aes(x = sqft, y = quad_resid)) +
  geom_point(color = "blue", alpha = 0.6) +  
  geom_hline(yintercept = 0, color = "red", linetype = "dashed") +  
  labs(
    title = "Residual Plot for Quadratic Regression (c)",
    x = "House Size (hundreds of square feet)",
    y = "Residuals"
  )

# 🌟 載入必要套件
library(tidyverse)

# 🔗 確保數據載入成功並轉換為 data.frame
collegetown <- as.data.frame(collegetown)
colnames(collegetown)  # 確認變數名稱

# F.
# 🎯 創建平方項
collegetown <- collegetown %>% mutate(sqft2 = sqft^2)

# 📊 估計 (b) 線性回歸模型
linear_model <- lm(price ~ sqft, data = collegetown)

# 📊 估計 (c) 純二次回歸模型
quad_model <- lm(price ~ sqft2, data = collegetown)

# ✅ 計算殘差並存入資料框
collegetown <- collegetown %>%
  mutate(
    linear_resid = price - predict(linear_model),  # 線性回歸殘差
    quad_resid = price - predict(quad_model)      # 二次回歸殘差
  )

# 📉 繪製殘差圖 - 線性回歸
p1 <- ggplot(collegetown, aes(x = sqft, y = linear_resid)) +
  geom_point(color = "blue", alpha = 0.6) +  
  geom_hline(yintercept = 0, color = "red", linetype = "dashed") +  
  labs(
    title = "Residual Plot for Linear Regression (b)",
    x = "House Size (hundreds of square feet)",
    y = "Residuals"
  )

# 📉 繪製殘差圖 - 二次回歸
p2 <- ggplot(collegetown, aes(x = sqft, y = quad_resid)) +
  geom_point(color = "blue", alpha = 0.6) +  
  geom_hline(yintercept = 0, color = "red", linetype = "dashed") +  
  labs(
    title = "Residual Plot for Quadratic Regression (c)",
    x = "House Size (hundreds of square feet)",
    y = "Residuals"
  )

# 🖼️ 顯示圖形
print(p1)
print(p2)
# G.
# 計算兩個模型的殘差（Residuals）
linear_resid <- collegetown$price - predict(linear_model)
quad_resid <- collegetown$price - predict(quad_model)

# 計算 SSE（殘差平方和）
SSE_linear <- sum(linear_resid^2)
SSE_quad <- sum(quad_resid^2)

# 顯示 SSE 結果
cat("Linear  SSE:", SSE_linear, "\n")
cat("Quadratic SSE:", SSE_quad, "\n")

# 顯示圖形
print(p1)
print(p2)
