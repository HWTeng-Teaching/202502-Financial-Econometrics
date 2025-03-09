url <- "https://www.principlesofeconometrics.com/poe5/data/rdata/collegetown.rdata"
download.file(url, destfile = "collegetown.rdata", mode = "wb")  # 下載檔案

# 1. Load data
load("collegetown.rdata")

str(collegetown)  # 查看結構
head(collegetown)  # 查看前幾筆數據

plot(collegetown$sqft, collegetown$price,
     main = "Scatter Plot of House Price vs. House Size",
     xlab = "House Size (sqft)",
     ylab = "House Price (in thousands)",
     pch = 19,           # solid circle points
     col = "blue",
     cex = 0.5) 

model <- lm(price ~ sqft, data = collegetown)

# 提取迴歸係數
coef_intercept <- coef(model)[1]  # 截距 β1
coef_slope <- coef(model)[2]      # 斜率 β2
summary(model)

abline(model, col = "red", lwd = 2)

# 新增一個變數：SQFT 的平方
collegetown$sqft2 <- collegetown$sqft^2

# 建立二次迴歸模型
model_quad <- lm(price ~ sqft2, data = collegetown)

# 查看迴歸結果摘要
summary(model_quad)
# 抓出α₂（sqft2 的係數）
alpha2 <- coef(model_quad)["sqft2"]
alpha1 <- coef(model_quad)["(Intercept)"]

# 設定 SQFT = 20（2000 sqft）
sqft_value <- 20

price_at_20 <- alpha1 + alpha2 * sqft_value^2
price_at_20
# 計算邊際效果
marginal_effect <- 2 * alpha2 * sqft_value

# 顯示邊際效果
marginal_effect

# 畫散佈圖
plot(collegetown$sqft, collegetown$price,
     main = "房價與房屋大小的二次迴歸",
     xlab = "房屋大小（百平方英呎）",
     ylab = "房價（千元）",
     pch = 19, col = "grey",
     cex = 0.5)

# 建立 sqft 的範圍（用來畫預測曲線）
sqft_range <- seq(min(collegetown$sqft), max(collegetown$sqft), length.out = 100)

# 計算對應的預測價格
predicted_price <- coef(model_quad)["(Intercept)"] + coef(model_quad)["sqft2"] * sqft_range^2

# 加入二次迴歸的預測線
lines(sqft_range, predicted_price, col = "blue", lwd = 2)

# 計算 2000 sqft（20）時的預測價格
f_20 <- coef(model_quad)["(Intercept)"] + coef(model_quad)["sqft2"] * 20^2

# 切線方程式：y = f_20 + 邊際效果 * (x - 20)
tangent_line <- f_20 + marginal_effect * (sqft_range - 20)

# 畫出切線
lines(sqft_range, tangent_line, col = "red", lwd = 2, lty = 2)

# 標示切點
points(20, f_20, pch = 19, col = "darkgreen")

elasticity <- marginal_effect * (sqft_value / price_at_20)

# 輸出彈性
elasticity

residuals <- resid(model)
residuals_quad <- resid(model_quad)

# 殘差圖 for 線性模型
plot(collegetown$sqft, residuals,
     main = "Residuals vs SQFT (Linear Model)",
     xlab = "SQFT",
     ylab = "Residuals",
     pch = 19, col = "blue",
     cex = 0.5)
abline(h = 0, lty = 2, col = "red")

# 殘差圖 for 二次模型
plot(collegetown$sqft, residuals_quad,
     main = "Residuals vs SQFT (Quadratic Model)",
     xlab = "SQFT",
     ylab = "Residuals",
     pch = 19, col = "green",
     cex = 0.5)
abline(h = 0, lty = 2, col = "red")

# SSE for (b) 線性模型
SSE_linear <- sum(residuals(model)^2)
# 或者用 deviance
SSE_linear_alt <- deviance(model)

# SSE for (c) 二次模型
SSE_quad <- sum(residuals(model_quad)^2)
SSE_quad_alt <- deviance(model_quad)

# 看看結果
SSE_linear
SSE_quad

#SSE_linear_alt
#SSE_quad_alt
