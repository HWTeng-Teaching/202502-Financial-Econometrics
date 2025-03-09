# 清除環境變數（可選）
rm(list=ls())

library(POE5Rdata)
library(dplyr)
library(ggplot2)

data("collegetown") 

# 建立線性回歸模型
mod1 <- lm(price ~ sqft, data = collegetown)
summary(mod1)

# 提取模型係數
cat(b1 <- coef(mod1)[[1]])
cat(b2 <- coef(mod1)[[2]])

p <- ggplot(collegetown, aes(x = sqft, y = price)) +
  geom_point(color = "blue", alpha = 0.7) +  # 使用透明度更好地顯示數據分布
  geom_smooth(method = "lm", color = "red") +  # 添加回歸線
  labs(
    title = "House Price vs. House Size",
    x = "House Size (sq ft)",
    y = "House Price ($)"
  ) +
  theme_minimal()

# 顯示圖形
print(p)

#-----------------------------------------------------(a)、(b)

# 建立二次回歸模型 PRICE = α₁ + α₂SQFT² + e
mod_quad <- lm(price ~ I(sqft^2), data = collegetown)
summary(mod_quad)

# 提取模型係數
alpha1 <- coef(mod_quad)[1]  # 截距
alpha2 <- coef(mod_quad)[2]  # SQFT²的係數

# 印出模型係數
cat("α₁ =", round(alpha1, 4), "\n")
cat("α₂ =", round(alpha2, 6), "\n")

# 計算邊際效應（已轉換為百平方英尺）
sqft_2000 <- 20  # 2000平方英尺 = 20百平方英尺
marginal_effect <- 2 * alpha2 * sqft_2000  # 每百平方英尺的邊際效應

# 印出邊際效應結果
cat("\n邊際效應計算結果:\n")
cat("在2000平方英尺的房屋中，增加100平方英尺的邊際效應 =", 
    round(marginal_effect, 4), "千美元 = $", 
    format(round(marginal_effect * 1000, 1), nsmall = 1), "\n")

#---------------------------------------------------------------(c)

# 繪製擬合曲線和切線
# 設置更美觀的繪圖主題
par(mar = c(5, 5, 4, 2), bg = "white", las = 1, 
    cex.axis = 0.9, cex.lab = 1, cex.main = 1.2)

# 計算預測值和切線
sqft_range <- seq(min(collegetown$sqft), max(collegetown$sqft), length.out = 200)
price_pred <- predict(mod_quad, newdata = data.frame(sqft = sqft_range))
price_at_2000 <- alpha1 + alpha2 * sqft_2000^2
tangent_line <- price_at_2000 + marginal_effect * (sqft_range - sqft_2000)

# 繪製散點圖和模型
plot(collegetown$sqft, collegetown$price, 
     xlab = "面積 (百平方英尺)", 
     ylab = "價格 (千美元)",
     main = "房屋價格與面積的二次回歸模型",
     pch = 19, col = adjustcolor("gray30", alpha.f = 0.6), cex = 0.8,
     xlim = range(collegetown$sqft) * c(0.95, 1.05),
     ylim = range(collegetown$price) * c(0.95, 1.05))

# 添加擬合曲線
lines(sqft_range, price_pred, col = "blue", lwd = 2.5)

# 添加切線
lines(sqft_range, tangent_line, col = "red", lwd = 2, lty = 2)

# 添加圖例
legend("topleft", 
       legend = c( "二次回歸曲線", "2000平方英尺處的切線"),
       col = c("blue", "red"),
       pch = c(19, NA, NA, 19),
       lty = c(NA, 1, 2, NA),
       lwd = c(NA, 2.5, 2, NA),
       pt.cex = c(0.8, NA, NA, 1.5),
       bg = "white",
       box.lty = 1,
       cex = 0.85)

#------------------------------------------------------------(d))

# 彈性公式: (∂PRICE/∂SQFT) * (SQFT/PRICE)

# 步驟1: 轉換為百平方英尺
sqft_2000 <- 2000/100  # 20百平方英尺

# 步驟2: 計算在2000平方英尺處的預測價格
price_at_2000 <- alpha1 + alpha2 * sqft_2000^2

# 步驟3: 計算邊際效應 (∂PRICE/∂SQFT)
# 在二次模型中，邊際效應 = 2*α₂*SQFT
marginal_effect <- 2 * alpha2 * sqft_2000

# 步驟4: 計算彈性 = (∂PRICE/∂SQFT) * (SQFT/PRICE)
elasticity <- marginal_effect * (sqft_2000 / price_at_2000)

# 印出彈性結果
cat("\n彈性計算結果:\n")
cat("在2000平方英尺的房屋中，房價對面積的彈性 =", round(elasticity, 4), "\n")
cat("這表示面積增加1%時，房價預計增加約", round(elasticity * 100, 2), "%\n")

#------------------------------------------------------------(e)

# 計算兩個模型的殘差
residuals_mod1 <- resid(mod1)
residuals_mod_quad <- resid(mod_quad)


# 線性模型殘差圖
plot(collegetown$sqft, residuals_mod1, 
     main="線性模型殘差 vs SQFT", 
     xlab="SQFT (平方英尺)", 
     ylab="殘差",
     pch=16)

# 二次模型殘差圖
plot(collegetown$sqft, residuals_mod_quad, 
     main="二次模型殘差 vs SQFT", 
     xlab="SQFT (平方英尺)", 
     ylab="殘差",
     pch=16)

#--------------------------------------------------------------(f)

# 計算線性模型 mod1 的 SSE
sse_mod1 <- sum(residuals_mod1^2)

# 計算二次模型 mod_quad 的 SSE
sse_mod_quad <- sum(residuals_mod_quad^2)

# 印出兩個模型的 SSE
cat("線性模型 (mod1) 的 SSE:", sse_mod1, "\n")
cat("二次模型 (mod_quad) 的 SSE:", sse_mod_quad, "\n")
#--------------------------------------------------------------(g)




