# Q5.23
# (b)
# import library
library(POE5Rdata)

# dataset
data(cocaine)

# calc linear regression
model <- lm(price ~ quant + qual + trend, data = cocaine)
summary(model)

# (e)
# 參數設定
t_val <- 0.572         # t 值
df <- 52               # 自由度
alpha <- 0.05          # 顯著水準

# t 分布範圍
x <- seq(-4, 4, length = 1000)
y <- dt(x, df)

# 繪圖
plot(x, y, type = "l", lwd = 2, col = "black",
     main = "Right-tailed t-test for β3 (Quality)",
     xlab = "t value", ylab = "Density")

# 陰影區域：右尾 p-value
x_shade <- seq(t_val, 4, length = 500)
y_shade <- dt(x_shade, df)
polygon(c(t_val, x_shade, 4), c(0, y_shade, 0), col = "red", border = NA)

# 顯示 t 值
abline(v = t_val, col = "red", lty = 2)
text(t_val + 0.1, 0.1, paste("t =", round(t_val, 3)), col = "black")

# 註解
legend("topright", legend = c("t-distribution", "p-value area", "t observed"),
       col = c("black", "red", "red"), lty = c(1, NA, 2), lwd = c(2, NA, 1), pch = c(NA, 15, NA),
       pt.cex = 1.5, bty = "n")