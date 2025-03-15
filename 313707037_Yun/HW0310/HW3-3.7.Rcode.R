result2 <- qt(0.99, df  =62)
print(result2)

# 設定數據
bachelor <- seq(0, 50, by = 1)  # 百分比從 0 到 50
intercept <- 11.5163  # 截距
slope <- 1.029    # 斜率

# 計算預測的 INCOME
income <- intercept + slope * bachelor

# 繪製圖形
plot(bachelor, income, type = "l", col = "lightblue", lwd = 2,
     xlab = "BACHELOR (Percentage)",
     ylab = "INCOME (Thousands of Dollars)",
     main = "Estimated Relationship between BACHELOR and INCOME")

# 添加數據點（模擬數據）
#set.seed(42)
#points(bachelor, income + rnorm(length(bachelor), mean = 0, sd = 2), col = "orange", pch = 16)

# 添加迴歸線
abline(a = intercept, b = slope, col = "orange", lwd = 2)

result3 <- qt(0.975,df=49)
print(result3)

result4 <- 2*(1- pt(0.5675, df  =49))
print(result4)

# 設定參數
alpha <- 0.05
df <- 49
critical_value <- qt(1 - alpha/2, df)

# 繪製 t 分佈圖
x <- seq(-4, 4, length = 1000)
y <- dt(x, df)

plot(x, y, type = "l", lwd = 2, col = "lightblue",
     main = "Rejection Region for Two-Tailed Test (α = 0.05)",
     xlab = "t-value", ylab = "Density")

# 填充拒絕區域
abline(v = c(-critical_value, critical_value), col = "red", lty = 2)
polygon(c(x[x <= -critical_value], -critical_value), 
        c(y[x <= -critical_value], 0), col = "red", density = 30)
polygon(c(x[x >= critical_value], critical_value), 
        c(y[x >= critical_value], 0), col = "red", density = 30)

# 標示 t 統計量
abline(v = 0.567, col = "lightgreen", lwd = 2)
text(0.567, 0.05, labels = "t = 0.567", pos = 4, col = "lightgreen")