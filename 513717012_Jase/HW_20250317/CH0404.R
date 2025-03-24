#a

# 建立 EXPER 從 0 到 30 的向量
EXPER <- 0:30

# 計算對應的預測 RATING 值
RATING_hat <- 64.289 + 0.990 * EXPER

# 畫圖
plot(EXPER, RATING_hat, type = "l", col = "blue", lwd = 2,
     xlab = "EXPER", ylab = "RATTING",
     main = "Model 1")
grid()

#b

# 建立 EXPER 從 1 到 30 的向量
EXPER <- 1:30

# 計算對應的預測 RATING 值（使用自然對數 ln）
RATING_hat <- 39.464 + 15.312 * log(EXPER)

# 畫圖
plot(EXPER, RATING_hat, type = "l", col = "RED", lwd = 2,
     xlab = "EXPER", ylab = "RATTING",
     main = "Model 2")
grid()
