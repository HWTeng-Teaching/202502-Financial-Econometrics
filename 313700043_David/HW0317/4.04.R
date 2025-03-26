rm(list = ls())  

#4.04 a b
# 設定參數
EXPER1 <- seq(0, 30, by = 1)  # Model 1 的 EXPER 範圍
EXPER2 <- seq(1, 30, by = 1)  # Model 2 的 EXPER 範圍（從 1 開始）

# 計算擬合值
RATING1 <- 64.289 + 0.990 * EXPER1  # Model 1
RATING2 <- 39.464 + 15.312 * log(EXPER2)  # Model 2

# 繪製 Model 1（線性模型）
plot(EXPER1, RATING1, type = "l", col = "blue", lwd = 2, 
     xlab = "Years of Experience", ylab = "Predicted Rating",
     main = "Fitted Values from Model 1 (Linear)")
points(EXPER1, RATING1, col = "black", pch = 16)

# 繪製 Model 2（對數模型）
plot(EXPER2, RATING2, type = "l", col = "red", lwd = 2, 
     xlab = "Years of Experience", ylab = "Predicted Rating",
     main = "Fitted Values from Model 2 (Log)" )
points(EXPER2, RATING2, col = "black", pch = 16)

