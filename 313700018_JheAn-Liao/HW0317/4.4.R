#A.
#Modle 1
# 定義EXPER的範圍（從0到30）
exper <- 0:30

# 根據 Model 1 的方程式 (RATING = 64.289 + 0.90 * EXPER) 計算對應的RATING
rating <- 64.289 + 0.90 * exper

# 繪製折線
plot(exper, rating, 
     type = "l",                # 先畫線
     xlab = "EXPER（年數）",     
     ylab = "RATING",           
     main = "Model 1: Fitted Values (0 ~ 30)") 

# 在折線圖上加上每個點
points(exper, rating, 
       pch = 19,    # 圓點符號
       col = "blue") # 點的顏色

#B.
#Modle 2
# 定義 EXPER 的範圍（從 1 到 30；因為 ln(0) 未定義）
exper <- 1:30

# 根據 Model 2 的方程式：RATING = 39.464 + 15.312 * ln(EXPER)
# 在 R 中，ln(x) 使用 log(x)
rating <- 39.464 + 15.312 * log(exper)

# 繪製折線圖
plot(exper, rating,
     type = "l",               # 先畫線
     xlab = "EXPER（年數）",
     ylab = "RATING",
     main = "Model 2: Fitted Values (EXPER = 1 ~ 30)"
)

# 加上每個預測點
points(exper, rating,
       pch = 19,  # 圓點符號
       col = "blue")

