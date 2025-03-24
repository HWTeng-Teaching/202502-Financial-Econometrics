# 載入 ggplot2 套件
library(ggplot2)

# a.
# # 定義 EXPER 的範圍，從 0 到 30
# EXPER <- 0:30
# 
# # 使用 Model 1 的方程計算 RATING
# RATING <- 64.289 + 0.990 * EXPER
# 
# # 將數據整理成數據框
# data_1 <- data.frame(EXPER = EXPER, RATING = RATING)
# 
# # 使用 ggplot2 繪製圖形
# ggplot(data_1, aes(x = EXPER, y = RATING)) +
#   geom_line(color = "blue", size = 1) +  # 畫線條
#   geom_point(color = "blue", size = 2) +  # 畫點
#   labs(title = "Model 1: RATING vs EXPER (0 to 30 years)",
#        x = "Experience (EXPER in years)",
#        y = "Predicted Rating (RATING)") +
#   theme_minimal()  # 使用簡潔的主題

# b.
# 定義 EXPER 的範圍，從 1 到 30
EXPER <- 1:30

# 使用 Model 2 的方程計算 RATING
RATING <- 39.464 + 15.312 * log(EXPER)

# 將結果整理成數據框
data_2 <- data.frame(EXPER = EXPER, RATING = RATING)

# 使用 ggplot2 繪製圖形
ggplot(data_2, aes(x = EXPER, y = RATING)) +
  geom_line(color = "blue", size = 1) +  # 畫線條
  geom_point(color = "blue", size = 2) +  # 畫點
  labs(title = "Model 2: RATING vs EXPER (0 to 30 years)",
       x = "Experience (EXPER in years)",
       y = "Predicted Rating (RATING)") +
  theme_minimal()  # 使用簡潔的主題

