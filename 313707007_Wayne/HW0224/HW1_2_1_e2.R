# 建立資料
x <- c(3, 2, 1, -1, 0)
y <- c(4, 2, 3, 1, 0)

# 依前面結果，假設已估計的回歸係數為：
b1 <- 1.2  # 截距
b2 <- 0.8  # 斜率

# 繪製散佈圖
plot(
  x, y,
  main = "Scatter Plot with Fitted Regression Line",
  xlab = "x", 
  ylab = "y",
  pch = 19,       # 數據點形狀
  col = "red",    # 數據點顏色
  xlim = c(-2, 4), # x軸範圍，可依需要調整
  ylim = c(-1, 5)  # y軸範圍，可依需要調整
)

# 加入回歸直線
abline(a = b1, b = b2, col = "blue", lwd = 2)

# (可選) 在右上角加上圖例
legend(
  "topright",
  legend = c("Data Points", "Fitted Regression Line"),
  col = c("red", "blue"),
  pch = c(19, NA),
  lty = c(NA, 1),
  lwd = 2
)
