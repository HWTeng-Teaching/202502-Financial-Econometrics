# 清除工作區
rm(list = ls())

##############################
# 載入資料
##############################
# 假設資料檔案 "collegetown.csv" 存在於工作目錄中，並包含欄位 PRICE (以千美元為單位) 與 SQFT (以百平方英尺為單位)
collegetown <- read.csv("collegetown.csv", header = TRUE)
# 檢查資料結構
str(collegetown)

##############################
# (a) 繪製房價與房屋面積的散佈圖
##############################
plot(collegetown$SQFT, collegetown$PRICE,
     xlab = "房屋面積 (百平方英尺)",
     ylab = "房價 (千美元)",
     main = "房價與房屋面積散佈圖",
     pch = 16, col = "blue")

##############################
# (b) 估計線性迴歸模型 PRICE = β₁ + β₂ SQFT + e
##############################
lm_linear <- lm(PRICE ~ SQFT, data = collegetown)
summary(lm_linear)

# 將線性迴歸的擬合線加入散佈圖
abline(lm_linear, col = "red", lwd = 2)
legend("topleft", legend = c("資料點", "線性迴歸擬合線"),
       col = c("blue", "red"), pch = c(16, NA), lty = c(NA, 1), lwd = c(NA, 2))

# 註解：
# β₂ 的估計值表示房價（千美元）每增加一個單位的 SQFT（即100平方英尺）所增加的平均金額。
# β₁ 為當 SQFT = 0 時的預測房價。

##############################
# (c) 估計二次迴歸模型 PRICE = α₁ + α₂ SQFT² + e
##############################
lm_quad <- lm(PRICE ~ I(SQFT^2), data = collegetown)
summary(lm_quad)

# 計算在 SQFT = 20 (即2000平方英尺，因為 20×100 = 2000) 下，每增加100平方英尺的邊際效應
# 邊際效應: dPRICE/dSQFT = 2 * α₂ * SQFT
SQFT_target <- 20
alpha2 <- coef(lm_quad)["I(SQFT^2)"]
marginal_effect <- 2 * alpha2 * SQFT_target
cat("在2000平方英尺房屋下，每增加100平方英尺的邊際效應 =",
    marginal_effect, "（千美元）\n\n")

##############################
# (d) 繪製二次迴歸擬合曲線，並標示出2000平方英尺房屋處的切線
##############################
# 產生 SQFT 的連續序列
SQFT_seq <- seq(min(collegetown$SQFT), max(collegetown$SQFT), length.out = 300)
pred_quad <- predict(lm_quad, newdata = data.frame(SQFT = SQFT_seq))

# 繪製散佈圖與二次擬合曲線
plot(collegetown$SQFT, collegetown$PRICE,
     xlab = "房屋面積 (百平方英尺)",
     ylab = "房價 (千美元)",
     main = "二次迴歸擬合曲線與2000平方英尺切線",
     pch = 16, col = "blue")
lines(SQFT_seq, pred_quad, col = "red", lwd = 2)

# 計算在 SQFT_target (20) 處的預測房價
price_target <- predict(lm_quad, newdata = data.frame(SQFT = SQFT_target))
# 切線斜率：dPRICE/dSQFT = 2 * α₂ * SQFT_target
tangent_slope <- 2 * alpha2 * SQFT_target
# 定義切線函數： y - price_target = tangent_slope*(SQFT - SQFT_target)
tangent_line <- price_target + tangent_slope * (SQFT_seq - SQFT_target)
lines(SQFT_seq, tangent_line, col = "green", lwd = 2, lty = 2)
legend("topleft", legend = c("資料點", "二次迴歸擬合曲線", "2000平方英尺切線"),
       col = c("blue", "red", "green"), pch = c(16, NA, NA),
       lty = c(NA, 1, 2), lwd = c(NA, 2, 2))

##############################
# (e) 計算在2000平方英尺房屋下房價相對於面積的彈性
##############################
# 彈性 = (dPRICE/dSQFT) * (SQFT / PRICE) evaluated at SQFT_target
elasticity <- tangent_slope * (SQFT_target / price_target)
cat("在2000平方英尺房屋下，房價相對於面積的彈性 =", elasticity, "\n\n")

##############################
# (f) 對(b)與(c)模型計算最小平方法殘差並作圖
##############################
# 線性模型殘差
res_linear <- residuals(lm_linear)
# 二次模型殘差
res_quad <- residuals(lm_quad)

# 以並列圖方式繪製
par(mfrow = c(1, 2))
plot(collegetown$SQFT, res_linear,
     xlab = "房屋面積 (百平方英尺)",
     ylab = "殘差",
     main = "線性模型殘差圖",
     pch = 16, col = "purple")
abline(h = 0, lty = 2)
plot(collegetown$SQFT, res_quad,
     xlab = "房屋面積 (百平方英尺)",
     ylab = "殘差",
     main = "二次模型殘差圖",
     pch = 16, col = "orange")
abline(h = 0, lty = 2)
par(mfrow = c(1, 1))
# 檢查殘差圖是否有系統性模式或異方差性問題

##############################
# (g) 比較(b)與(c)模型的殘差平方和 (SSE)
##############################
SSE_linear <- sum(res_linear^2)
SSE_quad <- sum(res_quad^2)
cat("線性模型 SSE =", SSE_linear, "\n")
cat("二次模型 SSE =", SSE_quad, "\n")
# 較低的 SSE 代表模型對資料的擬合效果較好
