rm(list = ls())

# 🔗 下載並載入 motel 資料集
temp_file <- tempfile(fileext = ".rdata")
download.file(url = "https://www.principlesofeconometrics.com/poe5/data/rdata/wa_wheat.rdata",
              destfile = temp_file, mode = "wb")
load(temp_file)
head(wa_wheat)
wa_wheat$time <- as.numeric(wa_wheat$time)

#A.
YIELD <- wa_wheat$northampton
TIME  <- wa_wheat$time
#linear model
mod1 <- lm(YIELD ~ TIME)
#Linear Log Model
mod2 <- lm(YIELD ~ log(TIME))
#Quadratic Model
mod3 <- lm(YIELD ~ I(TIME^2))
#Log Linear Model
mod4 <- lm(log(YIELD) ~ TIME)

# 為了讓繪圖順序正確，依 time 排序
ord <- order(TIME)
# 設定畫布：4列2欄
par(mfrow = c(4, 2), mar = c(4, 4, 2, 1))
## Model 1: YIELD ~ TIME
plot(TIME[ord], YIELD[ord],
     main = "Model 1: YIELD ~ TIME",
     xlab = "TIME", ylab = "YIELD",
     pch = 16)
abline(mod1, col = "blue", lwd = 2)

# 設定文字標示的位置 (依據資料範圍做調整)
x_loc <- min(TIME[ord]) + 0.05 * (max(TIME[ord]) - min(TIME[ord]))
y_loc <- max(YIELD[ord]) - 0.05 * (max(YIELD[ord]) - min(YIELD[ord]))
# 標示迴歸方程式
text(x_loc, y_loc,
     paste0("Y = ", round(coef(mod1)[1], 3),
            " + ", round(coef(mod1)[2], 3), " * TIME"),
     adj = c(0, 1), cex = 0.8)
# 再標示 R squared，將 y 座標稍微往下移動
text(x_loc, y_loc - 0.15*(max(YIELD[ord]) - min(YIELD[ord])),
     paste0("R² = ", round(summary(mod1)$r.squared, 3)),
     adj = c(0, 1), cex = 0.8)

# 殘差圖
plot(TIME, resid(mod1),
     main = "Model 1 Residuals",
     xlab = "TIME", ylab = "Residuals",
     pch = 16)
abline(h = 0, col = "red", lty = 2, lwd = 2)

## Model 2: YIELD ~ log(TIME)
plot(TIME[ord], YIELD[ord],
     main = "Model 2: YIELD ~ log(TIME)",
     xlab = "TIME", ylab = "YIELD",
     pch = 16)
lines(TIME[ord], fitted(mod2)[ord], col = "blue", lwd = 2)

x_loc <- min(TIME[ord]) + 0.05 * (max(TIME[ord]) - min(TIME[ord]))
y_loc <- max(YIELD[ord]) - 0.05 * (max(YIELD[ord]) - min(YIELD[ord]))
text(x_loc, y_loc,
     paste0("Y = ", round(coef(mod2)[1], 3),
            " + ", round(coef(mod2)[2], 3), " * log(TIME)"),
     adj = c(0, 1), cex = 0.8)
text(x_loc, y_loc - 0.15*(max(YIELD[ord]) - min(YIELD[ord])),
     paste0("R² = ", round(summary(mod2)$r.squared, 3)),
     adj = c(0, 1), cex = 0.8)

plot(TIME, resid(mod2),
     main = "Model 2 Residuals",
     xlab = "TIME", ylab = "Residuals",
     pch = 16)
abline(h = 0, col = "red", lty = 2, lwd = 2)

## Model 3: YIELD ~ TIME^2
plot(TIME[ord], YIELD[ord],
     main = "Model 3: YIELD ~ TIME^2",
     xlab = "TIME", ylab = "YIELD",
     pch = 16)
lines(TIME[ord], fitted(mod3)[ord], col = "blue", lwd = 2)

x_loc <- min(TIME[ord]) + 0.05 * (max(TIME[ord]) - min(TIME[ord]))
y_loc <- max(YIELD[ord]) - 0.05 * (max(YIELD[ord]) - min(YIELD[ord]))
text(x_loc, y_loc,
     paste0("Y = ", round(coef(mod3)[1], 3),
            " + ", round(coef(mod3)[2], 5), " * TIME²"),
     adj = c(0, 1), cex = 0.8)
text(x_loc, y_loc - 0.15*(max(YIELD[ord]) - min(YIELD[ord])),
     paste0("R² = ", round(summary(mod3)$r.squared, 3)),
     adj = c(0, 1), cex = 0.8)

plot(TIME, resid(mod3),
     main = "Model 3 Residuals",
     xlab = "TIME", ylab = "Residuals",
     pch = 16)
abline(h = 0, col = "red", lty = 2, lwd = 2)

## Model 4: log(YIELD) ~ TIME
plot(TIME[ord], YIELD[ord],
     main = "Model 4: log(YIELD) ~ TIME",
     xlab = "TIME", ylab = "YIELD",
     pch = 16)
lines(TIME[ord], exp(fitted(mod4))[ord], col = "blue", lwd = 2)

x_loc <- min(TIME[ord]) + 0.05 * (max(TIME[ord]) - min(TIME[ord]))
y_loc <- max(YIELD[ord]) - 0.05 * (max(YIELD[ord]) - min(YIELD[ord]))
text(x_loc, y_loc,
     paste0("ln(Y) = ", round(coef(mod4)[1], 3),
            " + ", round(coef(mod4)[2], 3), " * TIME"),
     adj = c(0, 1), cex = 0.8)
text(x_loc, y_loc - 0.15*(max(YIELD[ord]) - min(YIELD[ord])),
     paste0("R² = ", round(summary(mod4)$r.squared, 3)),
     adj = c(0, 1), cex = 0.8)

plot(TIME, resid(mod4),
     main = "Model 4 Residuals",
     xlab = "TIME", ylab = "Residuals",
     pch = 16)
abline(h = 0, col = "red", lty = 2, lwd = 2)

#Normality Test
# 假設前面已建立四個模型 mod1, mod2, mod3, mod4

# 1. 使用 Shapiro-Wilk test 檢定各模型殘差常態性
shapiro_mod1 <- shapiro.test(resid(mod1))
shapiro_mod2 <- shapiro.test(resid(mod2))
shapiro_mod3 <- shapiro.test(resid(mod3))
shapiro_mod4 <- shapiro.test(resid(mod4))

# 輸出各模型的檢定結果
cat("Shapiro-Wilk test for Model 1 residuals:\n")
print(shapiro_mod1)
cat("\nShapiro-Wilk test for Model 2 residuals:\n")
print(shapiro_mod2)
cat("\nShapiro-Wilk test for Model 3 residuals:\n")
print(shapiro_mod3)
cat("\nShapiro-Wilk test for Model 4 residuals:\n")
print(shapiro_mod4)

# 2. 繪製 Q-Q plot 檢查各模型殘差的常態性
par(mfrow = c(2,2), mar = c(4,4,2,1))
qqnorm(resid(mod1), main = "Q-Q Plot: Model 1 Residuals", pch = 16)
qqline(resid(mod1), col = "red", lwd = 2)
qqnorm(resid(mod2), main = "Q-Q Plot: Model 2 Residuals", pch = 16)
qqline(resid(mod2), col = "red", lwd = 2)
qqnorm(resid(mod3), main = "Q-Q Plot: Model 3 Residuals", pch = 16)
qqline(resid(mod3), col = "red", lwd = 2)
qqnorm(resid(mod4), main = "Q-Q Plot: Model 4 Residuals", pch = 16)
qqline(resid(mod4), col = "red", lwd = 2)

#根據這四個模型的 Shapiro-Wilk 正態性檢定結果：
#Model 4 p-value < 0.05 未服從常態分佈
#就R squared 與 常態性檢定而言 Model 3 表現最好

#B.
#Model 3 對TIME微分後 可得2*b2*TIME 如果b2 > 0 表示隨著時間推移，產量加速上升
#b2 < 0 則表示隨著時間增加 產量則會加速下降 我認為這結果較符合現實生活 生產的技術進步
#可使產量以較高的倍數上升 而非單純的線性關係 相反的若出現突發的外在衝擊導致產量下降
#在短時間內也不會快速恢復 而會呈現持續性的產量下降 同時也不會是單純的線性關係

#C.
# 2. studentized residuals
stud_res3 <- rstudent(mod3)
# 3. 計算 leverage 值 (hat-values)
lev3 <- hatvalues(mod3)
# 4. 計算 DFBETA (對每個參數的影響)
dfb3 <- dfbeta(mod3)
# 5. 計算 DFFITS (對整體擬合的影響)
dff3 <- dffits(mod3)

# 1. 取得樣本大小 n
n <- length(resid(mod3))

# 2. 取得參數個數 (含截距)
p_plus_1 <- length(coef(mod3))  # Model 3 有截距 + 1 個自變數 (TIME^2)

# 3. 各指標常見 cutoff
cut_stud <- 2  # |學生化殘差| > 2
cut_lev  <- 2 * p_plus_1 / n
cut_dffits <- 2 * sqrt(p_plus_1 / n)
cut_dfbeta <- 2 / sqrt(n)

df_diag <- data.frame(
  obs       = 1:n,             # 觀測值編號
  stud_res  = stud_res3,
  leverage  = lev3,
  dffits    = dff3,
  dfbeta_int    = dfb3[, "(Intercept)"], # 截距的 DFBETA
  dfbeta_time2  = dfb3[, "I(TIME^2)"]    # TIME^2 的 DFBETA
)

# 新增 flag 欄位判斷是否超出閾值
df_diag$flag_stud  <- abs(df_diag$stud_res) > cut_stud
df_diag$flag_lev   <- df_diag$leverage > cut_lev
df_diag$flag_dffits<- abs(df_diag$dffits) > cut_dffits
df_diag$flag_dfb_int   <- abs(df_diag$dfbeta_int) > cut_dfbeta
df_diag$flag_dfb_time2 <- abs(df_diag$dfbeta_time2) > cut_dfbeta

# 將任何一個指標被標示為 TRUE 的列篩選出來
df_unusual <- df_diag[rowSums(df_diag[, grep("^flag_", names(df_diag))]) > 0, ]

# 檢視可疑觀測值
df_unusual

plot(df_diag$obs, df_diag$stud_res,
     main = "Studentized Residuals (Model 3)",
     xlab = "Observation Index", ylab = "Studentized Residual",
     pch = 16)
abline(h = c(-cut_stud, 0, cut_stud), col = c("blue","red","blue"), lty = 2, lwd = 2)

# 在圖上標示出超標的點
text(x = df_unusual$obs,
     y = df_unusual$stud_res,
     labels = df_unusual$obs,
     pos = 3, col = "red")

#異常值與高影響力點：觀測值 14、28 和 43 在學生化殘差上超標，
#表示其在垂直方向上為潛在異常值；而觀測值 45、46、47、48 則顯示出高槓桿特性，
#其中 14、43 和 48 同時在 DFFITS 上超標，它們對模型預測具有較大影響。

#模型穩定性：儘管有部分點在某些診斷指標上超標
#但 DFBETA 的結果顯示這些點對模型參數的改變影響不大，這意味著整體參數估計仍然穩定。

#D.
# 原資料包含 48 筆，對應 time = 1..48 (1950~1997)
# 先將 1..47 (1950~1996) 取出作為訓練資料
train_data <- subset(wa_wheat, time <= 47)

# 檢查資料筆數
nrow(train_data)

# 只用 1950~1996 來估計模型
mod3_sub <- lm(northampton ~ I(time^2), data = train_data)

# 檢視估計結果
summary(mod3_sub)

# 建立 1..48 的時間序列
time_seq <- data.frame(time = 1:48)

# 取得 95% 預測區間 (interval = "prediction")
pred_vals <- predict(mod3_sub, newdata = time_seq, 
                     interval = "prediction", level = 0.95)

# 將結果存入一個 data.frame 方便繪圖
pred_df <- data.frame(
  time = time_seq$time,
  fit  = pred_vals[, "fit"],
  lwr  = pred_vals[, "lwr"],
  upr  = pred_vals[, "upr"]
)

head(pred_df)

# 先畫出訓練資料點 (time=1..47)
plot(train_data$time, train_data$northampton,
     xlim = c(1, 48),                 # X軸範圍: 1..48
     ylim = range(pred_df$lwr, pred_df$upr, train_data$northampton),
     xlab = "Time (Year index)", 
     ylab = "Wheat Yield (ton/ha)",
     main = "Model 3: Quadratic Fit with 95% Prediction Interval",
     pch = 16)

# 加上預測的擬合線 (fit)
lines(pred_df$time, pred_df$fit, col = "blue", lwd = 2)

# 加上預測區間上下限 (lwr, upr)
lines(pred_df$time, pred_df$lwr, col = "red", lty = 2, lwd = 1.5)
lines(pred_df$time, pred_df$upr, col = "red", lty = 2, lwd = 1.5)

# 顯示 1997 年 (time=48) 的預測點
points(48, pred_df$fit[pred_df$time == 48], 
       col = "blue", pch = 19, cex = 1.2)

# 也可把預測區間在 time=48 的位置用 segments 標示出上下限
segments(48, pred_df$lwr[pred_df$time == 48],
         48, pred_df$upr[pred_df$time == 48],
         col = "blue", lwd = 2)

# 最後，把實際的 1997 年資料 (time=48) 加進來比較
actual_1997 <- subset(wa_wheat, time == 48)$northampton
points(48, actual_1997, col = "darkgreen", pch = 17, cex = 1.5)

# 加圖例 (legend)
legend("topleft", 
       legend = c("Training Data (1950-96)", 
                  "Fitted (Predicted Mean)", 
                  "95% Prediction Interval", 
                  "Predicted Value @1997", 
                  "Actual 1997 Value"), 
       col = c("black", "blue", "red", "blue", "darkgreen"), 
       pch = c(16, NA, NA, 19, 17), 
       lty = c(NA, 1, 2, NA, NA),
       bty = "n", 
       pt.cex = c(1, NA, NA, 1.2, 1.5))

if (actual_1997 >= pred_df$lwr[48] && actual_1997 <= pred_df$upr[48]) {
  cat("Actual 1997 value is INSIDE the 95% prediction interval.\n")
} else {
  cat("Actual 1997 value is OUTSIDE the 95% prediction interval.\n")
}


