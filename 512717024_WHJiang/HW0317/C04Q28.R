# ------------------------------------------------
# 0. 載入資料與套件
# ------------------------------------------------
# 假設資料檔案為 "wa-wheat.dat" 或 "wa-wheat.csv"，需依實際情況調整讀取方法
# 下方僅示範以 CSV 為例；若是其他格式請依實際情況更改。
# setwd("...")  # 若需要設定工作目錄
# wa_wheat <- read.csv("wa-wheat.csv")

# 假設我們現在已經有一個 data frame: wa_wheat
# 結構 (示範):
#   YEAR   TIME   YIELD
#   1950     1    ...
#   1951     2    ...
#   ...
#   1997    48    ...

# 載入可能會用到的套件
library(ggplot2)       # 繪圖
library(dplyr)         # 整理資料
library(car)           # 殘差圖等診斷函數
library(MASS)          # studres, rstudent, 等
library(lmtest)        # bptest 等檢定
library(sandwich)      # 白化估計量等 (可能需要)
library(influence.ME)  # 若需要進階影響力分析 (非必須)

# ------------------------------------------------
# 1. 建立四種模型
# ------------------------------------------------
# (1) YIELD1 = β0 + β1 * TIME + e
model1 <- lm(YIELD ~ TIME, data = wa_wheat)

# (2) YIELD2 = α0 + α1 * ln(TIME) + e
#   注意 TIME=0 會導致 ln(0) 無意義，題目顯示資料是 1950~1997，因此 TIME>=1 應無問題。
model2 <- lm(YIELD ~ log(TIME), data = wa_wheat)

# (3) YIELD3 = φ0 + φ1 * (TIME^2) + e
#   可以用 I(TIME^2) 表示 TIME^2
model3 <- lm(YIELD ~ I(TIME^2), data = wa_wheat)

# (4) ln(YIELD) = γ0 + γ1 * TIME + e
#   等價於 YIELD4 = exp(γ0 + γ1*TIME + e)，先對 YIELD 取對數
#   注意 YIELD>0 才能取 log
model4 <- lm(log(YIELD) ~ TIME, data = wa_wheat)

# ------------------------------------------------
# 2. 檢視迴歸結果與比較
# ------------------------------------------------
summary(model1)
summary(model2)
summary(model3)
summary(model4)

# R^2 (調整後的 R^2) 也是比較的依據之一
cat("Model1 R^2:", summary(model1)$r.squared, "\n")
cat("Model2 R^2:", summary(model2)$r.squared, "\n")
cat("Model3 R^2:", summary(model3)$r.squared, "\n")
cat("Model4 R^2:", summary(model4)$r.squared, "\n")

# 若要比較 AIC 或 BIC 也可以：
AIC(model1, model2, model3, model4)
BIC(model1, model2, model3, model4)

# ------------------------------------------------
# 3. 圖形檢視：實際 vs. 拟合, 殘差圖, Q-Q plot
# ------------------------------------------------
# (a) 實際 vs. 預測
wa_wheat$pred1 <- fitted(model1)
wa_wheat$pred2 <- fitted(model2)
wa_wheat$pred3 <- fitted(model3)
wa_wheat$pred4 <- exp(fitted(model4))  # model4 是 log(YIELD)，要反轉回原尺度

# 例如，繪製 Model1 的實際與預測
ggplot(wa_wheat, aes(x = TIME)) +
  geom_point(aes(y = YIELD), color = "blue", size = 2) +
  geom_line(aes(y = pred1), color = "red", size = 1) +
  labs(title = "Model 1: Actual vs Fitted",
       y = "YIELD") +
  theme_minimal()

# (b) 殘差圖 (以 TIME 為橫軸)
ggplot(wa_wheat, aes(x = TIME, y = residuals(model1))) +
  geom_point() +
  geom_hline(yintercept = 0, color = "red", linetype = "dashed") +
  labs(title = "Model 1 Residuals vs TIME") +
  theme_minimal()

# (c) Normal Q-Q Plot 檢查殘差常態性
qqnorm(residuals(model1))
qqline(residuals(model1), col = "red")

# 以上對 model2, model3, model4 亦可重複類似操作

# 若要做 Shapiro-Wilk normality test：
shapiro.test(residuals(model1))
shapiro.test(residuals(model2))
# 其餘模型依樣類推

# ------------------------------------------------
# 4. 選定最佳模型，解釋時間變數的係數意義
# ------------------------------------------------
# 根據 (1) 圖形外觀 (2) 殘差模式 (3) 常態檢定 (4) R^2 或 AIC/BIC
# 選出較佳模型 (假設我們最終選 model4 只是示範)

best_model <- model4

# 解釋時間變數的迴歸係數:
# model4 中 log(YIELD) = γ0 + γ1 * TIME
# γ1 表示 TIME 增加 1，log(YIELD) 的平均改變量
# => YIELD 大約會乘上 exp(γ1)

# ------------------------------------------------
# 5. 檢測異常值/高影響力點 (studentized residual, leverage, DFBETA, DFFITS)
# ------------------------------------------------
# (a) studentized residuals (rstudent) 或外學生化殘差 (studres)
wa_wheat$rstudent <- rstudent(best_model)  # 或 MASS::studres(best_model)
# (b) leverage
wa_wheat$leverage <- hatvalues(best_model)
# (c) dfbetas
dfb <- dfbetas(best_model)
# (d) dffits
dff <- dffits(best_model)

# 若要將 dfbetas/dffits 合併到資料框
wa_wheat$dfb_Intercept <- dfb[,1]
wa_wheat$dfb_TIME      <- dfb[,2]  # 依模型自變數數量而定
wa_wheat$dffits        <- dff

# 查看是否有絕對值明顯過大的觀測值 (例如 |rstudent|>2, |dfbeta|>1, etc.)
# 可依據文獻或教科書建議之臨界值標準
wa_wheat %>%
  filter(abs(rstudent) > 2 | abs(dffits) > 2*sqrt(2/nrow(wa_wheat))) %>%
  select(YEAR, TIME, YIELD, rstudent, leverage, dfb_Intercept, dfb_TIME, dffits)

# ------------------------------------------------
# 6. 以 1950~1996 資料估計，對 1997 預測
# ------------------------------------------------
# 假設 1997 對應 TIME=48
train_data <- filter(wa_wheat, YEAR <= 1996)  # 1950~1996
test_data  <- filter(wa_wheat, YEAR == 1997)  # 單筆，或可能多筆

# 用相同模型形式重新估計 (例如 model4)
best_model_train <- lm(log(YIELD) ~ TIME, data = train_data)

# 對 1997 (TIME=48) 進行預測，並產生 95% 預測區間
pred_1997 <- predict(best_model_train,
                     newdata = data.frame(TIME = 48),
                     interval = "prediction",
                     level = 0.95)

# 因為這是對 log(YIELD) 迴歸，所以要把結果轉回 YIELD 原尺度
# predict() 產生的 fit, lwr, upr 都在 log(YIELD) 尺度上
pred_1997_yield <- exp(pred_1997)
cat("Prediction for YIELD in 1997 (original scale):\n")
print(pred_1997_yield)

# 比較真實值是否落在預測區間內
actual_1997 <- test_data$YIELD
cat("Actual 1997 YIELD =", actual_1997, "\n")
cat("95% Prediction Interval = [", pred_1997_yield[1, "lwr"], ",", pred_1997_yield[1, "upr"], "]\n")

if (actual_1997 >= pred_1997_yield[1, "lwr"] &&
    actual_1997 <= pred_1997_yield[1, "upr"]) {
  cat("The actual 1997 YIELD is within the 95% prediction interval.\n")
} else {
  cat("The actual 1997 YIELD is NOT within the 95% prediction interval.\n")
}
