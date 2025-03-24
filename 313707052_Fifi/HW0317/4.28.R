library(POE5Rdata)
data("wa_wheat", package = "POE5Rdata")

names(wa_wheat)

#A.

# 設定 Northampton 地區的 YIELD 變數
data <- data.frame(
  YIELD = wa_wheat$northampton,
  TIME = wa_wheat$time
)

# 繪製時間序列圖
plot(data$TIME, data$YIELD, main="Wheat Yield Over Time (Northampton)", 
     xlab="Time", ylab="Yield", pch=16)

# 建立四種迴歸模型
model1 <- lm(YIELD ~ TIME, data=data)
model2 <- lm(YIELD ~ log(TIME), data=data)
model3 <- lm(YIELD ~ I(TIME^2), data=data)
model4 <- lm(log(YIELD) ~ TIME, data=data)

# 顯示模型摘要
summary(model1)
summary(model2)
summary(model3)
summary(model4)

# 比較 R²
cat("R-squared values:\n")
cat("Model 1:", summary(model1)$r.squared, "\n")
cat("Model 2:", summary(model2)$r.squared, "\n")
cat("Model 3:", summary(model3)$r.squared, "\n")
cat("Model 4:", summary(model4)$r.squared, "\n")

par(mfrow = c(2, 2))  # 建立 2x2 圖形佈局

# Model 1: YIELD ~ TIME
with(na.omit(data), {
  plot(TIME, YIELD, main = "Model 1: YIELD ~ TIME",
       xlab = "Time", ylab = "Yield", pch = 16)
  lines(TIME, fitted(model1), col = "blue", lwd = 2)
})

# Model 2: YIELD ~ log(TIME)
with(na.omit(data), {
  plot(TIME, YIELD, main = "Model 2: YIELD ~ log(TIME)",
       xlab = "Time", ylab = "Yield", pch = 16)
  lines(TIME, fitted(model2), col = "red", lwd = 2)
})

# Model 3: YIELD ~ TIME^2
with(na.omit(data), {
  plot(TIME, YIELD, main = "Model 3: YIELD ~ TIME^2",
       xlab = "Time", ylab = "Yield", pch = 16)
  lines(TIME, fitted(model3), col = "green", lwd = 2)
})

# Model 4: log(YIELD) ~ TIME
with(na.omit(data), {
  plot(TIME, log(YIELD), main = "Model 4: log(YIELD) ~ TIME",
       xlab = "Time", ylab = "log(Yield)", pch = 16)
  lines(TIME, fitted(model4), col = "purple", lwd = 2)
})

par(mfrow = c(2, 2))  # 一次排 4 行 2 列，共 8 張圖

# --------- Model 1: YIELD ~ TIME ---------
plot(model1, which = 1, main = "Model 1 - Residuals vs Fitted")
plot(model1, which = 2, main = "Model 1 - Normal Q-Q")

# --------- Model 2: YIELD ~ log(TIME) ---------
plot(model2, which = 1, main = "Model 2 - Residuals vs Fitted")
plot(model2, which = 2, main = "Model 2 - Normal Q-Q")

# --------- Model 3: YIELD ~ TIME^2 ---------
plot(model3, which = 1, main = "Model 3 - Residuals vs Fitted")
plot(model3, which = 2, main = "Model 3 - Normal Q-Q")

# --------- Model 4: log(YIELD) ~ TIME ---------
plot(model4, which = 1, main = "Model 4 - Residuals vs Fitted")
plot(model4, which = 2, main = "Model 4 - Normal Q-Q")





# 殘差常態性檢定 (Shapiro-Wilk test)
shapiro.test(residuals(model1))
shapiro.test(residuals(model2))
shapiro.test(residuals(model3))
shapiro.test(residuals(model4))

#B.

# 安裝並載入 car 套件（如果尚未安裝）
if (!require(car)) install.packages("car", dependencies=TRUE)
library(car)

# 選擇最佳模型（這裡使用 Model 3）
best_model <- lm(YIELD ~ I(TIME^2), data=data)

# 計算標準化殘差
stud_resid <- rstandard(best_model)

# 計算槓桿值 (Leverage)
leverage_values <- hatvalues(best_model)

# 計算 DFBETAS
dfbetas_values <- dfbetas(best_model)

# 計算 DFFITS
dffits_values <- dffits(best_model)

# 找出標準化殘差 > |2| 的異常值
outliers_resid <- which(abs(stud_resid) > 2)

# 找出槓桿值過高的點（通常閾值為 2*(p+1)/n）
n <- nrow(data)  # 樣本數
p <- length(coef(best_model))  # 變數數量（包含截距）
leverage_threshold <- 2 * (p + 1) / n
outliers_leverage <- which(leverage_values > leverage_threshold)

# 找出 DFFITS 大於 2 * sqrt(p/n) 的異常值
dffits_threshold <- 2 * sqrt(p/n)
outliers_dffits <- which(abs(dffits_values) > dffits_threshold)

# 找出 DFBETAS 大於 1 的異常值
outliers_dfbetas <- which(apply(abs(dfbetas_values), 1, max) > 1)

# 顯示異常觀測值
cat("標準化殘差異常值觀測點: ", outliers_resid, "\n")
cat("槓桿值異常值觀測點: ", outliers_leverage, "\n")
cat("DFFITS 異常值觀測點: ", outliers_dffits, "\n")
cat("DFBETAS 異常值觀測點: ", outliers_dfbetas, "\n")

#c.

# 計算 studentized residuals（外部 studentized residuals）
rstudent_vals <- rstudent(model3)

# 計算 hat values（leverage）
leverage_vals <- hatvalues(model3)

# 計算 DFBETAS
dfbetas_vals <- dfbetas(model3)

# 計算 DFFITS
dffits_vals <- dffits(model3)

# 用 data frame 整理指標
influence_df <- data.frame(
  Obs = 1:length(rstudent_vals),
  Studentized_Residual = rstudent_vals,
  Leverage = leverage_vals,
  DFFITS = dffits_vals,
  DFBETA_TIME2 = dfbetas_vals[, 2]  # 第二個係數是 TIME^2
)

# 顯示可能異常觀測值（依據常見門檻）
unusual_obs <- subset(influence_df,
                      abs(Studentized_Residual) > 2 |
                        Leverage > 2 * mean(leverage_vals) |
                        abs(DFFITS) > 2 * sqrt(2 / nrow(data)) |
                        abs(DFBETA_TIME2) > 2 / sqrt(nrow(data))
)

print(unusual_obs)

#d.

# 用 time <= 47 的資料來重新估計模型
train_data <- subset(data, TIME <= 47)
model3_train <- lm(YIELD ~ I(TIME^2), data = train_data)
# 新資料點：time = 48
newdata_1997 <- data.frame(TIME = 48)

# 進行預測，並給出 95% 預測區間（interval = "prediction"）
pred_1997 <- predict(model3_train, newdata = newdata_1997, interval = "prediction", level = 0.95)

print(pred_1997)

# 實際 1997 年的產量
actual_1997 <- data$YIELD[data$TIME == 48]
cat("Actual YIELD in 1997:", actual_1997, "\n")


