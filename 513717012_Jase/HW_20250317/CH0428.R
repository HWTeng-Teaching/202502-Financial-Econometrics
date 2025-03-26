# 載入 .RData 檔
load("wa_wheat.rdata")

# 查看目前有哪些物件被載入
ls()

# 檢查資料結構
str(wa_wheat)

# 查看前幾列
head(wa_wheat)

# 查看所有欄位名稱
names(wa_wheat)

# 將資料轉成 data frame
data <- data.frame(
  YIELD = wa_wheat$northampton,
  TIME = wa_wheat$time
)

# 建立四個模型
model1 <- lm(YIELD ~ TIME, data = data)
model2 <- lm(YIELD ~ log(TIME), data = data)
model3 <- lm(YIELD ~ I(TIME^2), data = data)
model4 <- lm(log(YIELD) ~ TIME, data = data)

# 模型清單與標籤
models <- list(model1, model2, model3, model4)
model_names <- c("Model 1: YIELD ~ TIME",
                 "Model 2: YIELD ~ log(TIME)",
                 "Model 3: YIELD ~ TIME^2",
                 "Model 4: log(YIELD) ~ TIME")

# 安裝所需套件（如尚未安裝）
if (!require("ggplot2")) install.packages("ggplot2")
if (!require("car")) install.packages("car")

# 設定 2x2 畫面
par(mfrow = c(2, 2))

for (i in 1:4) {
  model <- models[[i]]
  name <- model_names[i]
  fitted_vals <- fitted(model)
  residuals_vals <- resid(model)
  
  # (i) Fitted values plot
  plot(data$TIME, fitted_vals, type = "l", col = "blue",
       main = paste("Fitted:", name), ylab = "Fitted YIELD", xlab = "TIME")
}

# 設定 2x2 畫面
par(mfrow = c(2, 2))

# 畫 Model 1 殘差圖
plot(resid(model1), main = "Residuals of Model 1", ylab = "model1$residuals", xlab = "Index")

# 畫 Model 2 殘差圖
plot(resid(model2), main = "Residuals of Model 2", ylab = "model2$residuals", xlab = "Index")

# 畫 Model 3 殘差圖
plot(resid(model3), main = "Residuals of Model 3", ylab = "model3$residuals", xlab = "Index")

# 畫 Model 4 殘差圖
plot(resid(model4), main = "Residuals of Model 4", ylab = "model4$residuals", xlab = "Index")

# 設定 2x2 畫面
par(mfrow = c(2, 2))

# QQ Plot for Model 1
qqnorm(resid(model1), main = "QQ Plot of Model 1")
qqline(resid(model1), col = "red")

# QQ Plot for Model 2
qqnorm(resid(model2), main = "QQ Plot of Model 2")
qqline(resid(model2), col = "red")

# QQ Plot for Model 3
qqnorm(resid(model3), main = "QQ Plot of Model 3")
qqline(resid(model3), col = "red")

# QQ Plot for Model 4
qqnorm(resid(model4), main = "QQ Plot of Model 4")
qqline(resid(model4), col = "red")

## 如果尚未安裝 tseries 套件請先安裝
if (!require("tseries")) install.packages("tseries")
library(tseries)

# 計算每個模型的 Jarque-Bera 檢定 p-value
jb_model1 <- jarque.bera.test(resid(model1))
jb_model2 <- jarque.bera.test(resid(model2))
jb_model3 <- jarque.bera.test(resid(model3))
jb_model4 <- jarque.bera.test(resid(model4))

# 建立檢定結果表格
jb_results <- data.frame(
  Model = c("Model 1: YIELD ~ TIME",
            "Model 2: YIELD ~ log(TIME)",
            "Model 3: YIELD ~ TIME^2",
            "Model 4: log(YIELD) ~ TIME"),
  P_Value = c(jb_model1$p.value,
              jb_model2$p.value,
              jb_model3$p.value,
              jb_model4$p.value)
)

# 加上判斷常態性
jb_results$Normality <- ifelse(jb_results$P_Value > 0.05, "Yes", "No")

# 顯示結果表格
print(jb_results)


# (iv) Shapiro-Wilk 檢定與 R² 整理輸出
cat("Summary Table:\n")
cat("---------------\n")
for (i in 1:4) {
  model <- models[[i]]
  sw <- shapiro.test(resid(model))
  cat(model_names[i], "\n")
  cat("  R² =", summary(model)$r.squared, "\n")
  cat("  Shapiro-Wilk p-value =", sw$p.value, "\n\n")
}

# 選擇model 3 
model_best <- model3

# 取得樣本數與參數數
n <- nrow(data)
p <- length(coef(model_best))

# 計算診斷指標
stud_res <- rstudent(model_best)
leverage <- hatvalues(model_best)
dfbetas_vals <- dfbetas(model_best)
dffits_vals <- dffits(model_best)

# 設定閾值
stud_threshold <- 2
lev_threshold <- 2 * (p / n)
dfbetas_threshold <- 2 / sqrt(n)
dffits_threshold <- 2 * sqrt(p / n)

# 找出異常觀察值
unusual <- which(abs(stud_res) > stud_threshold |
                   leverage > lev_threshold |
                   apply(abs(dfbetas_vals), 1, max) > dfbetas_threshold |
                   abs(dffits_vals) > dffits_threshold)

# 顯示異常觀察值索引
print(unusual)

# 如果你想看這些點的所有診斷數值
unusual_df <- data.frame(
  Index = unusual,
  Studentized_Residual = stud_res[unusual],
  Leverage = leverage[unusual],
  DFFITS = dffits_vals[unusual],
  Max_DFBETAS = apply(abs(dfbetas_vals), 1, max)[unusual]
)
print(unusual_df)

# 建立訓練資料（1950–1996）
data_train <- data[1:47, ]

# 重新估計模型
model_train <- lm(YIELD ~ I(TIME^2), data = data_train)

# 建立 1997 年預測資料
new_data <- data.frame(TIME = 48)

# 預測 YIELD 並建立 95% 預測區間
pred <- predict(model_train, newdata = new_data, interval = "prediction", level = 0.95)

# 顯示預測值與區間
print(pred)

# 取得實際 1997 值
actual_1997 <- data$YIELD[48]
cat("Actual YIELD in 1997:", actual_1997, "\n")

# 判斷是否落在區間內
if (actual_1997 >= pred[1, "lwr"] & actual_1997 <= pred[1, "upr"]) {
  cat("The true value is inside the 95% prediction interval.\n")
} else {
  cat("The true value is NOT inside the 95% prediction interval.\n")
}

