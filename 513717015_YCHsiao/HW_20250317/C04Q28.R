

## R Code for Estimating the Four Equations

```R

install.packages("remotes")  # 確保 remotes 套件已安裝
remotes::install_github("ccolonescu/POE5Rdata")  # 需確認 GitHub Repo 位置
library(POE5Rdata)

# 載入資料
data(wa_wheat)
wa_wheat_data <- wa_wheat

# 定義變數
YIELD <- wa_wheat_data$northampton
TIME <- wa_wheat_data$time

# 模型估計
model1 <- lm(YIELD ~ TIME)
model2 <- lm(YIELD ~ log(TIME))
model3 <- lm(YIELD ~ I(TIME^2))
model4 <- lm(log(YIELD) ~ TIME)

# 模型摘要結果
summary(model1)
summary(model2)
summary(model3)
summary(model4)

#b.

# Extract coefficients from the quadratic model
coef(model3)

# Extract coefficients with names
summary(model3)$coefficients


#c.

# 計算 Leverage 值
leverage_values <- hatvalues(model3)

# 設定標準閾值（通常為 2 × 平均 leverage）
leverage_threshold <- 2 * mean(leverage_values)

# 找出高於閾值的觀測點
high_leverage <- which(leverage_values > leverage_threshold)

# 顯示異常觀測值
wa_wheat_data[high_leverage, ]


# 計算 Studentized Residuals
studentized_residuals <- rstudent(model3)

# 設定標準閾值（通常為 ±2）
studentized_threshold <- 2

# 找出超過閾值的異常觀測點
high_residuals <- which(abs(studentized_residuals) > studentized_threshold)

# 顯示異常觀測值
wa_wheat_data[high_residuals, ]


# 計算 DFBETAS
dfbetas_values <- dfbetas(model3)

# 設定標準閾值（通常為 $2 / \sqrt{N}$）
dfbetas_threshold <- 2 / sqrt(nrow(wa_wheat_data))

# 找出異常觀測值
high_dfbetas <- which(abs(dfbetas_values[,2]) > dfbetas_threshold)

# 顯示異常觀測值
wa_wheat_data[high_dfbetas, ]


# 計算 DFFITS
dffits_values <- dffits(model3)

# 設定標準閾值（通常為 $2 \times \sqrt{p / N}$）
dffits_threshold <- 2 * sqrt(ncol(model.matrix(model3)) / nrow(wa_wheat_data))

# 找出異常觀測值
high_dffits <- which(abs(dffits_values) > dffits_threshold)

# 顯示異常觀測值
wa_wheat_data[high_dffits, ]


#d.

# 過濾 1950-1996 年的數據
wa_wheat_train <- subset(wa_wheat_data, time <= 46)

# 重新估計 Model 3（只用 1950-1996 年的數據）
model3_train <- lm(northampton ~ I(time^2), data = wa_wheat_train)

# 顯示模型摘要
summary(model3_train)

# 建立 1997 年的預測數據
new_data <- data.frame(time = 47)

# 進行預測並計算 95% 預測區間
pred_1997 <- predict(model3_train, new_data, interval = "prediction", level = 0.95)

# 顯示預測結果
print(pred_1997)

# 取得 1997 年的實際值
actual_1997 <- wa_wheat_data[wa_wheat_data$time == 47, "northampton"]

# 比對預測區間與真實值
print(actual_1997)



