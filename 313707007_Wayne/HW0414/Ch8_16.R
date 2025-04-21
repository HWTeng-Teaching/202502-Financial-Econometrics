#(a)
# Step 1: 讀取資料
data <- vacation

# Step 2: 檢查欄位
head(data)
str(data)

# Step 3: 執行 OLS 回歸（使用小寫欄位名稱）
model <- lm(miles ~ income + age + kids, data = data)

# Step 4: 查看模型摘要
summary(model)

# Step 5: 建立 kids 的 95% 信賴區間
confint(model, "kids", level = 0.95)

#(b)
# Step 1: 建立回歸模型
model <- lm(miles ~ income + age + kids, data = data)

# Step 2: 取出 OLS 殘差與資料欄位
residuals <- resid(model)

# Step 3: 繪製殘差 vs INCOME
plot(data$income, residuals,
     xlab = "Income (in $1000s)",
     ylab = "OLS Residuals",
     main = "Residuals vs Income",
     pch = 20, col = "blue")
abline(h = 0, lty = 2)

# Step 4: 繪製殘差 vs AGE
plot(data$age, residuals,
     xlab = "Age",
     ylab = "OLS Residuals",
     main = "Residuals vs Age",
     pch = 20, col = "darkgreen")
abline(h = 0, lty = 2)

#(c)

# Step 2: 按 income 排序
data_sorted <- data[order(data$income), ]

# Step 3: 分割樣本
first90 <- data_sorted[1:90, ]
last90 <- data_sorted[(nrow(data_sorted)-89):nrow(data_sorted), ]

# Step 4: 建立模型
model_first <- lm(miles ~ income + age + kids, data = first90)
model_last <- lm(miles ~ income + age + kids, data = last90)

# Step 5: 計算 SSE
SSE_first <- sum(resid(model_first)^2)
SSE_last <- sum(resid(model_last)^2)

# Step 6: 計算 GQ 檢定統計量
df1 <- df2 <- 90 - 4  # 自由度 (樣本數 - 參數數量)
F_stat <- (SSE_last / df2) / (SSE_first / df1)

# Step 7: 顯示統計量與臨界值
F_stat
qf(0.95, df1, df2)  # 5% 顯著水準的臨界值

#(d)

# 取得估計值與 robust 標準誤
coef_kids <- coef(model)["kids"]
se_kids <- sqrt(robust_vcov["kids", "kids"])

# 取得自由度與 t 分位數
df <- df.residual(model)
t_crit <- qt(0.975, df)

# 計算 95% 信賴區間
ci_low <- coef_kids - t_crit * se_kids
ci_high <- coef_kids + t_crit * se_kids

# 顯示
cat("95% CI for kids (robust, using t-distribution):\n")
cat(sprintf("[%.2f, %.2f]\n", ci_low, ci_high))

#(e)
# 讀取資料
data <- read.csv("vacation.csv")

# 建立 GLS 權重（σ_i^2 ∝ income_i^2）
data$weight <- 1 / (data$income^2)

# (e) GLS 模型估計
gls_model <- lm(miles ~ income + age + kids, data = data, weights = weight)

# 取得 GLS 估計值與 kids 的傳統信賴區間
coef_kids_gls <- coef(gls_model)["kids"]
ci_gls_conventional <- confint(gls_model, "kids", level = 0.95)

# 建立 GLS robust 標準誤（White robust）
gls_vcov_robust <- vcovHC(gls_model, type = "HC0")

# 手動建立 kids 的 95% 信賴區間（t 分布）
se_kids_robust <- sqrt(gls_vcov_robust["kids", "kids"])
df_gls <- df.residual(gls_model)
tcrit <- qt(0.975, df_gls)
ci_gls_robust <- c(
  coef_kids_gls - tcrit * se_kids_robust,
  coef_kids_gls + tcrit * se_kids_robust
)

# 輸出比較結果
cat("GLS（假設 Var(e) ∝ income²）估計值 (kids):", round(coef_kids_gls, 2), "\n")
cat("95% CI (傳統 GLS 標準誤):", round(ci_gls_conventional, 2), "\n")
cat("95% CI (robust GLS 標準誤):", round(ci_gls_robust, 2), "\n")

