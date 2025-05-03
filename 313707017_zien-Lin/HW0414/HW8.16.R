# 載入所需的套件和數據集
library(POE5Rdata)
data("vacation")

model <- lm(miles ~ income + age + kids, data = vacation)
summary_model <- summary(model)

print(vacation)

# a.-----------------
beta_kids <- coef(summary_model)["kids", "Estimate"]
se_kids <- coef(summary_model)["kids", "Std. Error"]
t_value <- qt(0.975, df = 200 - 3 - 1)
ci_lower <- beta_kids - t_value * se_kids
ci_upper <- beta_kids + t_value * se_kids
cat("kids 的 95% 信心區間為：", ci_lower, "到", ci_upper, "\n")

# b.------------------
# 提取殘差
residuals <- resid(model)

# 繪製殘差 vs. income 的散點圖
plot(vacation$income, residuals, 
     xlab = "income (in $1000)", 
     ylab = "Residuals", 
     main = "Residuals vs Income")
abline(h = 0, col = "red")  # 加入水平線 y=0

# 繪製殘差 vs. age 的散點圖
plot(vacation$age, residuals, 
     xlab = "age", 
     ylab = "Residuals", 
     main = "Residuals vs Age")
abline(h = 0, col = "red")  # 加入水平線 y=0

# c.--------------------
# 按照 income 排序
vacation_sorted <- vacation[order(vacation$income), ]

# 分割數據：前 90 個和後 90 個觀測值
first_90 <- vacation_sorted[1:90, ]
last_90 <- vacation_sorted[111:200, ]

# 對前 90 個觀測值擬合 OLS 模型
model_first <- lm(miles ~ income + age + kids, data = first_90)

# 對後 90 個觀測值擬合 OLS 模型
model_last <- lm(miles ~ income + age + kids, data = last_90)

# 提取殘差平方和 (RSS)
rss_first <- sum(resid(model_first)^2)
rss_last <- sum(resid(model_last)^2)

# 計算 Goldfeld-Quandt 檢驗統計量（F 統計量）
gq_stat <- (rss_last / (90 - 4)) / (rss_first / (90 - 4))  # 自由度 = 90 - 4（4 個參數）

# 計算 5% 顯著性水平下的臨界值（F 分佈）
f_critical <- qf(0.975, df1 = 90 - 4, df2 = 90 - 4)  # 雙尾檢驗

# 輸出結果
cat("Goldfeld-Quandt 檢驗統計量：", gq_stat, "\n")
cat("F 臨界值 (5% 水平)：", f_critical, "\n")

# 判斷是否拒絕虛無假設
if (gq_stat > f_critical) {
  cat("拒絕虛無假設，存在異方差性。\n")
} else {
  cat("無法拒絕虛無假設，無顯著異方差性。\n")
}

# d.-------------------------------
# 載入數據和必要套件
install.packages("lmtest")
install.packages("sandwich")
library(sandwich)  # 用於穩健標準誤
library(lmtest)    # 用於 coeftest

# 計算異方差穩健標準誤
robust_se <- coeftest(model, vcov = vcovHC(model, type = "HC1"))

# 提取 kids 的係數和穩健標準誤
beta_kids_robust <- robust_se["kids", "Estimate"]
se_kids_robust <- robust_se["kids", "Std. Error"]

# 計算 95% 信心區間（t 分佈，自由度 = n - k - 1）
t_value <- qt(0.975, df = 200 - 3 - 1)
ci_lower_robust <- beta_kids_robust - t_value * se_kids_robust
ci_upper_robust <- beta_kids_robust + t_value * se_kids_robust

# 輸出穩健信心區間
cat("kids 的 95% 穩健信心區間為：", ci_lower_robust, "到", ci_upper_robust, "\n")

# 問題 (a) 的普通 OLS 信心區間（參考之前的計算）
summary_model <- summary(model)
beta_kids_ols <- coef(summary_model)["kids", "Estimate"]
se_kids_ols <- coef(summary_model)["kids", "Std. Error"]
ci_lower_ols <- beta_kids_ols - t_value * se_kids_ols
ci_upper_ols <- beta_kids_ols + t_value * se_kids_ols

# 輸出普通 OLS 信心區間
cat("問題 (a) 的 OLS 信心區間為：", ci_lower_ols, "到", ci_upper_ols, "\n")

# e.--------------------
# 擬合 GLS 模型（加權最小二乘）
# 權重為 1/income（因為方差正比於 income^2，標準差正比於 income）
weights <- 1 / vacation$income
model_gls <- lm(miles ~ income + age + kids, data = vacation, weights = weights)

# 提取 GLS 標準誤
summary_gls <- summary(model_gls)
beta_kids_gls <- coef(summary_gls)["kids", "Estimate"]
se_kids_gls <- coef(summary_gls)["kids", "Std. Error"]

# 計算 GLS 的 95% 信心區間
t_value <- qt(0.975, df = 200 - 3 - 1)
ci_lower_gls <- beta_kids_gls - t_value * se_kids_gls
ci_upper_gls <- beta_kids_gls + t_value * se_kids_gls

# 輸出 GLS 信心區間
cat("kids 的 95% GLS 信心區間為：", ci_lower_gls, "到", ci_upper_gls, "\n")

# 計算穩健 GLS 標準誤
robust_se_gls <- coeftest(model_gls, vcov = vcovHC(model_gls, type = "HC1"))

# 提取 kids 的穩健標準誤
beta_kids_robust_gls <- robust_se_gls["kids", "Estimate"]  # 與 GLS 係數相同
se_kids_robust_gls <- robust_se_gls["kids", "Std. Error"]

# 計算穩健 GLS 的 95% 信心區間
ci_lower_robust_gls <- beta_kids_robust_gls - t_value * se_kids_robust_gls
ci_upper_robust_gls <- beta_kids_robust_gls + t_value * se_kids_robust_gls

# 輸出穩健 GLS 信心區間
cat("kids 的 95% 穩健 GLS 信心區間為：", ci_lower_robust_gls, "到", ci_upper_robust_gls, "\n")

# 問題 (a) 和 (d) 的信心區間（參考之前的結果）
# 問題 (a) OLS 信心區間
model_ols <- lm(miles ~ income + age + kids, data = vacation)
summary_ols <- summary(model_ols)
beta_kids_ols <- coef(summary_ols)["kids", "Estimate"]
se_kids_ols <- coef(summary_ols)["kids", "Std. Error"]
ci_lower_ols <- beta_kids_ols - t_value * se_kids_ols
ci_upper_ols <- beta_kids_ols + t_value * se_kids_ols

# 問題 (d) 穩健 OLS 信心區間
robust_se_ols <- coeftest(model_ols, vcov = vcovHC(model_ols, type = "HC1"))
beta_kids_robust_ols <- robust_se_ols["kids", "Estimate"]
se_kids_robust_ols <- robust_se_ols["kids", "Std. Error"]
ci_lower_robust_ols <- beta_kids_robust_ols - t_value * se_kids_robust_ols
ci_upper_robust_ols <- beta_kids_robust_ols + t_value * se_kids_robust_ols

# 輸出問題 (a) 和 (d) 的信心區間
cat("問題 (a) 的 OLS 信心區間為：", ci_lower_ols, "到", ci_upper_ols, "\n")
cat("問題 (d) 的穩健 OLS 信心區間為：", ci_lower_robust_ols, "到", ci_upper_robust_ols, "\n")


