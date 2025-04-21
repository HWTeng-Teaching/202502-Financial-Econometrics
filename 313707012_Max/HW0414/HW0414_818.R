library(POE5Rdata)
data("cps5")

#a.------------------------------------------------------
# 計算男性（female = 0）和女性（female = 1）的分組
male_data <- subset(cps5, female == 0)
female_data <- subset(cps5, female == 1)

# 進行回歸分析，男性回歸
male_model <- lm(log(wage) ~ educ + exper + I(exper^2) + metro + south + midwest + west, data = male_data)
male_residuals <- residuals(male_model)

# 女性回歸
female_model <- lm(log(wage) ~ educ + exper + I(exper^2) + metro + south + midwest + west, data = female_data)
female_residuals <- residuals(female_model)

# 計算殘差的變異數
sigma_m_squared <- var(male_residuals)
sigma_f_squared <- var(female_residuals)

# 計算 F 統計量
f_statistic <- sigma_m_squared / sigma_f_squared

# 計算 Goldfeld–Quandt 檢定的臨界值（自由度較小的為 5% 水準）
n_male <- length(male_residuals)
n_female <- length(female_residuals)

# 假設自由度 df1 = n_male-1, df2 = n_female-1
critical_value <- qf(0.95, df1 = n_male - 1, df2 = n_female - 1)

# 印出結果
cat("F 統計量:", f_statistic, "\n")
cat("臨界值:", critical_value, "\n")

# 檢定結果
if (f_statistic > critical_value | f_statistic < 1 / critical_value) {
  cat("拒絕虛無假設，認為男性與女性的工資變異數不同。\n")
} else {
  cat("未拒絕虛無假設，認為男性與女性的工資變異數相同。\n")
}

#b.------------------------------------------------------
# 進行 OLS 回歸
model_ols <- lm(log(wage) ~ educ + exper + I(exper^2) + metro + south + midwest + west + female + black, data = cps5)
residuals_ols <- residuals(model_ols)
residuals_squared <- residuals_ols^2

# 首先，使用與異質變異性相關的變數（METRO, FEMALE, BLACK）進行回歸
hetero_model1 <- lm(residuals_squared ~ metro + female + black, data = cps5)

# 計算 R^2
r_squared1 <- summary(hetero_model1)$r.squared

# 計算 NR2 檢定統計量
n <- nrow(cps5)  # 樣本大小
nr2_statistic1 <- n * r_squared1

p_value1 <- 1 - pf(nr2_statistic1, df1 = 3, df2 = n - 4)

cat("使用 METRO, FEMALE, BLACK 變數進行 NR2 測試：\n")
cat("R^2:", r_squared1, "\n")
cat("NR2 統計量:", nr2_statistic1, "\n")
cat("p 值:", p_value1, "\n")

# 根據 p 值來得出結論
if (p_value1 < 0.01) {
  cat("在 1% 顯著性水準下，拒絕虛無假設，表明存在異質變異性。\n")
} else {
  cat("在 1% 顯著性水準下，未能拒絕虛無假設，表明不存在異質變異性。\n")
}

# 重複此測(所有參數)
hetero_model2 <- lm(residuals_squared ~ educ + exper + I(exper^2) + metro + south + midwest + west + female + black, data = cps5)
r_squared2 <- summary(hetero_model2)$r.squared

# 計算 NR2 檢定統計量
nr2_statistic2 <- n * r_squared2

# 計算 p 值
p_value2 <- 1 - pf(nr2_statistic2, df1 = 9, df2 = n - 10)

cat("\n使用所有解釋變數進行 NR2 測試：\n")
cat("R^2:", r_squared2, "\n")
cat("NR2 統計量:", nr2_statistic2, "\n")
cat("p 值:", p_value2, "\n")

# 根據 p 值來得出結論
if (p_value2 < 0.01) {
  cat("在 1% 顯著性水準下，拒絕虛無假設，表明存在異質變異性。\n")
} else {
  cat("在 1% 顯著性水準下，未能拒絕虛無假設，表明不存在異質變異性。\n")
}

#c.------------------------------------------------------
# 計算包含平方項的回歸模型
white_model <- lm(residuals_squared ~ educ + exper + I(exper^2) + metro + south + midwest + west + female + black, data = cps5)

# 計算 R^2
r_squared_white <- summary(white_model)$r.squared

# 計算 White 檢定統計量
n <- nrow(cps5)  # 樣本大小
white_statistic <- n * r_squared_white

# 計算臨界值（自由度 = 9，對應模型的解釋變數數量）
critical_value_white <- qchisq(0.95, df = 9)

# 印出結果
cat("White 檢定統計量:", white_statistic, "\n")
cat("臨界值:", critical_value_white, "\n")

# 檢定結論
if (white_statistic > critical_value_white) {
  cat("拒絕虛無假設，表明存在異質變異性。\n")
} else {
  cat("未拒絕虛無假設，表明不存在異質變異性。\n")
}


#d.------------------------------------------------------

# 使用 White 標準誤進行 OLS 回歸
library(sandwich)  # 需要載入這個包來計算白標準誤
white_se <- vcovHC(model_ols, type = "HC3")  # HC3 是一種常見的穩健估算方法

# 計算 White 標準誤
white_se_results <- coeftest(model_ols, vcov = white_se)
print(white_se_results)

# 95% 置信區間
confint_ols <- confint(model_ols)
confint_white <- confint(model_ols, vcov = white_se)

# 輸出置信區間
cat("傳統OLS標準誤的95%置信區間：\n")
print(confint_ols)
cat("\n使用White穩健標準誤的95%置信區間：\n")
print(confint_white)

# 比較置信區間的寬度
interval_width_ols <- confint_ols[, 2] - confint_ols[, 1]
interval_width_white <- confint_white[, 2] - confint_white[, 1]

cat("\n傳統OLS置信區間寬度：\n")
print(interval_width_ols)
cat("\n使用White穩健標準誤置信區間寬度：\n")
print(interval_width_white)

# 比較寬度，觀察是變窄還是變寬
narrower <- which(interval_width_white < interval_width_ols)
wider <- which(interval_width_white > interval_width_ols)

cat("\n置信區間變窄的係數：\n")
print(names(narrower))

cat("\n置信區間變寬的係數：\n")
print(names(wider))

#e.------------------------------------------------------
# 載入所需的包
library(sandwich)  # 用於穩健標準誤
library(lmtest)    # 用於顯示回歸結果

# 對 METRO 和 EXPER 變數進行 FGLS 估計
model_fgls <- lm(log(wage) ~ metro + exper, data = cps5)
summary(model_fgls)

# 估計殘差
residuals_fgls <- residuals(model_fgls)

# 根據 METRO 和 EXPER 計算誤差項的異質性結構（假設變異性與 METRO 和 EXPER 相關）
# 使用權重進行加權最小二乘回歸
fgls_weight <- lm(residuals_fgls^2 ~ metro + exper, data = cps5)
weights <- predict(fgls_weight)

# 進行加權最小二乘回歸
model_fgls_wls <- lm(log(wage) ~ metro + exper, data = cps5, weights = 1 / weights)

# 使用 White 標準誤進行穩健回歸估計
white_se_fgls <- vcovHC(model_fgls_wls, type = "HC3")

# 計算 White 標準誤下的置信區間
confint_fgls_white <- confint(model_fgls_wls, vcov = white_se_fgls)

# 輸出結果
cat("\nFGLS回歸的置信區間（使用White穩健標準誤）：\n")
print(confint_fgls_white)

# 輸出OLS回歸的置信區間（使用White穩健標準誤，來自第d部分）
cat("\nOLS回歸的置信區間（使用White穩健標準誤）：\n")
confint_ols_white <- confint(model_ols, vcov = white_se)
print(confint_ols_white)

# 比較FGLS和OLS置信區間的寬度
interval_width_fgls <- confint_fgls_white[, 2] - confint_fgls_white[, 1]
interval_width_ols <- confint_ols_white[, 2] - confint_ols_white[, 1]

cat("\nFGLS置信區間寬度：\n")
print(interval_width_fgls)
cat("\nOLS置信區間寬度：\n")
print(interval_width_ols)

# 比較區間寬度，觀察是變窄還是變寬
narrower_fgls <- which(interval_width_fgls < interval_width_ols)
wider_fgls <- which(interval_width_fgls > interval_width_ols)

cat("\nFGLS置信區間變窄的係數：\n")
print(names(narrower_fgls))

cat("\nFGLS置信區間變寬的係數：\n")
print(names(wider_fgls))

#f.------------------------------------------------------
# 執行 OLS 模型，取得殘差平方
ols_model <- lm(log(wage) ~ educ + exper + I(exper^2) + female + black + metro + south + midwest + west, data = cps5)
res_squared <- residuals(ols_model)^2

# 建立變異數模型（用 metro 和 exper 解釋異質性）
var_model <- lm(res_squared ~ metro + exper, data = cps5)
predicted_var <- predict(var_model)

# 防止除以 0（將預測變異數小於等於 0 的值改成最小正數）
predicted_var[predicted_var <= 0] <- min(predicted_var[predicted_var > 0])
weights <- 1 / predicted_var

# 加權最小平方法（FGLS）回歸
fgls_model <- lm(log(wage) ~ educ + exper + I(exper^2) + female + black + metro + south + midwest + west,
                 data = cps5, weights = weights)

# 使用 heteroskedasticity robust 標準誤計算 FGLS 的信賴區間
robust_se_fgls <- vcovHC(fgls_model, type = "HC1")
confint_fgls_robust <- confint(fgls_model, vcov = robust_se_fgls)

# 輸出結果
cat("FGLS（使用 robust 標準誤）95% 信賴區間:\n")
print(confint_fgls_robust)
