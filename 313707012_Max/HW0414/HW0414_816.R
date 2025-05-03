library(POE5Rdata)
data("vacation")

#a.------------------------------------------------------
# 線性迴歸模型估計
model <- lm(miles ~ income + age + kids, data = vacation)

summary(model)
confint(model, "kids", level = 0.95)

#b.------------------------------------------------------
residuals <- resid(model)
# 繪製殘差 vs income
plot(vacation$income, residuals,
     main = "Residuals vs Income",
     xlab = "Income (in $1000s)",
     ylab = "OLS Residuals",
     pch = 19, col = "blue")
abline(h = 0, col = "red", lwd = 2)

# 繪製殘差 vs age
plot(vacation$age, residuals,
     main = "Residuals vs Age",
     xlab = "Age of Adults",
     ylab = "OLS Residuals",
     pch = 19, col = "darkgreen")
abline(h = 0, col = "red", lwd = 2)

#c.------------------------------------------------------
#income 由小到大
vacation_sorted <- vacation[order(vacation$income), ]

#把中間 20 筆觀測拿掉，保留前 90 和後 90
group1 <- vacation_sorted[1:90, ]
group2 <- vacation_sorted[111:200, ]  # 共 200 筆，所以後 90 是從 111 開始

#分別估計 OLS 模型
model1 <- lm(miles ~ income + age + kids, data = group1)
model2 <- lm(miles ~ income + age + kids, data = group2)

#計算殘差平方和 (RSS)
RSS1 <- sum(resid(model1)^2)
RSS2 <- sum(resid(model2)^2)

#計算 F 統計量
F_stat <- RSS2 / RSS1

#比對臨界值或計算 p 值
df1 <- model2$df.residual  # 後組自由度
df2 <- model1$df.residual  # 前組自由度
p_value <- 1 - pf(F_stat, df1, df2)

cat("F statistic =", F_stat, "\np-value =", p_value, "\n")

#d.------------------------------------------------------
library(sandwich)
library(lmtest)

# 使用 robust 標準誤估計模型
model <- lm(miles ~ income + age + kids, data = vacation)

# 計算 robust 標準誤
robust_se <- vcovHC(model, type = "HC1")
coeftest(model, vcov = robust_se)

# 建立 KIDS 的 95% 信賴區間
confint_robust <- coefci(model, vcov. = robust_se, level = 0.95)
confint_robust["kids", ]

#e.------------------------------------------------------
# 計算權重：w_i = 1 / income_i
weights <- 1 / vacation$income

# 使用 WLS 估計模型（等同於 GLS）
gls_model <- lm(miles ~ income + age + kids, data = vacation, weights = weights)

# 顯示傳統（常態）標準誤估計下的結果
summary(gls_model)

# 建立 95% 信賴區間（傳統）
confint_traditional <- confint(gls_model)["kids", ]

# 使用異質變異穩健標準誤
robust_se_gls <- vcovHC(gls_model, type = "HC1")
confint_robust <- coefci(gls_model, vcov. = robust_se_gls, level = 0.95)["kids", ]

cat("傳統 GLS 信賴區間（kids）: \n")
print(confint_traditional)
cat("\nRobust GLS 信賴區間（kids）: \n")
print(confint_robust)

