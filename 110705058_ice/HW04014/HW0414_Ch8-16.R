url <- "https://www.principlesofeconometrics.com/poe5/data/rdata/vacation.rdata"
file_path <- "vacation.rdata"
download.file(url, file_path, mode = "wb")
load(file_path)
ls()
head(vacation)

model_ols <- lm(miles ~ income + age + kids, data = vacation)  # 線性回歸
summary(model_ols)
confint(model_ols, level = 0.95)  # 信賴區間


residuals_ols <- resid(model_ols)

plot(vacation$income, residuals_ols,
     main = "OLS Residuals vs INCOME",
     xlab = "INCOME ($1000)", ylab = "Residuals",
     pch = 19, col = "lightblue")

plot(vacation$age, residuals_ols,
     main = "OLS Residuals vs AGE",
     xlab = "AGE", ylab = "Residuals",
     pch = 19, col = "lightgreen")

vacation_sorted <- vacation[order(vacation$income), ] 
vacation_head90 <- vacation_sorted[1:90, ]
vacation_tail90 <- vacation_sorted[(nrow(vacation_sorted) - 89):nrow(vacation_sorted), ]

model_low <- lm(miles ~ income + age + kids, data = vacation_head90)
model_high <- lm(miles ~ income + age + kids, data = vacation_tail90)

SSE_low <- sum(resid(model_low)^2)    # 計算殘差平方和（SSE）
SSE_high <- sum(resid(model_high)^2)

alpha <- 0.05
df <- 90 - 4  # 自由度
F_stat <- (SSE_high / df) / (SSE_low / df)  # 計算 Goldfeld-Quandt 檢驗統計量
F_critical_upper <- qf(1 - alpha, df, df)   # 計算 F 分佈右尾臨界值
F_stat
F_critical_upper


library(sandwich)   # 用於計算Robust SE
library(lmtest)   # 用於檢驗和計算Robust SE的模型摘要 for coeftest()

robust_se <- vcovHC(model_ols, type = "HC1")    # 使用異質變異穩健標準誤 (Huber-White)
coeftest(model_ols, vcov = robust_se)   # 使用 coeftest 檢視 robust t-value
coefci(model_ols, vcov. = robust_se, level = 0.95) 

vacation$weights <- 1 / (vacation$income^2)   # 計算權重

# GLS (加權最小平方法) 估計
model_gls <- lm(miles ~ income + age + kids, data = vacation, weights = weights)
summary(model_gls)
confint(model_gls, level = 0.95)  # GLS 信賴區間

# Robust SE for GLS model
gls_robust_se <- vcovHC(model_gls, type = "HC1")
coeftest(model_gls, vcov. = gls_robust_se)
coefci(model_gls, vcov. = gls_robust_se, level = 0.95)    # GLS 信賴區間（以 robust SE）














