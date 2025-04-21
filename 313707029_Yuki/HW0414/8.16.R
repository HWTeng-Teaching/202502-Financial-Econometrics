# 清除環境變數（可選）
rm(list = ls())

# 安裝並載入所需套件（第一次用時取消註解安裝）
# install.packages(c("sandwich","lmtest","car","ggplot2","dplyr","POE5Rdata","tseries"))
library(POE5Rdata)  # vacation 資料
library(dplyr)      
library(ggplot2)    
library(sandwich)   # vcovHC()
library(lmtest)     # coeftest()
library(car)        # for some helper functions, 如需

# 載入資料
data("vacation")

#------------------------------------------------
# (a) OLS 回歸並建構 kids 係數的 95% 常規信賴區間
#------------------------------------------------
ols_model <- lm(miles ~ kids + income + age, data = vacation)
summary(ols_model)

# 95% CI for the coefficient on 'kids'
ci_a <- confint(ols_model, "kids", level = 0.95)
print("=== (a) 常規 95% CI for kids ===")
print(ci_a)

#------------------------------------------------
# (b) 畫殘差對 income 及 age 的散佈圖
#------------------------------------------------
vacation$resid <- resid(ols_model)

# 殘差 vs income
p1 <- ggplot(vacation, aes(x = income, y = resid)) +
  geom_point(alpha = 0.6) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(title = "Residuals vs. Income", x = "Income", y = "OLS Residuals")
print(p1)

# 殘差 vs age
p2 <- ggplot(vacation, aes(x = age, y = resid)) +
  geom_point(alpha = 0.6) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(title = "Residuals vs. Age", x = "Age", y = "OLS Residuals")
print(p2)

#------------------------------------------------
# (c) Goldfeld–Quandt 檢定
#    H0: σ²_low = σ²_high
#    H1: σ²_high > σ²_low
#------------------------------------------------
# 1. 依 income 排序
vac_sorted <- vacation %>% arrange(income)

# 2. 拆前 90 與後 90
low_group  <- vac_sorted[1:90, ]
high_group <- vac_sorted[(nrow(vac_sorted)-89):nrow(vac_sorted), ]

# 3. 各自 OLS
model_low  <- lm(miles ~ kids + income + age, data = low_group)
model_high <- lm(miles ~ kids + income + age, data = high_group)

# 4. SSE 與 df
SSE_low  <- sum(resid(model_low)^2);  df_low  <- df.residual(model_low)
SSE_high <- sum(resid(model_high)^2); df_high <- df.residual(model_high)

# 5. F 統計量（右尾）
F_stat <- (SSE_high/df_high) / (SSE_low/df_low)
F_crit <- qf(0.95, df1 = df_high, df2 = df_low)

cat("=== (c) Goldfeld–Quandt Test ===\n")
cat(sprintf("F_stat = %.4f  (df1=%d, df2=%d)\n", F_stat, df_high, df_low))
cat(sprintf("F_crit(0.95, %d, %d) = %.4f\n", df_high, df_low, F_crit))
if(F_stat > F_crit) {
  cat("Result: Reject H0 → 存在異質變異\n\n")
} else {
  cat("Result: Fail to reject H0 → 無法發現異質變異\n\n")
}

#------------------------------------------------
# (d) 以 robust 標準誤重估 OLS，並算 kids 的 95% CI
#------------------------------------------------
# 1. 先取出 HC1 的 vcov 矩陣
vcov_mat <- vcovHC(ols_model, type = "HC1")

# 2. 從矩陣中取對角線（各係數的變異數），再開根號得到所有係數的 robust SE
se_robust_all <- sqrt(diag(vcov_mat))
# names(se_robust_all)  # 可以檢查一下有 "Intercept","kids","income","age"

# 3. 把 kids 的 robust SE 抽出來
se_robust_kids <- se_robust_all["kids"]

# 4. 取出 kids 的 OLS 估計值
beta_kids <- coef(ols_model)["kids"]

# 5. 計算自由度與臨界 t 值
n     <- nrow(vacation)
k     <- length(coef(ols_model))
df    <- n - k
tcrit <- qt(0.975, df)

# 6. 算出 95% robust‐SE 信賴區間
CI_d <- beta_kids + c(-1, 1) * tcrit * se_robust_kids
names(CI_d) <- c("Lower95","Upper95")

# 7. 列印結果
cat("=== (d) kids 的 95% robust‐SE 信賴區間 ===\n")
print(CI_d)

# 8. 如果想一併看到 (a) 的常規 CI
cat("\n=== (a) kids 的 95% 常規信賴區間 ===\n")
print(ci_a)

#------------------------------------------------
# (e) GLS (WLS)：假設 σ_i² = σ²·income_i²
#------------------------------------------------

# 1. 建立加權回歸模型：weights = 1/income^2
wls_model <- lm(miles ~ kids + income + age,
                data    = vacation,
                weights = 1/(income^2))

# 2. 檢視 WLS 回歸摘要（常規 SE）
summary(wls_model)

# 3. 用 confint() 取出 kids 的 95% 常規 CI（GLS）
ci_gls_conv <- confint(wls_model, "kids", level = 0.95)

#------------------------------------------------
# 4. 計算 GLS (WLS) 的 robust SE
#------------------------------------------------
library(sandwich)
library(lmtest)

# 4‑a. 先拿 vcovHC 的對角線
vcov_wls_hc1 <- vcovHC(wls_model, type = "HC1")
se_wls_rob   <- sqrt(diag(vcov_wls_hc1))["kids"]

# 4‑b. 取出 kids 的估計值
beta_kids_wls <- coef(wls_model)["kids"]

# 4‑c. t 臨界值（同 a,d）
n    <- nrow(vacation)
k    <- length(coef(wls_model))
df   <- n - k
tcrit <- qt(0.975, df)

# 4‑d. robust‐SE 下的 95% CI
ci_gls_robust <- beta_kids_wls + c(-1,1)*tcrit*se_wls_rob
names(ci_gls_robust) <- c("Lower95","Upper95")

#------------------------------------------------
# 5. 列印並比較所有區間
#------------------------------------------------
cat("=== (a) 常規 OLS 95% CI for kids ===\n")
print(ci_a)         # 來自前面 (a)

cat("\n=== (d) 95% Robust‐SE CI for kids ===\n")
print(CI_d)         # 來自前面 (d)

cat("\n=== (e) GLS 常規 SE 95% CI for kids ===\n")
print(ci_gls_conv)

cat("\n=== (e) GLS Robust‐SE 95% CI for kids ===\n")
print(ci_gls_robust)
