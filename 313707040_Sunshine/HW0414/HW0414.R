#CH8Q06
# (a)設定樣本數與統計值
n_male <- 577
SSE_male <- 97161.9174
n_female <- 1000 - n_male
sigma_female_hat <- 12.024  # 注意：這裡是標準差不是變異數

# 計算女性 SSE（需先將標準差平方成變異數）
sigma2_female_hat <- sigma_female_hat^2
SSE_female <- sigma2_female_hat * (n_female - 4)  # 減去4個參數的自由度

# 計算估計變異數
sigma2_male_hat <- SSE_male / (n_male - 4)

# F 統計量（較大的放在分子）
F_stat <- max(sigma2_male_hat, sigma2_female_hat) / min(sigma2_male_hat, sigma2_female_hat)

# 臨界值與 p 值
alpha <- 0.05
df1 <- n_male - 4
df2 <- n_female - 4
F_crit_lower <- qf(alpha / 2, df1 = df1, df2 = df2)
F_crit_upper <- qf(1 - alpha/ 2, df1 = df1, df2 = df2)
p_value <- 2 * min(pf(F_stat, df1, df2), pf(F_stat, df1, df2, lower.tail = FALSE))

cat("F = ", F_stat, "\n")
cat("Critical values at 5% level (two-tailed) = [", F_crit_lower, ", ", F_crit_upper, "]\n")
cat("p-value = ", p_value, "\n")
  
#(b)
n_single <- 400
SSE_single <- 56231.0382
n_married <- 600
SSE_married <- 100703.0471

sigma2_single_hat <- SSE_single / (n_single - 5)  # 5 個變數 (含 FEMALE)
sigma2_married_hat <- SSE_married / (n_married - 5)

F_stat2 <- sigma2_married_hat / sigma2_single_hat
df1_b <- n_married - 5
df2_b <- n_single - 5
F_crit2 <- qf(1 - alpha, df1_b, df2_b)
p_value2 <- pf(F_stat2, df1_b, df2_b, lower.tail = FALSE)

cat("F = ", F_stat2, "\n")
cat("Critical value at 5% level = ", F_crit2, "\n")
cat("p-value = ", p_value2, "\n")

#(c)
NR2_stat <- 59.03
df_NR2 <- 4 # 有5個自變數
chi_crit <- qchisq(0.95, df = df_NR2)
p_value_NR2 <- pchisq(NR2_stat, df = df_NR2, lower.tail = FALSE)

cat("NR² = ", NR2_stat, "\n")
cat("Chi-squared critical value (5% level) = ", chi_crit, "\n")
cat("p-value = ", p_value_NR2, "\n")

#(d)
White_stat <- 78.82
df_white <- 20  # 根據實際模型修正
chi_white_crit <- qchisq(0.95, df_white)
p_value_white <- pchisq(White_stat, df = df_white, lower.tail = FALSE)

cat("White test statistic = ", White_stat, "\n")
cat("Critical value (5% level) = ", chi_white_crit, "\n")
cat("p-value = ", p_value_white, "\n")


#CH8Q16
library(POE5Rdata)
data(vacation)
#(a)
# 預設變數包含：MILES, INCOME, AGE, KIDS
model_a <- lm(miles ~ income + age + kids, data = vacation)
summary(model_a)

# 提取 KIDS 係數與標準誤
beta_kids <- coef(model_a)["kids"]
se_kids  <- coef(summary(model_a))["kids", "Std. Error"]
# 95% 信賴區間
ci_lower <- beta_kids + qt(0.025, df = df.residual(model_a)) * se_kids
ci_upper <- beta_kids + qt(0.975, df = df.residual(model_a)) * se_kids
cat("(a) KIDS 95% CI: [", round(ci_lower,3), ", ", round(ci_upper,3), "]\n")

#(b)繪製殘差對 INCOME 與 AGE 的散佈圖，檢查 heteroskedasticity
resid_a <- resid(model_a)
# Residuals vs INCOME
plot(vacation$income, resid_a,
     xlab = "INCOME ($1000)", ylab = "Residuals",
     main = "Residuals vs INCOME")
abline(h = 0, lty = 2)
# Residuals vs AGE
plot(vacation$age, resid_a,
     xlab = "AGE (years)", ylab = "Residuals",
     main = "Residuals vs AGE")
abline(h = 0, lty = 2)

# (c) Goldfeld-Quandt test
library(lmtest)
# 依 INCOME 排序，採用前 90 與後 90 觀察值
vac_sorted <- vacation[order(vacation$income), ]
subset1 <- vac_sorted[1:90, ]
subset2 <- vac_sorted[(nrow(vac_sorted)-89):nrow(vac_sorted), ]
# 分別估 OLS
mod1 <- lm(miles ~ income + age + kids, data = subset1)
mod2 <- lm(miles ~ income + age + kids, data = subset2)
# GQ test
gq <- gqtest(miles ~ income + age + kids, order.by = ~ income,
             data = vacation, fraction = 0.1)
print(gq)

# (d) OLS + robust standard errors，重建 KIDS 信賴區間
install.packages('sandwich')
library(sandwich)
model_d <- model_a  # 同 (a) 的模型
# 計算 robust covariance
vcov_robust <- vcovHC(model_d, type = "HC1")
se_robust <- sqrt(diag(vcov_robust))["kids"]
# robust 95% CI
tl <- qt(0.025, df = df.residual(model_d))
u  <- qt(0.975, df = df.residual(model_d))
ci_r_lower <- beta_kids + tl * se_robust
ci_r_upper <- beta_kids + u  * se_robust
cat("(d) Robust KIDS 95% CI: [", round(ci_r_lower,3), ", ", round(ci_r_upper,3), "]\n")

# (e) GLS 假設 σ_i^2 = σ^2 * INCOME_i^2
# 於 nlme::gls() 中以 weights = varFixed(~ INCOME^2)
library(nlme)
model_e <- gls(miles ~ income + age + kids,
               data = vacation,
               weights = varFixed(~ income^2))
summary(model_e)
# 傳統 GLS 標準誤已在 summary() 中顯示
# 若要 robust GLS SE，可再對 gls 物件使用 sandwich:

# 建信賴區間
beta_kids_e <- coef(model_e)["kids"]
se_kids_e   <- summary(model_e)$tTable["kids", "Std.Error"]
ci_e_lower <- beta_kids_e + qt(0.025, df = df.residual(model_a)) * se_kids_e
ci_e_upper <- beta_kids_e + qt(0.975, df = df.residual(model_a)) * se_kids_e
cat("(e) GLS KIDS 95% CI: [", round(ci_e_lower,3), ", ", round(ci_e_upper,3), "]\n")

#CH8Q18
#(a)
library(POE5Rdata)
data(cps5)
# 計算 ln(WAGE)
cps5$ln_wage <- log(cps5$wage)

# 分離男性和女性數據
males <- subset(cps5, female == 0)
females <- subset(cps5, female == 1)

# 對男性估計模型
model_males <- lm(ln_wage ~ educ + exper + I(exper^2) + black + metro + south + midwest + west, data = males)

# 對女性估計模型
model_females <- lm(ln_wage ~ educ + exper + I(exper^2) + black + metro + south + midwest + west, data = females)

# 計算殘差平方和 (RSS)
SSE_males <- sum(resid(model_males)^2)
SSE_females <- sum(resid(model_females)^2)

# 計算自由度
n_males <- nrow(males)
n_females <- nrow(females)
k <- 9  # 參數數量 (含截距)
df_males <- n_males - k
df_females <- n_females - k

# 計算測試統計量 (較大方差除以較小方差)
if (SSE_females / df_females > SSE_males / df_males) {
  F_stat <- (SSE_females / df_females) / (SSE_males / df_males)
  df1 <- df_females
  df2 <- df_males
} else {
  F_stat <- (SSE_males / df_males) / (SSE_females / df_females)
  df1 <- df_males
  df2 <- df_females
}

# 雙尾檢驗的臨界值 (5% 顯著性水平)
alpha <- 0.05
critical_value_upper <- qf(1 - alpha/2, df1, df2)
critical_value_lower <- qf(alpha/2, df1, df2)
# 計算 p 值
p_value <- 2 * (1 - pf(F_stat, df1, df2))

# 結論
if (p_value < alpha) {
  conclusion <- "拒絕 H0：方差不相等"
} else {
  conclusion <- "無法拒絕 H0：方差相等"
}

# 輸出結果
cat("測試統計量:", F_stat, "\n")
cat("臨界值 (上界):", critical_value_upper, "\n")
cat("p 值:", p_value, "\n")
cat("結論:", conclusion, "\n")

#(b)
library(lmtest)

# 使用 OLS 估計模型
model_ols <- lm(ln_wage ~ educ + exper + I(exper^2) + female + black + metro + south + midwest + west, data = cps5)

# Breusch-Pagan 測試，使用 METRO、FEMALE、BLACK
bp_test1 <- bptest(model_ols, ~ metro + female + black, data = cps5)
print("使用 METRO、FEMALE、BLACK 的 Breusch-Pagan 測試:")
print(bp_test1)

# 使用所有解釋變數進行測試
bp_test2 <- bptest(model_ols)
print("使用所有解釋變數的 Breusch-Pagan 測試:")
print(bp_test2)

# 結論 (1% 顯著性水平)
alpha <- 0.01
if (bp_test1$p.value < alpha) {
  cat("測試 1：拒絕 H0，存在異方差性\n")
} else {
  cat("測試 1：無法拒絕 H0，無異方差性\n")
}
if (bp_test2$p.value < alpha) {
  cat("測試 2：拒絕 H0，存在異方差性\n")
} else {
  cat("測試 2：無法拒絕 H0，無異方差性\n")
}

#(c)
cps5$educ_sq <- cps5$educ^2
cps5$exper_sq <- cps5$exper^2
cps5$female_black <- cps5$female * cps5$black
cps5$metro_female <- cps5$metro * cps5$female
e2 <- resid(model_ols)^2
log_e2 <- log(e2)

# ========== 輔助回歸模型 ==========
aux_model_white <- lm(log_e2 ~ educ + exper + I(exper^2) + female + black + 
                        metro + south + midwest + west +
                        educ_sq + exper_sq + female_black + metro_female,
                      data = cps5)

# ========== 統計計算 ==========
n <- nrow(cps5)
R2_white <- summary(aux_model_white)$r.squared
white_stat <- n * R2_white

df_white <- length(coef(aux_model_white)) - 1  
crit_white <- qchisq(0.95, df_white)

# ========== 結果輸出 ==========
cat("White test statistic:", round(white_stat, 4), "\n")
cat("Critical value at 5% level (df =", df_white, "):", round(crit_white, 4), "\n")

if (white_stat > crit_white) {
  cat("Conclusion: Reject the null hypothesis. Evidence of heteroskedasticity.\n")
} else {
  cat("Conclusion: Fail to reject the null hypothesis. No evidence of heteroskedasticity.\n")
}




#(d)
library(sandwich)

# 使用 White 穩健標準誤
coeftest_robust <- coeftest(model_ols, vcov = vcovHC(model_ols, type = "HC0"))
print("OLS 與 White 穩健標準誤:")
print(coeftest_robust)

# 獲取傳統標準誤
summary_ols <- summary(model_ols)
conventional_se <- summary_ols$coefficients[, "Std. Error"]
robust_se <- sqrt(diag(vcovHC(model_ols, type = "HC0")))

# 比較標準誤
cat("\n傳統標準誤 vs 穩健標準誤:\n")
comparison <- data.frame(Conventional = conventional_se, Robust = robust_se)
print(comparison)

# 檢查顯著性變化
sig_conventional <- summary_ols$coefficients[, "Pr(>|t|)"] < 0.05
sig_robust <- coeftest_robust[, "Pr(>|t|)"] < 0.05
cat("\n顯著性變化:\n")
print(data.frame(Conventional = sig_conventional, Robust = sig_robust))

#(e)
# 獲取 OLS 殘差平方
e2 <- resid(model_ols)^2
log_e2 <- log(e2)

# 回歸 log(e^2) 到 METRO 和 EXPER
aux_model_fgls <- lm(log_e2 ~ metro + exper, data = cps5)

# 計算權重
log_h_hat <- predict(aux_model_fgls)
weights <- 1 / exp(log_h_hat)

# 進行加權最小二乘法 (FGLS)
model_fgls <- lm(ln_wage ~ educ + exper + I(exper^2) + female + black + metro + south + midwest + west, 
                 data = cps5, weights = weights)

# 輸出結果
summary_fgls <- summary(model_fgls)
print("FGLS 估計:")
print(summary_fgls)

# 與 OLS 穩健標準誤比較
fgls_se <- summary_fgls$coefficients[, "Std. Error"]
cat("\nFGLS 標準誤 vs OLS 穩健標準誤:\n")
print(data.frame(FGLS = fgls_se, OLS_Robust = robust_se))

#(f)
# 計算 FGLS 的穩健標準誤
coeftest_fgls_robust <- coeftest(model_fgls, vcov = vcovHC(model_fgls, type = "HC0"))
print("FGLS 與穩健標準誤:")
print(coeftest_fgls_robust)

# 提取標準誤
robust_se_fgls <- sqrt(diag(vcovHC(model_fgls, type = "HC0")))

# 比較
cat("\nFGLS 穩健標準誤 vs FGLS 標準誤 vs OLS 穩健標準誤:\n")
print(data.frame(FGLS_Robust = robust_se_fgls, FGLS = fgls_se, OLS_Robust = robust_se))

#(g)
# 假設根據測試結果進行選擇
if (bp_test2$p.value < 0.01 || LM_white > critical_value_white) {
  cat("存在異方差性，建議使用 FGLS 與穩健標準誤 (部分 f)，因為它考慮了異方差性並提供有效估計。\n")
} else {
  cat("無顯著異方差性，OLS 即可 (部分 d)。\n")
}

# 比較標準誤大小
se_comparison <- data.frame(OLS_Robust = robust_se, FGLS = fgls_se, FGLS_Robust = robust_se_fgls)
print("標準誤比較:")
print(se_comparison)
