if (!requireNamespace("remotes", quietly = TRUE)) install.packages("remotes")
remotes::install_github("ccolonescu/POE5Rdata", force = TRUE)

library(POE5Rdata)
data('cps5')
data <- cps5  
# (a以F統計量檢驗不同性別之標準差
# 資料分割(subset)
data_male <- subset(data, female == 0)
data_female <- subset(data, female == 1)

model_male <- lm(log(wage) ~ educ + exper + I(exper^2) + black + 
                   metro + south + midwest + west, data = data_male)
model_female <- lm(log(wage) ~ educ + exper + I(exper^2) + black + 
                     metro + south + midwest + west, data = data_female)
summary(model_male)
summary(model_female)

sse_male <- sum(model_male$residuals^2)
sse_female <- sum(model_female$residuals^2)
df_male <- model_male$df.residual
df_female <- model_female$df.residual

# 計算F統計量 (方差比)
F_stat <- (sse_male/df_male) / (sse_female/df_female)

# 計算 F 分佈臨界值（5%顯著水準）
F_critical_upper <- qf(1 - 0.05/2, df_male, df_female) 
F_critical_lower <- qf(0.05/2, df_male, df_female) 
cat("F統計量值：", F_stat,"\n")
cat("5%顯著水準的臨界值：", F_critical_lower,"和", F_critical_upper, "\n")


# (b)用居住地、膚色、性別來檢驗異質性
model_ols <- lm(log(wage) ~ educ + exper + I(exper^2) + female + black + 
                  metro + south + midwest + west, data = data)
summary(model_ols)

residuals_squared <- resid(model_ols)^2   # 獲取殘差平方

# 特定變數回歸
aux_model <- lm(residuals_squared ~ metro + female + black, data = data)

# NR^2 測試統計量
n <- nobs(model_ols)  # 樣本數
R2 <- summary(aux_model)$r.squared  # 輔助回歸的 R^2
NR2 <- n * R2

critical_value <- qchisq(1-0.01, df=length(coef(aux_model)) - 1)  

aux_model_all <- lm(residuals_squared ~ educ + exper + I(exper^2) + female + black + 
                      metro + south + midwest + west, data = data)

# NR^2 測試統計量
R2_all <- summary(aux_model_all)$r.squared
NR2_all <- n * R2_all

# 臨界值（自由度=9，1%顯著水準）
critical_value_all <- qchisq(1-0.01, df=length(coef(aux_model_all)) - 1)  

cat("使用 METRO、FEMALE、BLACK: \n")
cat("NR^2:", NR2, "\n")
cat("1%顯著水準的χ²臨界值 =", critical_value, "\n")
cat("使用所有解釋變數: \n")
cat("NR^2:", NR2_all, "\n")
cat("1%顯著水準的χ²臨界值 =", critical_value_all, "\n")


# 8.18(c)
library(lmtest)  # 提供 bptest() 函數（White 檢定）

# 進行 White 檢定，輔助回歸包含原始變數、平方項和所有交互項
white_test <- bptest(model_ols,
                     ~ educ + exper + I(exper^2) + female + black + metro + south + midwest + west +
                       I(educ^2) + I(exper^4) + 
                       educ:exper + educ:female + educ:black + educ:metro + educ:south + educ:midwest + educ:west + educ:I(exper^2) +
                       exper:female + exper:black + exper:metro + exper:south + exper:midwest + exper:west + exper:I(exper^2) +
                       female:black + female:metro + female:south + female:midwest + female:west + female:I(exper^2) + 
                       black:metro + black:south + black:midwest + black:west + black:I(exper^2) + 
                       metro:south + metro:midwest + metro:west + metro:I(exper^2) +
                       south:I(exper^2) + midwest:I(exper^2) + west:I(exper^2),
                     data = data)

print(white_test)

# 臨界值（5%顯著水準）
df_white <- white_test$parameter  # 使用 bptest 中的自由度
critical_value_white <- qchisq(1-0.05, df_white)

cat("White 檢定 NR^2:", white_test$statistic, "\n")
cat("自由度:", white_test$parameter, "\n")
cat("5% 顯著水準的臨界值:", critical_value_white, "\n")


# 8.18(d)
library(sandwich)   # 用於計算Robust SE
library(lmtest)     # for coeftest()

# White 穩健標準誤
robust_results <- coeftest(model_ols, vcov = vcovHC(model_ols, type = "HC0"))
print(robust_results)

# 比較標準誤、信賴區間(OLS vs Robust OLS)
se_ols <- summary(model_ols)$coefficients[, "Std. Error"]   # OLS 標準誤
ci_ols <- confint(model_ols)   # OLS 信賴區間
se_ols_robust <- sqrt(diag(vcovHC(model_ols, type = "HC0")))      # OLS 標準誤（以 robust SE）
ci_ols_robust <- coefci(model_ols, vcov. = vcovHC(model_ols, type = "HC0"))   # OLS 信賴區間（以 robust SE）

comparison_d <- data.frame(
  OLS_SE = se_ols,
  OLS_Robust_SE = se_ols_robust,
  SE_Change = ifelse(se_ols_robust > se_ols, "變大", 
                     ifelse(se_ols_robust < se_ols, "變小", "不變")),
  OLS_Width = ci_ols[,2] - ci_ols[,1],
  OLS_Robust_Width = ci_ols_robust[,2] - ci_ols_robust[,1],
  CI_Change = ifelse((ci_ols[,2] - ci_ols[,1]) < (ci_ols_robust[,2] - ci_ols_robust[,1]), "變寬", 
                     ifelse((ci_ols[,2] - ci_ols[,1]) > (ci_ols_robust[,2] - ci_ols_robust[,1]), "變窄", "不變"))
)
print(comparison_d)

# (e) Metro、EXPER之FGL估計
log_resid_sq <- log(resid(model_ols)^2)   
model_var <- lm(log_resid_sq ~ metro + exper, data = data)
summary(model_var)

sigma_sq <- exp(fitted(model_var))
weights <- 1/sqrt(sigma_sq)

# FGLS model
model_fgls <- lm(log(wage) ~ educ + exper + I(exper^2) + female + black + 
                   metro + south + midwest + west, data = data, weights = weights)
summary(model_fgls)

# 比較標準誤、信賴區間(Robust OLS vs FGLS)
se_fgls <- summary(model_fgls)$coefficients[, "Std. Error"]     # FGLS 標準誤
ci_fgls <- confint(model_fgls)   # FGLS 信賴區間
comparison_e <- data.frame(
  OLS_Robust_SE = se_ols_robust,
  FGLS_SE = se_fgls,
  SE_Change = ifelse(se_ols_robust > se_fgls, "變小", 
                     ifelse(se_ols_robust < se_fgls, "變大", "不變")),
  OLS_Robust_Width = ci_ols_robust[,2] - ci_ols_robust[,1],
  FGLS_Width = ci_fgls[,2] - ci_fgls[,1],
  CI_Change = ifelse((ci_fgls[,2] - ci_fgls[,1]) < (ci_ols_robust[,2] - ci_ols_robust[,1]), "變窄", 
                     ifelse((ci_fgls[,2] - ci_fgls[,1]) > (ci_ols_robust[,2] - ci_ols_robust[,1]), "變寬", "不變"))
)
print(comparison_e)

# (f)
# Robust SE for FGLS model
coeftest(model_fgls, vcov. = vcovHC(model_fgls, type = "HC0"))
se_fgls_robust <- sqrt(diag(vcovHC(model_fgls, type = "HC0")))      # FGLS 標準誤（以 robust SE）
ci_fgls_robust <- coefci(model_fgls, vcov. = vcovHC(model_fgls, type = "HC0"))    # FGLS 信賴區間（以 robust SE）

# 比較標準誤、信賴區間(Robust OLS vs Robust FGLS)
comparison_f1 <- data.frame(
  Variable = rownames(ci_ols_robust),
  OLS_Robust_SE = se_ols_robust,
  FGLS_Robust_SE = se_fgls_robust,
  SE_Change = ifelse(se_ols_robust > se_fgls_robust, "變小", 
                     ifelse(se_ols_robust < se_fgls_robust, "變大", "不變")),
  OLS_Robust_Width = ci_ols_robust[,2] - ci_ols_robust[,1],
  FGLS_Robust_Width = ci_fgls_robust[,2] - ci_fgls_robust[,1],
  CI_Change = ifelse((ci_fgls_robust[,2] - ci_fgls_robust[,1]) < (ci_ols_robust[,2] - ci_ols_robust[,1]), "變窄", 
                     ifelse((ci_fgls_robust[,2] - ci_fgls_robust[,1]) > (ci_ols_robust[,2] - ci_ols_robust[,1]), "變寬", "不變"))
)
print(comparison_f1)

# 比較標準誤、信賴區間(FGLS vs Robust FGLS)
ci_comparison_f2 <- data.frame(
  FGLS_SE = se_fgls,
  FGLS_Robust_SE = se_fgls_robust,
  SE_Change = ifelse(se_fgls > se_fgls_robust, "變小", 
                     ifelse(se_fgls < se_fgls_robust, "變大", "不變")),
  FGLS_Width = ci_fgls[,2] - ci_fgls[,1],
  FGLS_Robust_Width = ci_fgls_robust[,2] - ci_fgls_robust[,1],
  CI_Change = ifelse((ci_fgls[,2] - ci_fgls[,1]) < (ci_fgls_robust[,2] - ci_fgls_robust[,1]), "變寬", 
                     ifelse((ci_fgls[,2] - ci_fgls[,1]) > (ci_fgls_robust[,2] - ci_fgls_robust[,1]), "變窄", "不變"))
)
print(ci_comparison_f2)