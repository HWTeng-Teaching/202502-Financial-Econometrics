remotes::install_github("ccolonescu/POE5Rdata", force = TRUE)
library(POE5Rdata)
data("vacation")
library(ggplot2)
summary(vacation)

#8.16.a
model_ols <- lm(miles ~ income + age + kids, data = vacation)  
summary(model_ols)
confint(model_ols, level = 0.95)

#8.16.b
residuals_ols <- resid(model_ols)

plot(vacation$income, residuals_ols,
     main = "Residuals vs Income",
     xlab = "Income (in $1000s)",
     ylab = "Residuals",
     pch = 19, col = "steelblue")
abline(h = 0, col = "red", lwd = 2)

plot(vacation$age, residuals_ols,
     main = "Residuals vs Age",
     xlab = "Age (years)",
     ylab = "Residuals",
     pch = 19, col = "darkgreen")
abline(h = 0, col = "red", lwd = 2)

#8.16.c
# Step 1: 依 income 
data_sorted <- vacation[order(vacation$income), ]

n <- nrow(data_sorted)
data_low <- data_sorted[1:90, ]
data_high <- data_sorted[(n - 89):n, ]

model_low <- lm(miles ~ income + age + kids, data = data_low)
model_high <- lm(miles ~ income + age + kids, data = data_high)

summary(model_low)
summary(model_high)
 
SSE_low <- sum(resid(model_low)^2)
SSE_high <- sum(resid(model_high)^2)

alpha <- 0.05
df <- 90 - 4  

F_stat <- (SSE_high / df) / (SSE_low / df)
F_critical_upper <- qf(1 - alpha, df, df)  

cat("Manual F-statistic: ", F_stat, "\n")
cat("Critical value (5% level): ", F_critical_upper, "\n")

install.packages("lmtest")
library(lmtest)

gq_result <- gqtest(miles ~ income + age + kids, data = data_sorted,
                    order.by = ~ income, alternative = "greater",
                    fraction = 0.1)  
print(gq_result)

8.16.d
install.packages("sandwich")
library(sandwich)   
library(lmtest)     

robust_se <- vcovHC(model_ols, type = "HC1")
coeftest(model_ols, vcov = robust_se)
coefci(model_ols, vcov. = robust_se, level = 0.95)["kids", ]

#8.16.e
vacation$weights <- 1 / (vacation$income^2)   

model_gls <- lm(miles ~ income + age + kids, data = vacation, weights = weights)
summary(model_gls)
confint(model_gls, level = 0.95) 
gls_robust_se <- vcovHC(model_gls, type = "HC1")
coeftest(model_gls, vcov. = gls_robust_se)
coefci(model_gls, vcov. = gls_robust_se, level = 0.95)

#8.18.a
data(cps5)
model <- lm(log(wage) ~ educ + exper + I(exper^2) + female + black + 
              metro + south + midwest + west, data = cps5)

cps5$resid_sq <- resid(model)^2

male_data <- subset(cps5, female == 0)
female_data <- subset(cps5, female == 1)

RSS_male <- sum(male_data$resid_sq)
RSS_female <- sum(female_data$resid_sq)

df_male <- nrow(male_data) - 10  
df_female <- nrow(female_data) - 10

F_stat <- (RSS_male / df_male) / (RSS_female / df_female)

alpha <- 0.05
F_crit_upper <- qf(1 - alpha / 2, df_male, df_female)
F_crit_lower <- 1 / F_crit_upper

cat("F statistic:", round(F_stat, 4), "\n")
cat("Critical region: F <", round(F_crit_lower, 4), "or F >", round(F_crit_upper, 4), "\n")

if (F_stat < F_crit_lower | F_stat > F_crit_upper) {
  cat("Conclusion: Reject the null hypothesis. Error variances differ significantly between males and females.\n")
} else {
  cat("Conclusion: Fail to reject the null hypothesis. No significant difference in error variances.\n")
}

#8.18.b
# ------------ NR² Test using selected variables ------------
aux_model_selected <- lm(resid_sq ~ metro + female + black, data = cps5)
R2_selected <- summary(aux_model_selected)$r.squared

n <- nrow(cps5)
df_selected <- 3  
NR2_selected <- n 
crit_val_selected <- qchisq(0.99, df_selected)

cat("NR² test statistic (selected variables):", round(NR2_selected, 4), "\n")
cat("Critical value (1% level, df = 3):", round(crit_val_selected, 4), "\n")

if (NR2_selected > crit_val_selected) {
  cat("Conclusion: Reject H0. Evidence of heteroskedasticity (selected variables).\n")
} else {
  cat("Conclusion: Fail to reject H0. No strong evidence of heteroskedasticity.\n")
}

# ------------ NR² Test using all explanatory variables ------------
aux_model_all <- lm(resid_sq ~ educ + exper + I(exper^2) + female + black + 
                      metro + south + midwest + west, data = cps5)
R2_all <- summary(aux_model_all)$r.squared
df_all <- 9  
NR2_all <- n * R2_all
crit_val_all <- qchisq(0.99, df_all)

cat("\nNR² test statistic (all variables):", round(NR2_all, 4), "\n")
cat("Critical value (1% level, df = 9):", round(crit_val_all, 4), "\n")

if (NR2_all > crit_val_all) {
  cat("Conclusion: Reject H0. Strong evidence of heteroskedasticity (all variables).\n")
} else {
  cat("Conclusion: Fail to reject H0. No strong evidence of heteroskedasticity.\n")
}

# ========== White Test 擴增變數 ==========
cps5$educ_sq <- cps5$educ^2
cps5$exper_sq <- cps5$exper^2
cps5$female_black <- cps5$female * cps5$black
cps5$metro_female <- cps5$metro * cps5$female

# ========== 輔助回歸模型 ==========
aux_model_white <- lm(resid_sq ~ educ + exper + I(exper^2) + female + black + 
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

#8.18.c
# ========== White Test 擴增變數 ==========
cps5$educ_sq <- cps5$educ^2
cps5$exper_sq <- cps5$exper^2
cps5$female_black <- cps5$female * cps5$black
cps5$metro_female <- cps5$metro * cps5$female

# ========== 輔助回歸模型 ==========
aux_model_white <- lm(resid_sq ~ educ + exper + I(exper^2) + female + black + 
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

#8.18.d
# ---------- 計算 OLS 與 Robust (White) 標準誤的信賴區間 ----------
library(sandwich)
library(lmtest)

robust_results <- coeftest(model_ols, vcov = vcovHC(model, type = "HC0"))
print(robust_results)

# 比較標準誤、信賴區間(OLS vs Robust OLS)
se_ols <- summary(model)$coefficients[, "Std. Error"]  
ci_ols <- confint(model)  
se_ols_robust <- sqrt(diag(vcovHC(model, type = "HC0")))      
ci_ols_robust <- coefci(model, vcov. = vcovHC(model, type = "HC0"))  

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

log_resid_sq <- log(resid(model_ols)^2)   
model_var <- lm(log_resid_sq ~ metro + exper, data = data)
summary(model_var)

sigma_sq <- exp(fitted(model_var))
weights <- 1/sqrt(sigma_sq)

model_fgls <- lm(log(wage) ~ educ + exper + I(exper^2) + female + black + 
                   metro + south + midwest + west, data = data, weights = weights)
summary(model_fgls)

se_fgls <- summary(model_fgls)$coefficients[, "Std. Error"]    
ci_fgls <- confint(model_fgls)  
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

#8.18.f
log_resid_sq <- log(resid(model)^2)   
model_var <- lm(log_resid_sq ~ metro + exper, data = cps5)
summary(model_var)

sigma_sq <- exp(fitted(model_var))
weights <- 1/sqrt(sigma_sq)

model_fgls <- lm(log(wage) ~ educ + exper + I(exper^2) + female + black + 
                   metro + south + midwest + west, data = cps5, weights = weights)
summary(model_fgls)

se_fgls <- summary(model_fgls)$coefficients[, "Std. Error"]    
ci_fgls <- confint(model_fgls)  
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

#8.18.f
coeftest(model_fgls, vcov. = vcovHC(model_fgls, type = "HC0"))
se_fgls_robust <- sqrt(diag(vcovHC(model_fgls, type = "HC0")))     
ci_fgls_robust <- coefci(model_fgls, vcov. = vcovHC(model_fgls, type = "HC0"))    

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
