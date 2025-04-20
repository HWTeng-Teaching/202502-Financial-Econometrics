#8.6
#a
qf(0.975, df1 = 573, df2 = 419)
qf(0.025, df1 = 573, df2 = 419)
#b
qf(0.975, df1 = 595, df2 = 395)
qf(0.025, df1 = 595, df2 = 395)
#c
qchisq(0.95, 4)
#d
qchisq(0.95, 12)

#8.16
#a
remotes::install_github("ccolonescu/POE5Rdata", force = TRUE)
library(POE5Rdata)
data(vacation)

mod1 <- lm(miles ~ income + age + kids, data = vacation)
summary(mod1)
confint(mod1, "kids", level = 0.95)

#b
residuals_ols <- resid(mod1)

plot(vacation$income, residuals_ols, main = "Residuals vs Income", xlab = "Income", ylab = "Residuals")
abline(h = 0, col = "red")

plot(vacation$age, residuals_ols, main = "Residuals vs Age", xlab = "Age", ylab = "Residuals")
abline(h = 0, col = "blue")

#c
vacation_sorted <- vacation[order(vacation$income),]
group1 <- vacation_sorted[1:90,]
group2 <- vacation_sorted[111:200,]
mod1_group1 <- lm(miles ~ income + age + kids, data = group1)
mod1_group2 <- lm(miles ~ income + age + kids, data = group2)

library(lmtest)
gq_test <- bptest(mod1_group1, mod1_group2, data = vacation_sorted)  
gq_test
SSE_group1 <- sum(resid(mod1_group1)^2)  
SSE_group2 <- sum(resid(mod1_group2)^2)

alpha <- 0.05
df <- 90 - 4  
F_stat <- (SSE_group2 / df) / (SSE_group1 / df)  
F_critical_upper <- qf(1 - alpha, df, df)   

library(lmtest)   
gq_result <- gqtest(miles ~ income + age + kids, data = vacation_sorted,
                    order.by = ~ income, alternative="greater",
                    fraction = 0.1)  

print(gq_result)


library(lmtest)
gqtest(mod1, order.by = ~income, data = vacation, fraction = 0.1)
qf(0.95, df1 = 86, df2 = 86)

#d
robust_se <- sqrt(diag(vcovHC(mod1, type = "HC1")))
estimate_kids <- coef(mod1)["kids"]
robust_se_kids <- robust_se["kids"]
lower_bound <- estimate_kids - 1.96 * robust_se_kids
upper_bound <- estimate_kids + 1.96 * robust_se_kids
c(lower_bound, upper_bound)

#e
weights <- 1 / (vacation$income^2)  

model_gls <- lm(miles ~ income + age + kids, data = vacation, weights = weights)
summary(model_gls)
confint(model_gls, level = 0.95)  

gls_robust_se <- vcovHC(model_gls, type = "HC1")
coeftest(model_gls, vcov. = gls_robust_se)
coefci(model_gls, vcov. = gls_robust_se, level = 0.95)    

#8.18
#a
remotes::install_github("ccolonescu/POE5Rdata", force = TRUE)
library(POE5Rdata)
data(cps5)

male <- subset(cps5, female == 0)
female <- subset(cps5, female == 1)

mod_m <- lm(log(wage) ~ educ + exper + I(exper^2) + black + metro + south + midwest + west, data = male)
mod_f <- lm(log(wage) ~ educ + exper + I(exper^2) + black + metro + south + midwest + west, data = female)
RSS_m <- sum(resid(mod_m)^2)
RSS_f <- sum(resid(mod_f)^2)
RSS_m
RSS_f
gq_stat <- (RSS_m/df.residual(mod_m)) /(RSS_f/df.residual(mod_f))
gq_stat
qf(0.025, df1 = df.residual(mod_m), df2 = df.residual(mod_f))  
qf(0.975, df1 = df.residual(mod_m), df2 = df.residual(mod_f))  

#b
mod_ols <- lm(log(wage) ~ educ + exper + I(exper^2) + female + black + metro + south + midwest + west, data = cps5)
u2 <- resid(mod_ols)^2

aux_model <- lm(u2 ~ metro + female + black, data = cps5)

n <- nobs(mod_ols)
R2 <- summary(aux_model)$r.squared  
NR2 <- n * R2
critical_value <- qchisq(1-0.01, df=length(coef(aux_model)) - 1)  
NR2
critical_value

aux_model_all <- lm(u2 ~ educ + exper + I(exper^2) + female + black + metro + south + midwest + west, data = cps5)
R2_all <- summary(aux_model_all)$r.squared
NR2_all <- n * R2_all
critical_value_all <- qchisq(1-0.01, df=length(coef(aux_model_all)) - 1)  
NR2_all
critical_value_all

#c
library(lmtest)
bptest(mod_ols, ~ educ + exper + I(exper^2) + female + black + metro + south + midwest + west +
                       I(educ^2) + I(exper^4) + educ:exper + educ:female + educ:black + educ:metro + educ:south + educ:midwest + educ:west + educ:I(exper^2) +
                       exper:female + exper:black + exper:metro + exper:south + exper:midwest + exper:west + exper:I(exper^2) +
                       female:black + female:metro + female:south + female:midwest + female:west + female:I(exper^2) + 
                       black:metro + black:south + black:midwest + black:west + black:I(exper^2) + 
                       metro:south + metro:midwest + metro:west + metro:I(exper^2) +
                       south:I(exper^2) + midwest:I(exper^2) + west:I(exper^2),
                     data = cps5)
#d
library(sandwich)   
library(lmtest)    

summary(mod_ols)  
robust_results <- coeftest(mod_ols, vcov = vcovHC(mod_ols, type = "HC0"))
print(robust_results)

se_ols <- summary(mod_ols)$coefficients[, "Std. Error"]   # OLS 標準誤
ci_ols <- confint(mod_ols)   # OLS 信賴區間
se_ols_robust <- sqrt(diag(vcovHC(mod_ols, type = "HC0")))      # OLS 標準誤（以 robust SE）
ci_ols_robust <- coefci(mod_ols, vcov. = vcovHC(mod_ols, type = "HC0"))   # OLS 信賴區間（以 robust SE）
ci_ols
ci_ols_robust

comparison_d <- data.frame(
  OLS_Width = ci_ols[,2] - ci_ols[,1],
  OLS_Robust_Width = ci_ols_robust[,2] - ci_ols_robust[,1],
  CI_Change = ifelse((ci_ols[,2] - ci_ols[,1]) < (ci_ols_robust[,2] - ci_ols_robust[,1]), "wider", 
                     ifelse((ci_ols[,2] - ci_ols[,1]) > (ci_ols_robust[,2] - ci_ols_robust[,1]), "narrower", "same")))
print(comparison_d)

p_ols <- summary(mod_ols)$coefficients[, "Pr(>|t|)"]  
p_ols_robust <- robust_results[, "Pr(>|t|)"]    

significance_comparison <- data.frame(
  OLS_p = p_ols,
  OLS_Significant = p_ols < 0.05,
  OLS_Robust_p = p_ols_robust,
  OLS_Robust_Significant = p_ols_robust < 0.05
)
print(significance_comparison)

#e
residuals_squared <- resid(mod_ols)^2
log_resid_sq <- log(residuals_squared)   
model_var <- lm(log_resid_sq ~ metro + exper, data = cps5)

sigma_sq <- exp(fitted(model_var))
weights <- 1/sqrt(sigma_sq)

mod_fgls <- lm(log(wage) ~ educ + exper + I(exper^2) + female + black + 
                   metro + south + midwest + west, data = cps5, weights = weights)
summary(mod_fgls)

se_fgls <- summary(model_fgls)$coefficients[, "Std. Error"]    
ci_fgls <- confint(model_fgls)  
comparison_e <- data.frame(
  OLS_Robust_Width = ci_ols_robust[,2] - ci_ols_robust[,1],
  FGLS_Width = ci_fgls[,2] - ci_fgls[,1],
  CI_Change = ifelse((ci_fgls[,2] - ci_fgls[,1]) < (ci_ols_robust[,2] - ci_ols_robust[,1]), "narrower", 
                     ifelse((ci_fgls[,2] - ci_fgls[,1]) > (ci_ols_robust[,2] - ci_ols_robust[,1]), "wider", "same"))
)
print(comparison_e)

#f
coeftest(model_fgls, vcov. = vcovHC(model_fgls, type = "HC0"))
se_fgls_robust <- sqrt(diag(vcovHC(model_fgls, type = "HC0")))      # FGLS 標準誤（以 robust SE）
ci_fgls_robust <- coefci(model_fgls, vcov. = vcovHC(model_fgls, type = "HC0"))    # FGLS 信賴區間（以 robust SE）

comparison_f1 <- data.frame(
  OLS_Robust_Width = ci_ols_robust[,2] - ci_ols_robust[,1],
  FGLS_Robust_Width = ci_fgls_robust[,2] - ci_fgls_robust[,1],
  CI_Change = ifelse((ci_fgls_robust[,2] - ci_fgls_robust[,1]) < (ci_ols_robust[,2] - ci_ols_robust[,1]), "narrower", 
                     ifelse((ci_fgls_robust[,2] - ci_fgls_robust[,1]) > (ci_ols_robust[,2] - ci_ols_robust[,1]), "wider", "same"))
)
print(comparison_f1)

ci_comparison_f2 <- data.frame(
  FGLS_Width = ci_fgls[,2] - ci_fgls[,1],
  FGLS_Robust_Width = ci_fgls_robust[,2] - ci_fgls_robust[,1],
  CI_Change = ifelse((ci_fgls[,2] - ci_fgls[,1]) < (ci_fgls_robust[,2] - ci_fgls_robust[,1]), "wider", 
                     ifelse((ci_fgls[,2] - ci_fgls[,1]) > (ci_fgls_robust[,2] - ci_fgls_robust[,1]), "narrower", "same"))
)
print(ci_comparison_f2)
