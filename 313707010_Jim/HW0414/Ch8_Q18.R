library(POE5Rdata)
library(lmtest)
library(sandwich)

data("cps5")

# (a)
model_ols <- lm(log(wage) ~ educ + exper + I(exper^2) + female + black + 
                  metro + south + midwest + west, data = cps5)

data_male <- subset(cps5, female == 0)
data_female <- subset(cps5, female == 1)

model_male <- lm(log(wage) ~ educ + exper + I(exper^2) + black + 
                   metro + south + midwest + west, data = data_male)
model_female <- lm(log(wage) ~ educ + exper + I(exper^2) + black + 
                     metro + south + midwest + west, data = data_female)

sse_male <- sum(model_male$residuals^2)
sse_female <- sum(model_female$residuals^2)
df_male <- model_male$df.residual
df_female <- model_female$df.residual

F_stat <- (sse_male/df_male)/(sse_female/df_female)

F_critical_upper <- qf(1 - 0.05/2, df_male, df_female) 
F_critical_lower <- qf(0.05/2, df_male, df_female) 

cat("df：", df_male, "&", df_female, "\n")
cat("F-test statistic：", F_stat,"\n")
cat("5% thres：", F_critical_lower," and ", F_critical_upper, "\n")

# (b)
main_model <- lm(log(wage) ~ educ + exper + I(exper^2) + female + black + metro + south + midwest + west, data = cps5)
resid_sq <- resid(main_model)^2

aux_model1 <- lm(resid_sq ~ metro + female + black, data = cps5)
n1 <- nobs(aux_model1)
r2_1 <- summary(aux_model1)$r.squared
LM1 <- n1 * r2_1

qchisq(0.99, df = 3)

LM1

# NR^2 Test with all explanatory variables
aux_model2 <- lm(resid_sq ~ educ + exper + I(exper^2) + female + black + metro + south + midwest + west, data = cps5)
n2 <- nobs(aux_model2)
r2_2 <- summary(aux_model2)$r.squared
LM2 <- n2 * r2_2

qchisq(0.99, df = 9)

LM2

# (c)
white_test <- bptest(model_ols,
                     ~ educ + exper + I(exper^2) + female + black + metro + south + midwest + west +
                       I(educ^2) + I(exper^4) + 
                       educ:exper + educ:female + educ:black + educ:metro + educ:south + educ:midwest + educ:west + educ:I(exper^2) +
                       exper:female + exper:black + exper:metro + exper:south + exper:midwest + exper:west + exper:I(exper^2) +
                       female:black + female:metro + female:south + female:midwest + female:west + female:I(exper^2) + 
                       black:metro + black:south + black:midwest + black:west + black:I(exper^2) + 
                       metro:south + metro:midwest + metro:west + metro:I(exper^2) +
                       south:I(exper^2) + midwest:I(exper^2) + west:I(exper^2),
                     data = cps5)
print(white_test)

# (d)
coeftest(main_model, vcov = vcovHC(main_model, type = "HC0"))
ci_ols <- confint(main_model)

robust_se <- sqrt(diag(vcovHC(main_model, type = "HC0")))
coef_est <- coef(main_model)
ci_robust <- cbind(coef_est - 1.96 * robust_se,
                        coef_est + 1.96 * robust_se)
ci_robust

colnames(ci_robust) <- c("2.5 % (robust)", "97.5 % (robust)")

width_ols <- ci_ols[,2] - ci_ols[,1]
width_robust <- ci_robust[,2] - ci_robust[,1]
diff_width <- width_robust - width_ols
direction <- ifelse(diff_width > 0, "變寬", ifelse(diff_width < 0, "變窄", "無變化"))

comparison_df <- data.frame(
  係數 = rownames(ci_ols),
  `OLS CI 寬度` = round(width_ols, 4),
  `Robust CI 寬度` = round(width_robust, 4),
  `寬度差 (robust - OLS)` = round(diff_width, 4),
  `變化方向` = direction
)

print(comparison_df)

# (e)
log_resid_sq <- log(resid(model_ols)^2)   
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
  OLS_Robust_SE = robust_se,
  FGLS_SE = se_fgls,
  SE_Change = ifelse(robust_se > se_fgls, "smaller", 
                     ifelse(robust_se < se_fgls, "larger", "unchanged")),
  OLS_Robust_Width = ci_robust[,2] - ci_robust[,1],
  FGLS_Width = ci_fgls[,2] - ci_fgls[,1],
  CI_Change = ifelse((ci_fgls[,2] - ci_fgls[,1]) < (ci_robust[,2] - ci_robust[,1]), "narrower", 
                     ifelse((ci_fgls[,2] - ci_fgls[,1]) > (ci_robust[,2] - ci_robust[,1]), "wider", "unchanged"))
)
print(comparison_e)

# (f)
coeftest(model_fgls, vcov = sandwich::vcovHC(model_fgls))

robust_se <- sqrt(diag(vcovHC(main_model, type = "HC0")))
ci_ols_robust <- cbind(coef(main_model) - 1.96 * robust_se,
                       coef(main_model) + 1.96 * robust_se)
width_ols_robust <- ci_ols_robust[,2] - ci_ols_robust[,1]

fgls_model <- lm(log(wage) ~ educ + exper + I(exper^2) + female + black + metro + south + midwest + west,
                 weights = 1 / model_fgls$fitted.values^2,
                 data = cps5)
ci_fgls <- confint(fgls_model)
width_fgls <- ci_fgls[,2] - ci_fgls[,1]

robust_se_fgls <- sqrt(diag(vcovHC(fgls_model, type = "HC0")))
ci_fgls_robust <- cbind(coef(fgls_model) - 1.96 * robust_se_fgls,
                        coef(fgls_model) + 1.96 * robust_se_fgls)
width_fgls_robust <- ci_fgls_robust[,2] - ci_fgls_robust[,1]
ci_comparison <- data.frame(
  `OLS+Robust` = round(width_ols_robust, 4),
  `FGLS+Normal` = round(width_fgls, 4),
  `FGLS+Robust` = round(width_fgls_robust, 4),
  Compare_FGLS_Norm = ifelse(width_fgls < width_ols_robust, "narrower", "wider"),
  Compare_FGLS_Rob = ifelse(width_fgls_robust < width_ols_robust, "narrower", "wider")
)

print(ci_comparison)
