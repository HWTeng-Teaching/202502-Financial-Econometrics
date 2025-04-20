#8.16
##(a)
library(POE5Rdata)
data("vacation")

model <- lm(miles ~ income + age + kids, data = vacation)
summary(model)
confint(model, "kids", level = 0.95)

##(b)
residuals <- resid(model)
par(mfrow = c(1, 2))
plot(vacation$income, residuals, xlab = "Income", ylab = "Residuals", main = "Residuals vs Income")
abline(h = 0, col = "red")
plot(vacation$age, residuals, xlab = "Age", ylab = "Residuals", main = "Residuals vs Age")
abline(h = 0, col = "red")

##(c)
sorted <- vacation[order(vacation$income), ]
n <- nrow(sorted) 
first <- vacation_sorted[1:90, ]
last <- vacation_sorted[(n-89):n, ]
model_first <- lm(miles ~ income + age + kids, data = first)
model_last <- lm(miles ~ income + age + kids, data = last)

summary(model_first)
summary(model_last)

SSE_first <- sum(resid(model_first)^2)
SSE_last <- sum(resid(model_last)^2)

k <- 4 
df <- 90 - k  
F_stat <- (SSE_last / df) / (SSE_first / df)

alpha <- 0.05
F_critical <- qf(1 - alpha, df, df)

library(lmtest)
gq_result <- gqtest(miles ~ income + age + kids, data = sorted,
                    order.by = ~ income, alternative = "greater",
                    fraction = 0.1)  # 排除中間 10%
print(gq_result)

##(d)
install.packages("sandwich", dependencies = TRUE)
library(sandwich)   
library(lmtest) 
robust_se <- vcovHC(model,type = "HC1") 
coeftest(model, vcov = robust_se)   
coefci(model, vcov. = robust_se, level = 0.95)  

##(e)
model_gls <- lm(miles ~ income + age + kids, data = vacation, weights = 1/(income^2))
summary(model_gls)
confint(model_gls, level = 0.95)

robust_se_gls <- vcovHC(model_gls, type = "HC1")
coeftest(model_gls, vcov = robust_se_gls)
coefci(model_gls, vcov. = robust_se_gls)

#8.18
##(a)
library(POE5Rdata)
data("cps5")

data_male <- subset(cps5, female == 0)
data_female <- subset(cps5, female == 1)

model_male <- lm(log(wage) ~ educ + exper + I(exper^2) + black + metro + south + midwest + west, data = data_male)
model_female <- lm(log(wage) ~ educ + exper + I(exper^2) + black + metro + south + midwest + west, data = data_female)

sse_male <- sum(resid(model_male)^2)
sse_female <- sum(resid(model_female)^2)
df_male <- model_male$df.residual
df_female <- model_female$df.residual

F_stat <- (sse_male / df_male) / (sse_female / df_female)
alpha <- 0.05
F_critical_upper <- qf(1 - alpha/2, df_male, df_female)
F_critical_lower <- qf(alpha/2, df_male, df_female)
cat(F_stat)
cat(F_critical_lower,F_critical_upper)

##(b)
model <- lm(log(wage) ~ educ + exper + I(exper^2) + female + black + metro + south + midwest + west, data = cps5)

# R2 test（Breusch-Pagan test, 指定 METRO, FEMALE, BLACK 為異質變異來源）
bptest(model, ~ metro + female + black, data = cps5, studentize = FALSE)

# R2 test（Breusch-Pagan test, 所有自變數皆納入異質變異來源）
bptest(model, ~ educ + exper + I(exper^2) + female + black + metro + south + midwest + west, data = cps5, studentize = FALSE)

##(C)
library(lmtest)
# White test: 包含所有自變數、平方項與交乘項
bptest(model, ~ educ + exper + I(exper^2) + female + black + metro + south + midwest + west +
         I(educ^2) + I(exper^4) + I(female^2) + I(black^2) + I(metro^2) + I(south^2) +
         I(midwest^2) + I(west^2) +
         educ:exper + educ:female + educ:black + educ:metro + educ:south + educ:midwest + educ:west +
         exper:female + exper:black + exper:metro + exper:south + exper:midwest + exper:west +
         female:black + female:metro + female:south + female:midwest + female:west +
         black:metro + black:south + black:midwest + black:west +
         metro:south + metro:midwest + metro:west +
         south:midwest + south:west + midwest:west,
       data = cps5)

##(d)

model_ols <- lm(log(wage) ~ educ + exper + I(exper^2) + female + black + metro + south + midwest + west, data = cps5)
confint_ols <- confint(model_ols)
robust_se <- vcovHC(model_ols, type = "HC1")

coef_est <- coef(model_ols)
se_robust <- sqrt(diag(robust_se))
alpha <- 0.05
crit_val <- qt(1 - alpha/2, df = df.residual(model_ols))
lower_robust <- coef_est - crit_val * se_robust
upper_robust <- coef_est + crit_val * se_robust
confint_robust <- cbind(lower_robust, upper_robust)

##(e)

model_ols <- lm(log(wage) ~ educ + exper + I(exper^2) + female + black + metro + south + midwest + west, data = cps5)
robust_se_ols <- sqrt(diag(vcovHC(model_ols, type = "HC1")))
ols_coef <- coef(model_ols)
alpha <- 0.05
crit_val <- qt(1 - alpha/2, df = df.residual(model_ols))
ols_ci_lower <- ols_coef - crit_val * robust_se_ols
ols_ci_upper <- ols_coef + crit_val * robust_se_ols

# FGLS (using METRO and EXPER as variance predictors)
log_resid_sq <- log(resid(model_ols)^2)
aux_model <- lm(log_resid_sq ~ metro + exper, data = cps5)
weights <- exp(predict(aux_model))
model_fgls <- lm(log(wage) ~ educ + exper + I(exper^2) + female + black + metro + south + midwest + west,
                 data = cps5, weights = 1/weights)
fgls_coef <- coef(model_fgls)
fgls_se <- summary(model_fgls)$coefficients[,2]
fgls_ci_lower <- fgls_coef - crit_val * fgls_se
fgls_ci_upper <- fgls_coef + crit_val * fgls_se

ols_width <- ols_ci_upper - ols_ci_lower
fgls_width <- fgls_ci_upper - fgls_ci_lower

# 比較寬度
ci_change <- ifelse(fgls_width > ols_width, "wider", "narrower")

# 組成表格
result_table <- data.frame(
  Variable = names(ols_coef),
  OLS_CI_Lower = ols_ci_lower,
  OLS_CI_Upper = ols_ci_upper,
  OLS_Width = ols_width,
  FGLS_CI_Lower = fgls_ci_lower,
  FGLS_CI_Upper = fgls_ci_upper,
  FGLS_Width = fgls_width,
  CI_Change = ci_change
)

print(result_table)

##(f)

# OLS 模型
model_ols <- lm(log(wage) ~ educ + exper + I(exper^2) + female + black + metro + south + midwest + west, data = cps5)

# FGLS 步驟
# (a) 以 METRO 與 EXPER 為異質變異來源，對 log(residual^2) 進行輔助迴歸
log_resid_sq <- log(resid(model_ols)^2)
aux_model <- lm(log_resid_sq ~ metro + exper, data = cps5)
weights <- exp(predict(aux_model))

# (b) FGLS 迴歸（加權最小平方法）
model_fgls <- lm(log(wage) ~ educ + exper + I(exper^2) + female + black + metro + south + midwest + west,
                 data = cps5, weights = 1/weights)

# 取得 robust 標準誤
robust_se_ols <- sqrt(diag(vcovHC(model_ols, type = "HC1")))
robust_se_fgls <- sqrt(diag(vcovHC(model_fgls, type = "HC1")))

# 信賴區間（以 95% 為例）
alpha <- 0.05
crit_val_ols <- qt(1 - alpha/2, df = df.residual(model_ols))
crit_val_fgls <- qt(1 - alpha/2, df = df.residual(model_fgls))

ols_coef <- coef(model_ols)
ols_ci_lower <- ols_coef - crit_val_ols * robust_se_ols
ols_ci_upper <- ols_coef + crit_val_ols * robust_se_ols

fgls_coef <- coef(model_fgls)
fgls_ci_lower <- fgls_coef - crit_val_fgls * robust_se_fgls
fgls_ci_upper <- fgls_coef + crit_val_fgls * robust_se_fgls

# 計算信賴區間寬度
ols_width <- ols_ci_upper - ols_ci_lower
fgls_width <- fgls_ci_upper - fgls_ci_lower

# 比較寬度
ci_change <- ifelse(fgls_width > ols_width, "wider", "narrower")

# 組成結果表
result_table <- data.frame(
  Variable = names(ols_coef),
  OLS_CI_Lower = ols_ci_lower,
  OLS_CI_Upper = ols_ci_upper,
  FGLS_CI_Lower = fgls_ci_lower,
  FGLS_CI_Upper = fgls_ci_upper,
  CI_Change = ci_change
)

print(result_table)

