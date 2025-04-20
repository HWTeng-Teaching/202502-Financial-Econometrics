library(POE5Rdata)
data(vacation)
data(cps5)

##8.16(a)
model <- lm(miles ~ income + age + kids, data = vacation)

summary(model)
confint(model, "kids", level = 0.95)

##(b)
residuals <- resid(model)

plot(vacation$income, residuals,
     main = "Residuals vs INCOME",
     xlab = "INCOME (in $1000s)", ylab = "Residuals",
     pch = 20, col = "blue")
abline(h = 0, col = "red")

plot(vacation$age, residuals,
     main = "Residuals vs AGE",
     xlab = "AGE", ylab = "Residuals",
     pch = 20, col = "darkgreen")
abline(h = 0, col = "red")
## It looks like the residuals are increasing when the income is larger.

##(c)
vacation_sorted <- vacation[order(vacation$income), ]

first90 <- vacation_sorted[1:90, ]
last90 <- vacation_sorted[(nrow(vacation_sorted)-89):nrow(vacation_sorted), ]

model_low <- lm(miles ~ income + age + kids, data = first90)
model_high <- lm(miles ~ income + age + kids, data = last90)

rss_low <- sum(resid(model_low)^2)
rss_high <- sum(resid(model_high)^2)

gq_stat <- rss_high / rss_low

df1 <- model_high$df.residual
df2 <- model_low$df.residual

alpha <- 0.05
f_crit <- qf(1 - alpha, df1, df2)

cat("GQ statistic:", round(gq_stat, 3), "\n")
cat("Critical value at 5% level:", round(f_crit, 3), "\n")

if (gq_stat > f_crit) {
  cat("Reject H₀: Evidence of heteroskedasticity.\n")
} else {
  cat("Fail to reject H₀: No evidence of heteroskedasticity.\n")
}

##(d)
library(sandwich)
library(lmtest)

ols_model <- lm(miles ~ income + age + kids, data = vacation)

robust_se <- vcovHC(ols_model, type = "HC1")

coeftest(ols_model, vcov. = robust_se)

confint_robust <- coeftest(ols_model, vcov. = robust_se)
beta_kids <- confint_robust["kids", 1]
se_kids <- confint_robust["kids", 2]
lower <- beta_kids - 1.96 * se_kids
upper <- beta_kids + 1.96 * se_kids

cat("Robust 95% CI for KIDS:", round(lower, 3), "to", round(upper, 3), "\n")
## This is wider than the interval in (a)

##(e)
vacation$weights_gls <- 1 / (vacation$income^2)

gls_model <- lm(miles ~ income + age + kids, data = vacation, weights = weights_gls)

summary(gls_model)
confint(gls_model, level = 0.95)["kids", ]
robust_gls_se <- vcovHC(gls_model, type = "HC1")
coeftest(gls_model, vcov. = robust_gls_se)

beta_kids_gls <- coef(gls_model)["kids"]
se_kids_gls <- sqrt(robust_gls_se["kids", "kids"])

lower_gls <- beta_kids_gls - 1.96 * se_kids_gls
upper_gls <- beta_kids_gls + 1.96 * se_kids_gls

cat("Robust GLS 95% CI for KIDS:", round(lower_gls, 3), "to", round(upper_gls, 3), "\n")
## From the narrowest to the widest:a,d,gls,robust gls

##8.18(a)
library(tidyverse)
library(broom)
data <- cps5  

data$lnWAGE <- log(data$wage)

formula <- lnWAGE ~ educ + exper + I(exper^2) + metro

male_data <- subset(data,female == 0)
fit_male <- lm(formula, data = male_data)
rss_male <- sum(resid(fit_male)^2)
df_male <- df.residual(fit_male)

female_data <- subset(data, female == 1)
fit_female <- lm(formula, data = female_data)
rss_female <- sum(resid(fit_female)^2)
df_female <- df.residual(fit_female)
F_stat <- max(rss_male / df_male, rss_female / df_female) / 
  min(rss_male / df_male, rss_female / df_female)

F_stat

df1 <- max(df_male, df_female)
df2 <- min(df_male, df_female)

F_critical <- qf(0.975, df1, df2)  # upper tail

if (F_stat > F_critical) {
  cat("Reject H0: There is evidence of heteroskedasticity (σ²_M ≠ σ²_F).\n")
} else {
  cat("Fail to reject H0: No evidence of different error variances between males and females.\n")
}

##(b)
model <- lm(log(wage) ~ educ + exper + I(exper^2) + female + black + metro + south + midwest + west, data = cps5)

resid_sq <- resid(model)^2

aux_data1 <- cps5[, c("metro", "female", "black")]
aux_model1 <- lm(resid_sq ~ ., data = aux_data1)
R2_1 <- summary(aux_model1)$r.squared
N1 <- nrow(cps5)
q1 <- ncol(aux_data1)
NR2_1 <- N1 * R2_1
pval1 <- 1 - pchisq(NR2_1, df = q1)

cat("Test with METRO, FEMALE, BLACK:\n")
cat("NR² =", NR2_1, ", df =", q1, ", p-value =", pval1, "\n")
if (pval1 < 0.01) {
  cat("Reject H0 at 1% level → Evidence of heteroskedasticity.\n\n")
} else {
  cat("Fail to reject H0 → No strong evidence of heteroskedasticity.\n\n")
}

X_full <- model.matrix(model)[, -1]  
aux_model2 <- lm(resid_sq ~ X_full)
R2_2 <- summary(aux_model2)$r.squared
q2 <- ncol(X_full)
NR2_2 <- N1 * R2_2
pval2 <- 1 - pchisq(NR2_2, df = q2)

cat("Test with ALL regressors:\n")
cat("NR² =", NR2_2, ", df =", q2, ", p-value =", pval2, "\n")
if (pval2 < 0.01) {
  cat("Reject H0 at 1% level → Strong evidence of heteroskedasticity.\n")
} else {
  cat("Fail to reject H0 → No strong evidence of heteroskedasticity.\n")
}

##(c)
resid_sq <- resid(model)^2

X <- model.matrix(model)[, -1]

white_vars <- as.data.frame(X)
var_names <- colnames(white_vars)

for (v in var_names) {
  white_vars[[paste0(v, "_sq")]] <- white_vars[[v]]^2
}

for (i in 1:(ncol(X) - 1)) {
  for (j in (i + 1):ncol(X)) {
    new_name <- paste0(var_names[i], "_x_", var_names[j])
    white_vars[[new_name]] <- X[, i] * X[, j]
  }
}

aux_model <- lm(resid_sq ~ ., data = white_vars)
R2 <- summary(aux_model)$r.squared
N <- nrow(cps5)
q <- length(coef(aux_model)) - 1  

NR2 <- N * R2
crit_val <- qchisq(0.95, df = q)
pval <- 1 - pchisq(NR2, df = q)

cat("White test results:\n")
cat("NR² =", round(NR2, 3), "\n")
cat("Degrees of freedom (q) =", q, "\n")
cat("5% critical value =", round(crit_val, 3), "\n")
cat("p-value =", round(pval, 4), "\n")

if (NR2 > crit_val) {
  cat("→ Reject H0: Evidence of heteroskedasticity.\n")
} else {
  cat("→ Fail to reject H0: No strong evidence of heteroskedasticity.\n")
}

##(d)
conventional <- coeftest(model)

robust <- coeftest(model, vcov = vcovHC(model, type = "HC0"))

compare <- data.frame(
  Estimate = coef(model),
  SE_Conventional = conventional[, "Std. Error"],
  SE_Robust = robust[, "Std. Error"]
)

compare$Interval_Width_Conventional <- 1.96 * compare$SE_Conventional
compare$Interval_Width_Robust <- 1.96 * compare$SE_Robust
compare$Change <- compare$Interval_Width_Robust - compare$Interval_Width_Conventional

print(compare)

##(e)
model_ols <- lm(log(wage) ~ educ+ exper + I(exper^2) + female + black + metro + south + midwest + west, data = cps5)

resid_sq <- residuals(model_ols)^2

aux_model <- lm(log(resid_sq) ~ metro + exper, data = cps5)
log_sigma2_hat <- fitted(aux_model)

weights_fgls <- 1 / exp(log_sigma2_hat)

model_fgls <- lm(log(wage) ~ educ+ exper + I(exper^2) + female + black + metro + south + midwest + west,
                 data = cps5, weights = weights_fgls)


ci_fgls <- confint(model_fgls)

robust_se <- coeftest(model_ols, vcov = vcovHC(model_ols, type = "HC0"))


ci_robust <- cbind(
  Estimate = coef(model_ols),
  Lower = coef(model_ols) - 1.96 * robust_se[, "Std. Error"],
  Upper = coef(model_ols) + 1.96 * robust_se[, "Std. Error"]
)


comparison <- data.frame(
  Coef = names(coef(model_ols)),
  CI_FGLS_Lower = ci_fgls[, 1],
  CI_FGLS_Upper = ci_fgls[, 2],
  CI_Robust_Lower = ci_robust[, "Lower"],
  CI_Robust_Upper = ci_robust[, "Upper"]
)

print(comparison)

##(f)
ci_robust_ols <- confint(model_ols, vcov = vcovHC(model_ols, type = "HC0"))

ci_fgls_conventional <- confint(model_fgls)

ci_robust_fgls <- confint(model_fgls, vcov = vcovHC(model_fgls, type = "HC0"))

ci_comparison <- data.frame(
  Coef = rownames(ci_fgls_conventional),
  OLS_Lower = ci_robust_ols[, 1],
  OLS_Upper = ci_robust_ols[, 2],
  FGLS_Lower = ci_fgls_conventional[, 1],
  FGLS_Upper = ci_fgls_conventional[, 2],
  RobustFGLS_Lower = ci_robust_fgls[, 1],
  RobustFGLS_Upper = ci_robust_fgls[, 2]
)

print(ci_comparison)

