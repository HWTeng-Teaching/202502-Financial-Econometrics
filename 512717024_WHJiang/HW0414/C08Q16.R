
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
     pch = 20, col = "orange")
abline(h = 0, col = "brown")
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
