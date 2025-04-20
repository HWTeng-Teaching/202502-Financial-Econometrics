install.packages(c("tidyverse", "lmtest", "sandwich", "car"))
library(tidyverse)
library(lmtest)
library(sandwich)
library(car)
library(POE5Rdata)
data(vacation)
?data
#8.16
#a
ols_model <- lm(miles ~ income + age + kids, data = vacation)
summary(ols_model)

confint(ols_model, "kids", level = 0.95)

#b
residuals <- resid(ols_model)  

plot(vacation$income, residuals,
     main = "Residuals vs INCOME",
     xlab = "INCOME", ylab = "Residuals")
plot(vacation$age, residuals,
     main = "Residuals vs AGE",
     xlab = "AGE", ylab = "Residuals")

#c
sorted <- vacation[order(vacation$income), ]
N <- nrow(sorted)

low_sample <- sorted[1:90, ]
high_sample <- sorted[(N - 89):N, ]

fit_low <- lm(miles ~ income + age + kids, data = low_sample)
fit_high <- lm(miles ~ income + age + kids, data = high_sample)

rss_low <- sum(resid(fit_low)^2)
rss_high <- sum(resid(fit_high)^2)

gq_stat <- rss_high / rss_low
df1 <- df.residual(fit_high)
df2 <- df.residual(fit_low)

alpha <- 0.05
f_crit <- qf(1 - alpha, df1, df2)

cat("GQ statistic:", round(gq_stat, 3), "\n")
cat("Critical value at 5% level:", round(f_crit, 3), "\n")

if (gq_stat > f_crit) {
  cat("Reject H₀: Evidence of heteroskedasticity.\n")
} else {
  cat("Fail to reject H₀: No evidence of heteroskedasticity.\n")
}

#d
library(sandwich)
library(lmtest)

ols_model <- lm(miles ~ income + age + kids, data = vacation)
robust_se <- vcovHC(ols_model, type = "HC1")

cat("Regression with robust standard errors:\n")
coeftest(ols_model, vcov = robust_se)

beta_kids <- coef(ols_model)["kids"]
se_kids <- sqrt(robust_se["kids", "kids"])

lower <- beta_kids - 1.96 * se_kids
upper <- beta_kids + 1.96 * se_kids

cat("\n95% robust confidence interval for 'kids':\n")
cat("(", round(lower, 4), ",", round(upper, 4), ")\n")

#e
library(sandwich)
library(lmtest)
gls_model <- lm(miles ~ income + age + kids,
                data = vacation,
                weights = 1 / (vacation$income^2))

ci_gls_conventional <- confint(gls_model, "kids", level = 0.95)

cat("GLS (conventional SE) 95% CI for kids:\n")
cat("(", round(ci_gls_conventional[1], 4), ",", round(ci_gls_conventional[2], 4), ")\n\n")

vcov_gls_robust <- vcovHC(gls_model, type = "HC1")
coef_kids_gls <- coef(gls_model)["kids"]
se_kids_gls_robust <- sqrt(vcov_gls_robust["kids", "kids"])

lower_gls_robust <- coef_kids_gls - 1.96 * se_kids_gls_robust
upper_gls_robust <- coef_kids_gls + 1.96 * se_kids_gls_robust

cat("GLS (robust SE) 95% CI for kids:\n")
cat("(", round(lower_gls_robust, 4), ",", round(upper_gls_robust, 4), ")\n")



