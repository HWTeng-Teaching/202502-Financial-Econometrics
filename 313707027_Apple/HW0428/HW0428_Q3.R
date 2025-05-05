# 10.24
# (a)
library(AER)
mroz2 <- subset(mroz, !is.na(wage) & wage > 0 & lfp == 1)
mroz2$lwage <- log(mroz2$wage)

# baseline 2SLS model
iv_base <- ivreg(lwage ~ educ + exper + I(exper^2) +educ |
                   mothereduc + fathereduc + exper + I(exper^2),
                 data = mroz2)

mroz2$resid_iv <- residuals(iv_base)


plot(mroz2$exper, mroz2$resid_iv,
     pch = 16, cex = 0.7, col = "blue",
     xlab = "EXPER", ylab = "2SLS Residuals",
     main = "Residuals vs Experience")
abline(h = 0, col = "red", lwd = 2, lty = 1)

# (b)
mroz2$resid_iv_sq <- mroz2$resid_iv^2

nr2_model <- lm(resid_iv_sq ~ exper, data = mroz2)

summary(nr2_model)

nr2_statistic <- summary(nr2_model)$r.squared * nrow(mroz2)
nr2_statistic

# (c)
library(sandwich)  
library(lmtest)    

robust_se <- coeftest(iv_base, vcov = vcovHC(iv_base, type = "HC1"))

print(robust_se)

beta_educ <- robust_se["educ", "Estimate"]
se_educ <- robust_se["educ", "Std. Error"]

lower <- beta_educ - 1.96 * se_educ
upper <- beta_educ + 1.96 * se_educ
c(lower, upper)

# (d)
library(boot)

boot_iv <- function(data, indices) {
  d <- data[indices, ]
  model <- ivreg(log(wage) ~ educ + exper + I(exper^2) | mothereduc + fathereduc + exper + I(exper^2), data = d)
  return(coef(model)["educ"])
}

set.seed(123)
boot_result <- boot(data = mroz2, statistic = boot_iv, R = 200)

boot_se <- sd(boot_result$t)
boot_ci <- quantile(boot_result$t, probs = c(0.025, 0.975))

boot_se
boot_ci
