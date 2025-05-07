#10.24
# 313707014 陳紀蓁
#rm(list = ls())



library(POE5Rdata)
library(tidyverse)
library(sandwich)
library(lmtest)
library(sandwich)
library(car)
library(AER)
library(stargazer)


data ("mroz")
summary(mroz)
head(mroz)
mroz_clean <- subset(mroz, wage > 0)

#a.
mroz.iv <- ivreg(log(wage)~ educ+exper+I(exper^2) |
                         exper+I(exper^2)+mothereduc+fathereduc, data=mroz_clean)
summary(mroz.iv)

iv_residual <-  residuals(mroz.iv)
plot(mroz_clean$exper, iv_residual, main = "Residuals vs EXPER", xlab = "EXPER", ylab = "2SLS Residuals")
abline(h = 0, col = "red")


#b.
mroz_clean$iv_resquare <- iv_residual^2

model <- lm(iv_resquare ~ exper , data = mroz_clean)
summary(model)

n <- nrow(mroz_clean)
R2 <- summary(model)$r.squared
NR2 <- n * R2
p_value <- 1 - pchisq(NR2, df = 1)  # df = number of explanatory variables in aux regression (excluding constant)

cat("NR^2 statistic:", NR2, "\nP-value:", p_value, "\n")


#c.
# Robust 標準誤
robust_se <- sqrt(diag(vcovHC(mroz.iv, type = "HC1")))
summary_iv <- summary(mroz.iv)

# EDUCI 的估計值與 robust 標準誤
coef_educ <- coef(mroz.iv)["educ"]
se_educ_robust <- robust_se["educ"]

# 計算 95% CI
ci_lower <- coef_educ - 1.96 * se_educ_robust
ci_upper <- coef_educ + 1.96 * se_educ_robust
cat("95% CI for EDUCI (robust SE): [", ci_lower, ",", ci_upper, "]\n")

confint(mroz.iv)


#d.
library(boot)
set.seed(123)

boot_iv <- function(mroz_clean, indices) {
  d <- mroz_clean[indices, ]
  fit <- ivreg(log(wage) ~ educ + exper + I(exper^2) | exper + I(exper^2) + mothereduc + fathereduc, data = d)
  return(coef(fit)["educ"])
}

# 執行 bootstrap
boot_res <- boot(data = mroz_clean, statistic = boot_iv, R = 200)

# 取出 bootstrap 標準誤
boot_se <- sd(boot_res$t)

# 原估計值
educ_hat <- coef(mroz.iv)["educ"]

# Bootstrap 95% CI
ci_boot <- quantile(boot_res$t, probs = c(0.025, 0.975))

cat("Bootstrap SE:", boot_se, "\n")
cat("95% CI for EDUCI (bootstrap): [", ci_boot[1], ",", ci_boot[2], "]\n")











