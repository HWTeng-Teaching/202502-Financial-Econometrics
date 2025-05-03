# 10.24
# c

coeftest(iv_base, vcov = vcovHC(iv_base, type = "HC1"))

#                Estimate  Std. Error t value Pr(>|t|)   
# (Intercept)  0.04810030  0.42979772  0.1119 0.910945   
# educ         0.06139663  0.03333859  1.8416 0.066231 . 
# exper        0.04417039  0.01554638  2.8412 0.004711 **
# I(exper^2)  -0.00089897  0.00043008 -2.0902 0.037193 * 

# Larger than baseline model.

robust_se <- sqrt(diag(vcovHC(iv_base, type = "HC1")))
educ_est <- coef(iv_base)["educ"]
z_crit <- qnorm(0.975)
CI_robust <- educ_est + c(-1, 1) * z_crit * robust_se["educ"]
CI_robust
# [-0.003945805,  0.126739060]


# d

library(boot)

boot_fun <- function(data, indices) {
  d <- data[indices, ]
  model <- ivreg(log(wage) ~ educ + exper + I(exper^2) | mothereduc + fathereduc + exper + I(exper^2),
                 data = d)
  coef(model)["educ"]
}

boot_result <- boot(mroz_lf, statistic = boot_fun, R = 200)

boot_se <- sd(boot_result$t)
boot_se
# 0.03205281

# Larger than baseline model.
# Larger than robust standard errors in (c).

educ_coef <- coef(iv_base)["educ"]
CI_boot <- educ_coef + c(-1, 1) * z_crit * boot_se
CI_boot
# [-0.001425735,  0.124218990]
