# 10.24
# a

load(url("https://www.principlesofeconometrics.com/poe5/data/rdata/mroz.rdata"))

mroz_lf <- subset(mroz, lfp == 1)

iv_base <- ivreg(log(wage) ~ educ + exper + I(exper^2) | mothereduc + fathereduc + exper + I(exper^2),
                 data = mroz_lf)
summary(iv_base)

#               Estimate Std. Error t value Pr(>|t|)   
# (Intercept)  0.0481003  0.4003281   0.120  0.90442   
# educ         0.0613966  0.0314367   1.953  0.05147 . 
# exper        0.0441704  0.0134325   3.288  0.00109 **
# I(exper^2)  -0.0008990  0.0004017  -2.238  0.02574 * 

e_hat <- resid(iv_base)

plot(mroz_lf$exper, e_hat,
     xlab = "EXPER", ylab = "IV Residuals",
     main = "IV Residuals vs Exper")
abline(h = 0, col = "red", lty = 2)

# No. The residuals decrease as experience increases, suggesting the presence of heteroskedasticity.



# b

ehat_sq <- e_hat^2
lm_nr2 <- lm(ehat_sq ~ mroz_lf$exper)
summary(lm_nr2)

#                Estimate Std. Error t value Pr(>|t|)    
# (Intercept)    0.676563   0.096573   7.006 9.65e-12 ***
# mroz_lf$exper -0.017303   0.006303  -2.745  0.00631 ** 

n <- nrow(mroz_lf)
R2 <- summary(lm_nr2)$r.squared
NR2_stat <- n * R2
pval_NR2 <- 1 - pchisq(NR2_stat, df = 1)

cat("NR2 statistics:", round(NR2_stat, 3), "\nP-value:", round(pval_NR2, 4), "\n")

# NR² statistics: 7.439 
# P-value: 0.0064 

# Because p-value < 0.01, so we reject H0. The model exhibits heteroskedasticity.


