# 10.20
# g

iv_model_g <- ivreg(rj_rf ~ rm_rf | RANK + POS, data = capm5)
summary(iv_model_g)

#             Estimate Std. Error t value Pr(>|t|)    
# (Intercept) 0.003004   0.006044   0.497     0.62    
# rm_rf       1.283118   0.127866  10.035   <2e-16 ***

# Yes. The estimate is close to its in (a).


# h

capm5$uhat <- resid(iv_model_g)

sargan_aux <- lm(uhat ~ RANK + POS, data = capm5)
n <- nrow(capm5)
R2 <- summary(sargan_aux)$r.squared
sargan_stat <- n * R2
pval_sargan <- 1 - pchisq(sargan_stat, df = 1)

cat("Sargan statistics:", sargan_stat, "\nP-value:", pval_sargan, "\n")

# Sargan statistics: 0.5584634 
# P-value: 0.45488 

# Because p-value > 0.05, so we fail to reject H0. IV is valid.






