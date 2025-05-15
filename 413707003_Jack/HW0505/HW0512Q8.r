# 11.30
# f

klein$e2_hat <- residuals(stage2_model)

sargan_test <- lm(e2_hat ~ g + w2 + tx + time + plag + klag + elag, data=klein)
summary(sargan_test)

T <- nrow(klein)
R2 <- summary(sargan_test)$r.squared
Sargan_stat <- T * R2
Sargan_stat
# 1.281519

qchisq(0.95, df=4)
# 9.487729

# Since the Sargan test statistic is less than the critical value, the instruments are considered valid.








