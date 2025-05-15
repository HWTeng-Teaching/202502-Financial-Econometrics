# 11.30
# d

iv_model <- ivreg(i ~ p + plag + klag | g + w2 + tx + time + plag + klag + elag, data=klein)
summary(iv_model)

#             Estimate Std. Error t value Pr(>|t|)   
# (Intercept) 20.27821    8.38325   2.419  0.02707 * 
# p            0.15022    0.19253   0.780  0.44598   
# plag         0.61594    0.18093   3.404  0.00338 **
# klag        -0.15779    0.04015  -3.930  0.00108 **

# The statistical significance of p has disappeared, while the other results remain consistent.


# e

stage2_model <- lm(i ~ P_hat + plag + klag, data=klein)
summary(stage2_model)

#             Estimate Std. Error t value Pr(>|t|)   
# (Intercept) 20.27821    9.97663   2.033  0.05802 . 
# P_hat        0.15022    0.22913   0.656  0.52084   
# plag         0.61594    0.21531   2.861  0.01083 * 
# klag        -0.15779    0.04778  -3.302  0.00421 **

# The coefficients are exactly the same, but the standard errors have increased.





