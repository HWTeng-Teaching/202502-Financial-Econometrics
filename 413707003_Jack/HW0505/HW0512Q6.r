# 11.30
# c

hausman_model <- lm(i ~ p + plag + klag + v_hat, data=klein)
summary(hausman_model)

#             Estimate Std. Error t value Pr(>|t|)    
# (Intercept) 20.27821    4.70179   4.313 0.000536 ***
# p            0.15022    0.10798   1.391 0.183222    
# plag         0.61594    0.10147   6.070 1.62e-05 ***
# klag        -0.15779    0.02252  -7.007 2.96e-06 ***
# v_hat        0.57451    0.14261   4.029 0.000972 ***

# Since v_hat is statistically significant, we reject the null hypothesis and conclude that p is an endogenous variable.








