# 10.20
# d

iv_model_d <- ivreg(rj_rf ~ rm_rf | RANK, data = capm5)
summary(iv_model_d)

#             Estimate Std. Error t value Pr(>|t|)    
# (Intercept) 0.003018   0.006044   0.499    0.618    
# rm_rf       1.278318   0.128011   9.986   <2e-16 ***


# The estimate is little more than its in (a).


# e

capm5$POS <- as.numeric(capm5$rm_rf > 0)

first_stage_e <- lm(rm_rf ~ RANK + POS, data = capm5)
summary(first_stage_e)

#               Estimate Std. Error t value Pr(>|t|)    
# (Intercept) -0.0804216  0.0022622  -35.55   <2e-16 ***
# RANK         0.0009819  0.0000400   24.55   <2e-16 ***
# POS         -0.0092762  0.0042156   -2.20   0.0291 * 

anova(first_stage_e)

#            Df  Sum Sq Mean Sq   F value  Pr(>F)    
# RANK        1 0.39955 0.39955 1897.6825 < 2e-16 ***
# POS         1 0.00102 0.00102    4.8421 0.02907 *  

# RANK is strong IV.
# R-squared:  0.9139 


# f

capm5$vhat2 <- resid(first_stage_e)

hausman_test <- lm(rj_rf ~ rm_rf + vhat2, data = capm5)
summary(hausman_test)

#              Estimate Std. Error t value Pr(>|t|)    
# (Intercept)  0.003004   0.005972   0.503   0.6157    
# rm_rf        1.283118   0.126344  10.156   <2e-16 ***
# vhat2       -0.954918   0.433062  -2.205   0.0287 *  


# Because p-value >0.01, so we can regard market return as exogenous.


