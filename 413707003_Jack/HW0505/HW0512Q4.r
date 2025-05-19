# 11.28
# f

demand_ols <- lm(p ~ q + ps + di, data=truffles)
summary(demand_ols)

#             Estimate Std. Error t value Pr(>|t|)    
# (Intercept) -13.6195     9.0872  -1.499   0.1460    
# q             0.1512     0.4988   0.303   0.7642    
# ps            1.3607     0.5940   2.291   0.0303 *  
# di           12.3582     1.8254   6.770 3.48e-07 ***



supply_ols <- lm(p ~ q + pf, data=truffles)
summary(supply_ols)

#             Estimate Std. Error t value Pr(>|t|)    
# (Intercept) -52.8763     5.0238  -10.53 4.68e-11 ***
# q             2.6613     0.1712   15.54 5.42e-15 ***
# pf            2.9217     0.1482   19.71  < 2e-16 ***

# The overall results are similar for both models, except that the coefficient on $Q$ in the demand equation is not statistically significant in the OLS estimation.







