# 10.20
# a

load(url("https://www.principlesofeconometrics.com/poe5/data/rdata/capm5.rdata"))

capm5$rj_rf <- capm5$msft - capm5$riskfree
capm5$rm_rf <- capm5$mkt - capm5$riskfree

ols_model <- lm(rj_rf ~ rm_rf, data = capm5)
summary(ols_model)

#             Estimate Std. Error t value Pr(>|t|)    
# (Intercept) 0.003250   0.006036   0.538    0.591    
# rm_rf       1.201840   0.122152   9.839   <2e-16 ***

# Risky. Because estimate > 1.


# b

capm5$RANK <- rank(capm5$rm_rf)

first_stage_b <- lm(rm_rf ~ RANK, data = capm5)
summary(first_stage_b)

#               Estimate Std. Error t value Pr(>|t|)    
# (Intercept) -7.903e-02  2.195e-03   -36.0   <2e-16 ***
# RANK         9.067e-04  2.104e-05    43.1   <2e-16 ***

# Yes. It is very significant.
# R-squared:  0.9121 
# Yes.


# c

capm5$vhat <- resid(first_stage_b)

aug_model <- lm(rj_rf ~ rm_rf + vhat, data = capm5)
summary(aug_model)

#              Estimate Std. Error t value Pr(>|t|)    
# (Intercept)  0.003018   0.005984   0.504   0.6146    
# rm_rf        1.278318   0.126749  10.085   <2e-16 ***
# vhat        -0.874599   0.428626  -2.040   0.0428 *  

# Yes. Because p-value >0.01, so we can regard market return as exogenous.






