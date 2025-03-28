# 5.23
# a
load(url("https://www.principlesofeconometrics.com/poe5/data/rdata/cocaine.rdata"))

# beta_2:Negative. The relationship between price and quantity is usually inverse.
# beta_3:Positive. The higher the purity, the more expensive the price tends to be.
# beta_4:Uncertain. It depends on supply and demand in different years.


# b
model <- lm(price ~ quant + qual + trend, data = cocaine)
summary(model)

# Call:
#   lm(formula = price ~ quant + qual + trend, data = cocaine)
# 
# Residuals:
#   Min      1Q  Median      3Q     Max 
# -43.479 -12.014  -3.743  13.969  43.753 
# 
# Coefficients:
#                Estimate Std. Error t value Pr(>|t|)    
#    (Intercept) 90.84669    8.58025  10.588 1.39e-14 ***
#    quant       -0.05997    0.01018  -5.892 2.85e-07 ***
#    qual         0.11621    0.20326   0.572   0.5700    
#    trend       -2.35458    1.38612  -1.699   0.0954 .  
# ---
# 
# 
# Residual standard error: 20.06 on 52 degrees of freedom
# Multiple R-squared:  0.5097,	Adjusted R-squared:  0.4814 
# F-statistic: 18.02 on 3 and 52 DF,  p-value: 3.806e-08


# beta_2:Quantity increase 1 gram, price decrease 0.05997. As expected.
# beta_3:Quality increase 1 unit, price increase 0.11621. As expected.
# beta_4:Year increase 1, price decrease 2.35458.







