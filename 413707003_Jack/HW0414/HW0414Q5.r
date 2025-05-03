# 8.16
# d

vcov_hc <- vcovHC(model, type = "HC1")
coefci(model, vcov. = vcov_hc, level = 0.95)["kids",]
#        2.5 %     97.5 % 
#   -139.32297  -24.32986 

#  The interval estimate is wider than interval estimate in (a).


# e

vacation$weights_gls <- 1 / (vacation$income^2)

gls_model <- lm(miles ~ income + age + kids, data = vacation, weights = weights_gls)
confint(gls_model, level = 0.95)["kids", ]
#        2.5 %     97.5 % 
#   -119.89450  -33.71808 

vcov_hc <- vcovHC(gls_model, type = "HC1")
coefci(model, vcov. = vcov_hc, level = 0.95)["kids",]
#        2.5 %     97.5 % 
#   -126.43351  -37.21932 

# Both interval estimates are narrower than interval estimate in (a) and (d). 
# (d)OLS_robust > (a)OLS > GLS_robust > GLS