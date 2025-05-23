# 15.17
# b

load(url("https://www.principlesofeconometrics.com/poe5/data/rdata/liquor5.rdata"))

library(plm)

re_model <- plm(liquor ~ income, data = liquor5, index = c("hh", "year"), model = "random")
summary(re_model)
#              Estimate Std. Error z-value  Pr(>|z|)    
# (Intercept) 0.9690324  0.5210052  1.8599 0.0628957 .  
# income      0.0265755  0.0070126  3.7897 0.0001508 ***

confint(re_model)["income",]
#      2.5 %     97.5 % 
# 0.01283111 0.04031983 


# c

plmtest(re_model, type = "bp")

# Because p < 0.05, we reject H_0, indicating that using the random effects model is appropriate.


# d

liquor5$INCOMEM <- ave(liquor5$income, liquor5$hh, FUN = mean)

re_mundlak <- plm(liquor ~ income + INCOMEM, data = liquor5, index = c("hh", "year"), model = "random")
summary(re_mundlak)

#              Estimate Std. Error z-value Pr(>|z|)  
# (Intercept) 0.9163337  0.5524439  1.6587  0.09718 .
# income      0.0207421  0.0209083  0.9921  0.32117  
# INCOMEM     0.0065792  0.0222048  0.2963  0.76700  


# Because the coefficient on INCOMEM is not significant, the random effects model remains valid.
