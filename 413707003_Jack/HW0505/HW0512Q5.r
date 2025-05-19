# 11.30
# a

load(url("https://www.principlesofeconometrics.com/poe5/data/rdata/klein.rdata"))

ols_model <- lm(i ~ p + plag + klag, data=klein)
summary(ols_model)

#             Estimate Std. Error t value Pr(>|t|)    
# (Intercept) 10.12579    5.46555   1.853 0.081374 .  
# p            0.47964    0.09711   4.939 0.000125 ***
# plag         0.33304    0.10086   3.302 0.004212 ** 
# klag        -0.11179    0.02673  -4.183 0.000624 ***


# b

rf_model <- lm(p ~ g + w2 + tx + time + plag + klag + elag, data=klein)
summary(rf_model)

#             Estimate Std. Error t value Pr(>|t|)  
# (Intercept) 50.38442   31.63026   1.593   0.1352  
# g            0.43902    0.39114   1.122   0.2820  
# w2          -0.07961    2.53382  -0.031   0.9754  
# tx          -0.92310    0.43376  -2.128   0.0530 .
# time         0.31941    0.77813   0.410   0.6881  
# plag         0.80250    0.51886   1.547   0.1459  
# klag        -0.21610    0.11911  -1.814   0.0928 .
# elag         0.02200    0.28216   0.078   0.9390  

linearHypothesis(rf_model, c("g=0", "w2=0", "tx=0", "time=0", "(Intercept)=0", "elag=0"))

#   Res.Df    RSS Df Sum of Sq      F  Pr(>F)  
# 1     19 154.32                              
# 2     13  61.95  6    92.373 3.2307 0.03612 *

klein <- na.omit(klein)
klein$v_hat <- residuals(rf_model)
klein$P_hat <- fitted(rf_model)




