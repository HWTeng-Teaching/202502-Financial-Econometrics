# 10.18
# f


first_stage2 <- lm(educ ~ MOTHERCOLL + FATHERCOLL + exper + I(exper^2), data = mroz_lf)
summary(first_stage2)

#              Estimate Std. Error t value Pr(>|t|)    
# (Intercept) 11.890259   0.290251  40.965  < 2e-16 ***
# MOTHERCOLL   1.749947   0.322347   5.429 9.58e-08 ***
# FATHERCOLL   2.186612   0.329917   6.628 1.04e-10 ***
# exper        0.049149   0.040133   1.225    0.221    
# I(exper^2)  -0.001449   0.001199  -1.209    0.227  

anova(first_stage2)

#             Df  Sum Sq Mean Sq F value    Pr(>F)    
# MOTHERCOLL   1  288.18 288.184 69.7243 9.826e-16 ***
# FATHERCOLL   1  187.39 187.394 45.3387 5.420e-11 ***
# exper        1    0.24   0.242  0.0586    0.8089    
# I(exper^2)   1    6.04   6.037  1.4607    0.2275 

# Yes. They seem adequately strong.


# g

library(sandwich)
library(lmtest)

summary(iv2, diagnostics = TRUE)

#                  df1 df2 statistic p-value    
# Weak instruments   2 423    56.963  <2e-16 ***
# Wu-Hausman         1 423     0.519   0.472    
# Sargan             1  NA     0.238   0.626    

# strong instrument
# educ is marginally endogenous
# the extra instruments are valid
