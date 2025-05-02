# 10.18
# d

first_stage <- lm(educ ~ MOTHERCOLL + exper + I(exper^2), data = mroz_lf)
summary(first_stage)
# educ = 2.517068 MOTHERCOLL + 0.056230 exper + -0.001956 (exper^2)

anova(first_stage)["MOTHERCOLL","F value"]
# 63.31414

# Yes


# e

iv2 <- ivreg(log(wage) ~ educ + exper + I(exper^2) | MOTHERCOLL + FATHERCOLL + exper + I(exper^2), data = mroz_lf)
summary(iv2)

#               Estimate Std. Error t value Pr(>|t|)   
# (Intercept) -0.2790819  0.3922213  -0.712  0.47714   
# educ         0.0878477  0.0307808   2.854  0.00453 **
# exper        0.0426761  0.0132950   3.210  0.00143 **
# I(exper^2)  -0.0008486  0.0003976  -2.135  0.03337 * 

confint(iv2, level = 0.95)["educ",]

#      2.5 %     97.5 % 
# 0.02751845 0.14817686 

#narrower than (c)

