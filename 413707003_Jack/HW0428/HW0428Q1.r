# 10.18
# a

load(url("https://www.principlesofeconometrics.com/poe5/data/rdata/mroz.rdata"))


mroz_lf <- subset(mroz, lfp == 1)

mroz_lf$MOTHERCOLL <- as.numeric(mroz_lf$mothereduc > 12)
mroz_lf$FATHERCOLL <- as.numeric(mroz_lf$fathereduc > 12)

mean(mroz_lf$MOTHERCOLL) * 100  
mean(mroz_lf$FATHERCOLL) * 100

# Mother 12.14953%
# Father 11.68224%


# b

cor(mroz_lf[, c("educ", "MOTHERCOLL", "FATHERCOLL")], use = "complete.obs")

#                 educ MOTHERCOLL FATHERCOLL
# educ       1.0000000  0.3594705  0.3984962
# MOTHERCOLL 0.3594705  1.0000000  0.3545709
# FATHERCOLL 0.3984962  0.3545709  1.0000000

# The instrumental variable is only related to education and does not directly affect wages; using a dummy variable can help reduce multicollinearity issues.


# c
library(AER)

iv1 <- ivreg(log(wage) ~ educ + exper + I(exper^2) | MOTHERCOLL + exper + I(exper^2), data = mroz_lf)
summary(iv1)

#               Estimate Std. Error t value Pr(>|t|)   
# (Intercept) -0.1327561  0.4965325  -0.267  0.78932   
# educ         0.0760180  0.0394077   1.929  0.05440 . 
# exper        0.0433444  0.0134135   3.231  0.00133 **
# I(exper^2)  -0.0008711  0.0004017  -2.169  0.03066 * 

confint(iv1, level = 0.95)["educ",]

#        2.5 %       97.5 % 
# -0.001219763  0.153255678 


