# 5.33
# a

load(url("https://www.principlesofeconometrics.com/poe5/data/rdata/cps5_small.rdata"))

model <- lm(log(wage) ~ educ + I(educ^2) + exper + I(exper^2) + I(educ * exper), data = cps5_small)

summary(model)
# Call:
#   lm(formula = log(wage) ~ educ + I(educ^2) + exper + I(exper^2) + 
#        I(educ * exper), data = cps5_small)
# 
# Residuals:
#   Min      1Q  Median      3Q     Max 
# -1.6628 -0.3138 -0.0276  0.3140  2.1394 
# 
# Coefficients:
#                     Estimate Std. Error t value Pr(>|t|)    
#   (Intercept)      1.038e+00  2.757e-01   3.764 0.000175 ***
#   educ             8.954e-02  3.108e-02   2.881 0.004038 ** 
#   I(educ^2)        1.458e-03  9.242e-04   1.578 0.114855    
#   exper            4.488e-02  7.297e-03   6.150 1.06e-09 ***
#   I(exper^2)      -4.680e-04  7.601e-05  -6.157 1.01e-09 ***
#   I(educ * exper) -1.010e-03  3.791e-04  -2.665 0.007803 ** 
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 0.4638 on 1194 degrees of freedom
# Multiple R-squared:  0.3227,	Adjusted R-squared:  0.3198 
# F-statistic: 113.8 on 5 and 1194 DF,  p-value: < 2.2e-16

#Except for educ^2, all other coefficients are significant at the 1% level.


# b

cps5_small$ME_EDUC <- coef(model)["educ"] + 
  2 * coef(model)["I(educ^2)"] * cps5_small$educ + 
  coef(model)["I(educ * exper)"] * cps5_small$exper

# ME_educ = 0.0895391 + 0.002916572 educ - 0.001010236 exper

# The marginal effect of education increases due to the increase in education or the decrease in experience.


# c

hist(cps5_small$ME_EDUC, main = "marginal effect of education", xlab = "marginal effect", col = "lightblue")
quantile(cps5_small$ME_EDUC, probs = c(0.05, 0.5, 0.95))
#         5%        50%        95% 
# 0.08008187 0.10843125 0.13361880 


