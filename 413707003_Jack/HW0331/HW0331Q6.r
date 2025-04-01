# 5.33
# d

cps5_small$ME_EXPER <- coef(model)["exper"] + 
  2 * coef(model)["I(exper^2)"] * cps5_small$exper + 
  coef(model)["I(educ * exper)"] * cps5_small$educ

# ME_exper = 0.04487888 - 0.000936045 exper - 0.001010236 educ

# The marginal effect of education increases due to the decrease in experience or the decrease in education.


# e

hist(cps5_small$ME_EXPER, main = "marginal effect of experience", xlab = "marginal effect", col = "lightgreen")
quantile(cps5_small$ME_EXPER, probs = c(0.05, 0.5, 0.95))
#           5%          50%          95% 
# -0.010376212  0.008418878  0.027931151 


# f







