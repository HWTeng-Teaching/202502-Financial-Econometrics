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

beta2 <- coef(model)[2]
beta3 <- coef(model)[3]
beta4 <- coef(model)[4]
beta5 <- coef(model)[5]
beta6 <- coef(model)[6]

c1 <- c(1, 17, 17^2, 8, 8^2, 17*8)
c2 <- c(1, 16, 16^2, 18, 18^2, 16*18)
diff_vec <- c2 - c1

vcov_matrix <- vcov(model)

std_error <- sqrt(t(diff_vec) %*% vcov_matrix %*% diff_vec)

t = (- beta2 - 33 * beta3 + 10 * beta4 + 260 * beta5 + 152 * beta6) / std_error
t
# 1.669902

qt(0.05, 1194)
# -1.646131

# Since t-value > qt(0.05, 1194), we fail to reject the null hypothesis, David's expected wage is not significantly greater than Svetlana's.

