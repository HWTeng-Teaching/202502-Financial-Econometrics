# 5.33
# g

c1 <- c(1, 17, 17^2, 16, 16^2, 17*16)
c2 <- c(1, 16, 16^2, 26, 26^2, 16*26)
diff_vec <- c2 - c1

std_error <- sqrt(t(diff_vec) %*% vcov_matrix %*% diff_vec)

t = (- beta2 - 33 * beta3 + 10 * beta4 + 420 * beta5 + 144 * beta6) / std_error
t
# -2.062365

# Since t-value < qt(0.05, 1194), we reject the null hypothesis, David's expected wage is significantly greater than Svetlana's.


# h

x_vector <- c(0, 0, 0, 0, 12, -4)

std_error <- sqrt(t(x_vector) %*% vcov_matrix %*% x_vector)

t = (12 * beta5 - 4 * beta6) / std_error
t
# -1.027304

qt(0.975, 1194)
# 1.961953

qt(0.025, 1194)
# -1.961953

# Since qt(0.025, 1194)< t-value < qt(0.975, 1194), we fail to reject the null hypothesis, there is no statistically significant evidence that the marginal effect of experience on log-wage differs between Wendy and Jill.


# i

x_crit = - (beta4 + beta6 * 16) / (2 * beta5)

 grad <- c(
  d_b4 = -1 / (2 * beta5),
  d_b5 = (beta4 + beta6 * 16) / (2 * beta5^2),
  d_b6 = -16 / (2 * beta5)
)

var_mat <- vcov_matrix[c("exper", "I(exper^2)", "I(educ * exper)"), 
             c("exper", "I(exper^2)", "I(educ * exper)")]

std_error <- sqrt(t(grad) %*% var_mat %*% grad)

lower <- x_crit - 1.96 * std_error - 11
upper <- x_crit + 1.96 * std_error - 11

year = x_crit - 11
# 19.67706

# The marginal effect of experience for Jill is expected to become negative after 19.67706 more years of experience.

# ci = [15.96146, 23.39265]









