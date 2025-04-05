# 5.31
# g

expected_time <- coef(model)["(Intercept)"] + coef(model)["depart"] * 30 + coef(model)["reds"] * 6 + coef(model)["trains"] * 1


vcov_matrix <- vcov(model)

x_vector <- c(1, 30, 6, 1)


std_error <- sqrt(t(x_vector) %*% vcov_matrix %*% x_vector)
std_error


t =  (expected_time - 45) / std_error
t

# Since t-value < qt(0.05, 245), we fail to reject the null hypothesis, departing at 7:00 AM may not be early enough to arrive on time.


# h

# null hypothesis changes to beta_1 + 30 * beta_2 + 6 * beta_3 + beta_4 >= 45

# Since t-value < qt(0.05, 245), we  reject the null hypothesis, departing at 7:00 AM allows for an on-time arrival.










