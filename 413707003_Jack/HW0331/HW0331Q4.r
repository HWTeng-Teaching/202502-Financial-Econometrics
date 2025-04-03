# 5.31
# g

expected_time <- coef(model)["(Intercept)"] + coef(model)["depart"] * 30 + coef(model)["reds"] * 6 + coef(model)["trains"] * 1


vcov_matrix <- vcov(model)

x_vector <- c(1, 30, 6, 1)


std_error <- sqrt(t(x_vector) %*% vcov_matrix %*% x_vector)
std_error


t = expected_time / std_error





# h














