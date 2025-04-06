library(POE5Rdata)

data("commute5")

# (a)
model <- lm(time ~ depart + reds + trains, data = commute5)
summary(model)

# (b)
confint(model, level = 0.95)

# (c)
b3 <- coef(model)["reds"]
se_b3 <- summary(model)$coefficients["reds", "Std. Error"]

t_value <- (b3 - 2) / se_b3
p_value <- pt(t_value, df = model$df.residual)

if (abs(t_value) > abs(qt(0.05, df = 245))) {
  print("reject H0")
}
t_value
p_value

# (d)
b4 <- coef(model)["trains"]
se_b4 <- summary(model)$coefficients["trains", "Std. Error"]

t_value <- (b4 - 3) / se_b4
p_value <- 2 * pt(-abs(t_value), df = model$df.residual)
if (abs(t_value) > abs(qt(0.95, df = 245))) {
  print("reject H0")
}
t_value
p_value

# (e)
b2 <- coef(model)["depart"]
se_b2 <- summary(model)$coefficients["depart", "Std. Error"]

t_value <- (30 * b2 - 10) / (30 * se_b2)
p_value <- pt(t_value, df = model$df.residual)
if (abs(t_value) > abs(qt(0.95, df = 245))) {
  print("reject H0")
}
t_value
p_value

# (f)
A1 <- c(0, 0, -3, 1)
L1 <- as.numeric(t(A1) %*% coef(model))
se_L1 <- as.numeric(sqrt(t(A1) %*% vcov(model) %*% A1))
tva_L1 <- L1/se_L1
cat("t = ", tva_L1, "\n")
vcov(model)

# (g)
x <- c(1, 30, 6, 1) 
beta_hat <- coef(model)
pred_time <- sum(beta_hat * x)

vcov_mat <- vcov(model)
se_pred <- sqrt(t(x) %*% vcov_mat %*% x)

t_value <- (pred_time - 45) / se_pred
p_value <- 1 - pt(t_value, df = model$df.residual)
if (t_value > qt(0.95, df = 245)) {
  print("reject H0")
}
t_value
p_value
