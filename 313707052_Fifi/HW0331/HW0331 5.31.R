library(POE5Rdata)
data("commute5", package = "POE5Rdata")

names(commute5)

#a.

model <- lm(time ~ depart + reds + trains, data = commute5)
summary(model)

#b.

confint(model, level = 0.95)

#c.

b3_hat <- coef(model)["reds"]
se_b3 <- summary(model)$coefficients["reds", "Std. Error"]

t_stat <- (b3_hat - 2) / se_b3
t_stat

#d.

b4_hat <- coef(model)["trains"]
se_b4 <- summary(model)$coefficients["trains", "Std. Error"]

t_stat <- (b4_hat - 3) / se_b4
t_stat

#e.

b2_hat <- coef(model)["depart"]
se_b2 <- summary(model)$coefficients["depart", "Std. Error"]

t_stat <- (b2_hat - 1/3) / se_b2
t_stat

#f.

b3_hat <- coef(model)["reds"]
b4_hat <- coef(model)["trains"]

vcov_matrix <- vcov(model)

print(vcov_matrix)

L_hat <- b4_hat - 3 * b3_hat
SE_L <- sqrt(
  vcov_matrix["trains", "trains"] +
    9 * vcov_matrix["reds", "reds"] -
    6 * vcov_matrix["trains", "reds"]
)

print(SE_L)

t_stat <- (L_hat - 0) / SE_L
t_stat

#g.

coefs <- coef(model)
vcov_mat <- vcov(model)

L <- c(1, 30, 6, 1)

time_hat <- sum(L * coefs)

se_hat <- sqrt(t(L) %*% vcov_mat %*% L)

t_stat <- (time_hat - 45) / se_hat
t_stat


