library(POE5Rdata)
data("cps5_small", package = "POE5Rdata")

names(cps5_small)

#A.

cps5_small$log_wage <- log(cps5_small$wage)

cps5_small$educ2 <- cps5_small$educ^2
cps5_small$exper2 <- cps5_small$exper^2
cps5_small$educ_exper <- cps5_small$educ * cps5_small$exper

model <- lm(log_wage ~ educ + educ2 + exper + exper2 + educ_exper, data = cps5_small)

summary(model)

#b.c.

beta <- coef(model)
b2 <- beta["educ"]
b3 <- beta["educ2"]
b6 <- beta["educ_exper"]

marginal_educ <- b2 + 2 * b3 * cps5_small$educ + b6 * cps5_small$exper

hist(marginal_educ, breaks = 30,  main = "Marginal Effect of EDUC on log(WAGE)", 
     xlab = "Marginal Effect", col = "skyblue", border = "white")

quantile(marginal_educ, probs = c(0.05, 0.5, 0.95))

#d.e.

b4 <- beta["exper"]
b5 <- beta["exper2"]

marginal_exper <- b4 + 2 * b5 * cps5_small$exper + b6 * cps5_small$educ

hist(marginal_exper,
     breaks = 25,
     main = "Marginal Effect of EXPER on log(WAGE)",
     xlab = "Marginal Effect",
     col = "lightgreen",
     border = "white")

quantile(marginal_exper, probs = c(0.05, 0.5, 0.95))

#f.

coefs <- coef(model)
vcov_mat <- vcov(model)

L <- c(0, -1, -33, 10, 260, 152)

diff_hat <- sum(L * coefs)
diff_hat

se_hat <- sqrt(t(L) %*% vcov_mat %*% L)
se_hat

t_stat <- diff_hat / se_hat
t_stat

#g.

coefs <- coef(model)
vcov_mat <- vcov(model)

L <- c(0, -1, -33, 10, 420, 144)

diff_hat <- sum(L * coefs)
diff_hat

se_hat <- sqrt(t(L) %*% vcov_mat %*% L)
se_hat

t_stat <- diff_hat / se_hat
t_stat

#h.

coefs <- coef(model)
vcov_mat <- vcov(model)

L <- c(0, 0, 0, 0, 12, -4)  

diff_hat <- sum(L * coefs)
diff_hat

se_hat <- sqrt(t(L) %*% vcov_mat %*% L)
se_hat

t_stat <- diff_hat / se_hat
t_stat

#i.

coefs <- coef(model)
b4 <- coefs["exper"]
b5 <- coefs["exper2"]
b6 <- coefs["educ_exper"]

educ_val <- 16

exper_star <- (((-b4 - b6 * educ_val) / (2 * b5))-11)
cat("Marginal effect turns negative at EXPER =", round(exper_star, 4), "years\n")


vcov_mat <- vcov(model)

dg_db4 <- -1 / (2 * b5)
dg_db5 <- (b4 + b6 * educ_val) / (2 * b5^2)
dg_db6 <- -educ_val / (2 * b5)

grad <- c(dg_db4, dg_db5, dg_db6)

vcov_sub <- vcov_mat[c("exper", "exper2", "educ_exper"),
                     c("exper", "exper2", "educ_exper")]

se_exper_star <- sqrt(t(grad) %*% vcov_sub %*% grad)
se_exper_star

t_crit <- qt(0.975, df.residual(model))
lower <- exper_star - t_crit * se_exper_star
upper <- exper_star + t_crit * se_exper_star

cat("95% CI for turning point: [", round(lower, 4), ",", round(upper, 4), "]\n")
