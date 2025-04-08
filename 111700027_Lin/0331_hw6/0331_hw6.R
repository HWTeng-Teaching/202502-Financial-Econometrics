#5.31
#(a)
install.packages(POE5Rdata)
library(POE5Rdata)

data("commute5")
fit1 = lm(time ~ depart + reds + trains, data = commute5)
summary(fit1)

# (b)
confint(fit1)

# (c), (d), (e)
qt_vals = qt(c(0.05, 0.95), df = df.residual(fit1))
qt_vals

# (f)
vcm1 = vcov(fit1)
c_f = c(0, 0, -3, 1)
se_f = sqrt(t(c_f) %*% vcm1 %*% c_f)

t_f = (coef(fit1)["trains"] - 3 * coef(fit1)["reds"]) / se_f
t_f
qt(0.05, df = df.residual(fit1))

#(g)
x_new = c(1, 30, 6, 1)
mean_time = sum(coef(fit1) * x_new)

var_time = t(x_new) %*% vcm1 %*% x_new
se_pred = sqrt(var_time)

t_g = (mean_time - 45) / se_pred
t_g
qt(0.95, df = df.residual(fit1))


#5.33
install.packages(POE5Rdata)
library(POE5Rdata)
data("cps5_small")

install.packages(dplyr)
install.packages(ggplot2)
library(dplyr)
library(ggplot2)
install.packages("sandwich")
install.packages("lmtest")
library(sandwich)
library(lmtest)

#(a)
fit2 = lm(log(wage) ~ educ + I(educ^2) + exper + I(exper^2) + educ:exper, data = cps5_small)
summary(fit2)

#(b)
me_educ = coef(fit2)["educ"] + 
  2 * coef(fit2)["I(educ^2)"] * cps5_small$educ + 
  coef(fit2)["educ:exper"] * cps5_small$exper
me_educ

#(c)
hist(me_educ, breaks = 20, col = "skyblue", xlab = "Marginal Effect of Education")
quantile(me_educ, probs = c(0.05, 0.5, 0.95))

#(e)
me_exper = coef(fit2)["exper"] + 
  2 * coef(fit2)["I(exper^2)"] * cps5_small$exper + 
  coef(fit2)["educ:exper"] * cps5_small$educ

hist(me_exper, breaks = 30, col = "skyblue", xlab = "Marginal Effect of Experience")
quantile(me_exper, probs = c(0.05, 0.5, 0.95))

#(f)
vcov2 = vcov(fit2)
c_f2 = c(0, 1, 33, -10, -260, -152)
beta_hat = coef(fit2)
se_f2 = sqrt(t(c_f2) %*% vcov2 %*% c_f2)
t_stat_f = sum(c_f2 * beta_hat) / se_f2
t_stat_f
qt(0.05, df = df.residual(fit2))

#(g)
c_g = c(0, -1, -33, 10, 420, 144)
se_g = sqrt(t(c_g) %*% vcov2 %*% c_g)
t_stat_g = sum(c_g * beta_hat) / se_g
t_stat_g

#(h)
c_h = c(0, 0, 0, 0, 12, -4)
se_h = sqrt(t(c_h) %*% vcov2 %*% c_h)
t_stat_h = sum(c_h * beta_hat) / se_h
t_stat_h
qt(0.025, df = df.residual(fit2))

#(i)
b_vals = coef(fit2)
vc = vcov(fit2)

d1 = -1 / (2 * b_vals["I(exper^2)"])
d2 = (b_vals["exper"] + 16 * b_vals["educ:exper"]) / (2 * b_vals["I(exper^2)"]^2)
d3 = -16 / (2 * b_vals["I(exper^2)"])

var_zero = d1^2 * vc["exper", "exper"] +
  d2^2 * vc["I(exper^2)", "I(exper^2)"] +
  d3^2 * vc["educ:exper", "educ:exper"] +
  2 * d1 * d2 * vc["exper", "I(exper^2)"] +
  2 * d1 * d3 * vc["exper", "educ:exper"] +
  2 * d2 * d3 * vc["I(exper^2)", "educ:exper"]

se_zero = sqrt(var_zero)

turning_point = (-b_vals["exper"] - 16 * b_vals["educ:exper"]) / (2 * b_vals["I(exper^2)"]) - 11
ci_lower = turning_point - qt(0.025, df = df.residual(fit2)) * se_zero
ci_upper = turning_point + qt(0.025, df = df.residual(fit2)) * se_zero

turning_point
ci_lower
ci_upper
