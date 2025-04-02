#5.6
#a
qt(0.025, df = 60)

#b
b <- c(2, 3, -1)
covariance <- matrix(c(3,-2, 1, -2, 4, 0, 1, 0, 3),
                   nrow = 3,
                   byrow = TRUE)
B <- c(1, 2, 0)
Num <- 5
test_of_b <- (t(B) %*% b - Num) / sqrt(t(B) %*% covariance %*% B)
test_of_b

#c
C <- c(1, -1, 1)
NumC <- 4
test_of_c <- (t(C) %*% b - NumC) / sqrt(t(C) %*% covariance %*% C)
test_of_c

#5.31
#a
data("commute5")

mod1 <- lm(time ~ depart + reds + trains, data = commute5)
summary(mod1)

#b
confint(mod1, level = 0.95)

#c
test_reds <- qt(0.05, df = 245)
test_reds

#d
test_trains <- qt(0.95, df = 245)
test_trains

#e
test_depart <- qt(0.05, df = 245)
test_depart

#f
vcov_matrix <- vcov(mod1)
c_vec <- c(0, 0, -3, 1)  
(sef <- sqrt(t(c_vec) %*% vcov_matrix %*% c_vec))

t_f <- (coef(mod1)[4] - 3 * coef(mod1)[3]) / sef
t_f
test_f <- qt(0.05, df = 245)
test_f

#g
expected_time <- coef(mod1)["(Intercept)"] + coef(mod1)["depart"] * 30 + coef(mod1)["reds"] * 6 + coef(mod1)["trains"] * 1
se_time <- sqrt(
  vcov(mod1)["(Intercept)", "(Intercept)"] +
    30^2 * vcov(mod1)["depart", "depart"] +
    6^2 * vcov(mod1)["reds", "reds"] +
    1^2 * vcov(mod1)["trains", "trains"] +
    2 * (30 * vcov(mod1)["(Intercept)", "depart"] +
           6 * vcov(mod1)["(Intercept)", "reds"] +
           1 * vcov(mod1)["(Intercept)", "trains"]) +
    2 * (30 * 6 * vcov(mod1)["depart", "reds"] +
           30 * 1 * vcov(mod1)["depart", "trains"] +
           6 * 1 * vcov(mod1)["reds", "trains"])
)

t_g <- (expected_time - 45) / se_time
t_g

test_g <- qt(0.95, df = 245)
test_g


#5.33
remotes::install_github("ccolonescu/POE5Rdata", force = TRUE)
library(POE5Rdata)
data("cps5_small")
library(dplyr)
library(ggplot2)
install.packages("sandwich")
library(sandwich)
install.packages("lmtest")
library(lmtest)


#a
mod2 <- lm(log(wage) ~ educ + I(educ^2) + exper + I(exper^2) + educ:exper, data = cps5_small)
summary(mod2)

#b
marginal_effects_educ <- coef(mod2)[2] + 2 * coef(mod2)[3] * cps5_small$educ + coef(mod2)[6] * cps5_small$exper
marginal_effects_educ

#c
hist(marginal_effects_educ, 
     breaks = 20,  
     col = "lightblue",
     xlab = "Marginal Effect of EDUC")
quantile(marginal_effects_educ, c(0.05, 0.5, 0.95))


#e
marginal_effects_exper <- coef(mod2)[4] + 2 * coef(mod2)[5] * cps5_small$exper + coef(mod2)[6] * cps5_small$educ
hist(marginal_effects_exper,
     breaks = 30,  
     col = "lightblue",
     xlab = "Marginal Effect of EXPER")
quantile(marginal_effects_exper, c(0.05, 0.5, 0.95))

#f
vcov_matrix2 <- vcov(mod2)
c_vec <- c(0, 1, 33, -10,-260,-152)
coef_column_vector <- matrix(coef(mod2), ncol = 1)
(betaf <- c_vec2 %*% coef_column_vector)
(sef2 <- sqrt(t(c_vec2) %*% vcov_matrix2 %*% c_vec2))
t_value_f <- betaf/sef2
t_value_f
t <- qt(0.05, df = 1194)
t
#g
c_vec2 <- c(0, 1, 33, -10,-420,-144)
(betag <- (-c_vec2) %*% coef_column_vector)
(seg <- sqrt(t(c_vec2) %*% vcov_matrix2 %*% c_vec2))
t_value_g <- betag/seg
t_value_g

#h
c_vec3 <- c(0, 0, 0, 0, 12,-4)
(betah <- c_vec3 %*% coef_column_vector)
(seh <- sqrt(t(c_vec3) %*% vcov_matrix2 %*% c_vec3))
t_value_h <- betah/seh
t_value_h
t_h <- qt(0.025, df = 1194)
t_h

#i
beta_values <- coef(mod2)
cov_matrix <- vcov(mod2)

d_beta3 <- -1 / (2 * beta_values["I(exper^2)"])
d_beta4 <- (beta_values["exper"] + beta_values["educ:exper"] * 16) / (2 * beta_values["I(exper^2)"]^2)
d_beta5 <- -16 / (2 * beta_values["I(exper^2)"])

se_exper_zero <- sqrt(
  d_beta3^2 * cov_matrix["exper", "exper"] + 
    d_beta4^2 * cov_matrix["I(exper^2)", "I(exper^2)"] + 
    d_beta5^2 * cov_matrix["educ:exper", "educ:exper"] +
    2 * d_beta4 * d_beta3 * cov_matrix["I(exper^2)", "exper"] +
    2 * d_beta3 * d_beta5 * cov_matrix["exper", "educ:exper"] +
    2 * d_beta4 * d_beta5 * cov_matrix["I(exper^2)", "educ:exper"]
)

(exper_zero <- (-beta_values["exper"] - beta_values["educ:exper"] * 16) / (2 * beta_values["I(exper^2)"])-11)
(lower_bound <- exper_zero - qt(0.025,1194) * se_exper_zero)
(upper_bound <- exper_zero + qt(0.025,1194) * se_exper_zero)

