#5.31
#a
data("commute5")
tab1 <- lm(time ~ depart + reds + trains, data = commute5)
summary(tab1)

#b
confint(tab1, level = 0.95)

#c
qt(0.05, 245)


#f
vcov_matrix <- vcov(tab1)
c_vec <- c(0, 0, -3, 1)  # 假設 model 只有 4 個變數，若有更多則補 0
(sef <- sqrt(t(c_vec) %*% vcov_matrix %*% c_vec))

#g
new_data <- data.frame(depart = 30, reds = 6, trains = 1)
(predicted_TIME <- predict(tab1, newdata = new_data, se.fit = TRUE))

#5.33
#a
data("cps5_small")
tab2 <- lm(log(wage) ~ educ + I(educ^2) + exper + I(exper^2) + I(educ * exper), data = cps5_small)
summary(tab2)

#b
install.packages("dplyr")  
library(dplyr)
coef <- coef(tab2)
cps5_small <- cps5_small %>%
  mutate(marginal_effect_educ = coef["educ"] + 2 * coef["I(educ^2)"] * educ + coef["I(educ * exper)"] * exper)
head(cps5_small$marginal_effect_educ)

#c
quantile(cps5_small$marginal_effect_educ, probs = c(0.05, 0.5, 0.95))
hist(cps5_small$marginal_effect_educ, main="Histogram of Marginal Effect of EDUC", 
     xlab="Marginal Effect of EDUC", col="lightblue", border="black",breaks = 40)

#d
cps5_small <- cps5_small %>%
  mutate(marginal_effect_exper = coef["exper"] + 2 * coef["I(exper^2)"] * exper + coef["I(educ * exper)"] * educ)
head(cps5_small$marginal_effect_exper)

#e
quantile(cps5_small$marginal_effect_exper, probs = c(0.05, 0.5, 0.95))
hist(cps5_small$marginal_effect_exper, main="Histogram of Marginal Effect of EXPER", 
     xlab="Marginal Effect of EXPER", col="lightblue", border="black",breaks = 40)

#f
vcov_matrix2 <- vcov(tab2)
c_vec2 <- c(0, 1, 33, -10,-260,-152)
coef_column_vector <- matrix(coef(tab2), ncol = 1)
(betaf <- c_vec2 %*% coef_column_vector)
(sef2 <- sqrt(t(c_vec2) %*% vcov_matrix2 %*% c_vec2))

#g
vcov_matrix2 <- vcov(tab2)
c_vec3 <- c(0, 1, 33, -10,-420,-144)
(betag <- c_vec3 %*% coef_column_vector)
(seg <- sqrt(t(c_vec3) %*% vcov_matrix2 %*% c_vec3))

#h
vcov_matrix2 <- vcov(tab2)
c_vec4 <- c(0, 0, 0, 0, 12,-4)
(betah <- c_vec4 %*% coef_column_vector)
(seh <- sqrt(t(c_vec4) %*% vcov_matrix2 %*% c_vec4))

#i
beta_values <- coef(tab2)
cov_matrix <- vcov(tab2)

d_beta3 <- -1 / (2 * beta_values["I(exper^2)"])
d_beta4 <- (beta_values["exper"] + beta_values["I(educ * exper)"] * 16) / (2 * beta_values["I(exper^2)"]^2)
d_beta5 <- -16 / (2 * beta_values["I(exper^2)"])

se_exper_zero <- sqrt(
  d_beta3^2 * cov_matrix["exper", "exper"] + 
    d_beta4^2 * cov_matrix["I(exper^2)", "I(exper^2)"] + 
    d_beta5^2 * cov_matrix["I(educ * exper)", "I(educ * exper)"] +
    2 * d_beta4 * d_beta3 * cov_matrix["I(exper^2)", "exper"] +
    2 * d_beta3 * d_beta5 * cov_matrix["exper", "I(educ * exper)"] +
    2 * d_beta4 * d_beta5 * cov_matrix["I(exper^2)", "I(educ * exper)"]
)

(exper_zero <- (-beta_values["exper"] - beta_values["I(educ * exper)"] * 16) / (2 * beta_values["I(exper^2)"])-11)
(lower_bound <- exper_zero - qt(0.025,1194) * se_exper_zero)
(upper_bound <- exper_zero + qt(0.025,1194) * se_exper_zero)


