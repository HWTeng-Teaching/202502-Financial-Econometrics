#5.33
# (a)
data("cps5_small")
model <- lm(log(wage) ~ educ + I(educ^2) + exper + I(exper^2) + I(educ * exper), data = cps5_small)
summary(model)

# (b)
install.packages("dplyr")  
library(dplyr)
coef <- coef(model)
cps5_small <- cps5_small %>%
  mutate(marginal_effect_educ = coef["educ"] + 2 * coef["I(educ^2)"] * educ + coef["I(educ * exper)"] * exper)
head(cps5_small$marginal_effect_educ)

# (c)
quantile(cps5_small$marginal_effect_educ, probs = c(0.05, 0.5, 0.95))
hist(cps5_small$marginal_effect_educ, xlab="EDUC",
     main="Marginal Effect of EDUC", breaks = 40)

# (d)
cps5_small <- cps5_small %>%
  mutate(marginal_effect_exper = coef["exper"] + 2 * coef["I(exper^2)"] * exper + coef["I(educ * exper)"] * educ)
head(cps5_small$marginal_effect_exper)

# (e)
quantile(cps5_small$marginal_effect_exper, probs = c(0.05, 0.5, 0.95))
hist(cps5_small$marginal_effect_exper,  xlab="EXPER",
     main="Marginal Effect of EXPER", breaks = 40)

# (f)
vcov_matrix2 <- vcov(model)
c_vec2 <- c(0, 1, 33, -10,-260,-152)
coef_column_vector <- matrix(coef(model), ncol = 1)
(betaf <- c_vec2 %*% coef_column_vector)
(sef2 <- sqrt(t(c_vec2) %*% vcov_matrix2 %*% c_vec2))

# (g)
vcov_matrix2 <- vcov(model)
c_vec3 <- c(0, 1, 33, -10,-420,-144)
(betag <- c_vec3 %*% coef_column_vector)
(seg <- sqrt(t(c_vec3) %*% vcov_matrix2 %*% c_vec3))

# (h)
vcov_matrix2 <- vcov(model)
c_vec4 <- c(0, 0, 0, 0, 12,-4)
(betah <- c_vec4 %*% coef_column_vector)
(seh <- sqrt(t(c_vec4) %*% vcov_matrix2 %*% c_vec4))

# (i)
beta_values <- coef(model)
cov_matrix <- vcov(model)

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