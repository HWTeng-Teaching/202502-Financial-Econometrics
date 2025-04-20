# 5.31.a
lm_model <- lm(time ~ depart + reds + trains, data = commute5)
summary(lm_model)

# 5.31.b
confint(lm_model, level = 0.95)

# 5.31.c
qt(0.05, 245)

# 5.31.f
vcov_matrix <- vcov(lm_model)
contrast_vector <- c(0, 0, -3, 1)  
(sef <- sqrt(t(contrast_vector) %*% vcov_matrix %*% contrast_vector))

# 5.31.g
new_data <- data.frame(depart = 30, reds = 6, trains = 1)
(predicted_TIME <- predict(lm_model, newdata = new_data, se.fit = TRUE))

# 5.33.a
data("cps5_small")
model <- lm(log(wage) ~ educ + I(educ^2) + exper + I(exper^2) + I(educ * exper), data = cps5_small)
summary(model)

# 5.33.b
install.packages("dplyr")  
library(dplyr)
coefficients <- coef(model)
cps5_small <- cps5_small %>%
  mutate(marginal_effect_educ = coefficients["educ"] + 2 * coefficients["I(educ^2)"] * educ + coefficients["I(educ * exper)"] * exper)
head(cps5_small$marginal_effect_educ)

# 5.33.c
quantile(cps5_small$marginal_effect_educ, probs = c(0.05, 0.5, 0.95))
hist(cps5_small$marginal_effect_educ, main="Histogram of Marginal Effect of EDUC", 
     xlab="Marginal Effect of EDUC", col="lightblue", border="black", breaks = 40)

# 5.33.d
cps5_small <- cps5_small %>%
  mutate(marginal_effect_exper = coefficients["exper"] + 2 * coefficients["I(exper^2)"] * exper + coefficients["I(educ * exper)"] * educ)
head(cps5_small$marginal_effect_exper)

# 5.33.e
quantile(cps5_small$marginal_effect_exper, probs = c(0.05, 0.5, 0.95))
hist(cps5_small$marginal_effect_exper, main="Histogram of Marginal Effect of EXPER", 
     xlab="Marginal Effect of EXPER", col="lightblue", border="black", breaks = 40)

# 5.33.f
vcov_matrix2 <- vcov(model)
contrast_vector2 <- c(0, 1, 33, -10,-260,-152)
coef_matrix <- matrix(coef(model), ncol = 1)
(betaf <- contrast_vector2 %*% coef_matrix)
(sef2 <- sqrt(t(contrast_vector2) %*% vcov_matrix2 %*% contrast_vector2))

# 5.33.g
vcov_matrix2 <- vcov(model)
contrast_vector3 <- c(0, 1, 33, -10,-420,-144)
(betag <- contrast_vector3 %*% coef_matrix)
(seg <- sqrt(t(contrast_vector3) %*% vcov_matrix2 %*% contrast_vector3))

# 5.33.h
vcov_matrix2 <- vcov(model)
contrast_vector4 <- c(0, 0, 0, 0, 12,-4)
(betah <- contrast_vector4 %*% coef_matrix)
(seh <- sqrt(t(contrast_vector4) %*% vcov_matrix2 %*% contrast_vector4))

# 5.33.i
beta_vals <- coef(model)
cov_matrix <- vcov(model)

d_beta3 <- -1 / (2 * beta_vals["I(exper^2)"])
d_beta4 <- (beta_vals["exper"] + beta_vals["I(educ * exper)"] * 16) / (2 * beta_vals["I(exper^2)"]^2)
d_beta5 <- -16 / (2 * beta_vals["I(exper^2)"])

se_exper_zero <- sqrt(
  d_beta3^2 * cov_matrix["exper", "exper"] + 
    d_beta4^2 * cov_matrix["I(exper^2)", "I(exper^2)"] + 
    d_beta5^2 * cov_matrix["I(educ * exper)", "I(educ * exper)"] +
    2 * d_beta4 * d_beta3 * cov_matrix["I(exper^2)", "exper"] +
    2 * d_beta3 * d_beta5 * cov_matrix["exper", "I(educ * exper)"] +
    2 * d_beta4 * d_beta5 * cov_matrix["I(exper^2)", "I(educ * exper)"]
)

(exper_zero <- (-beta_vals["exper"] - beta_vals["I(educ * exper)"] * 16) / (2 * beta_vals["I(exper^2)"])-11)
(lower_bound <- exper_zero - qt(0.025,1194) * se_exper_zero)
(upper_bound <- exper_zero + qt(0.025,1194) * se_exper_zero)
qt(0.025,1194)
