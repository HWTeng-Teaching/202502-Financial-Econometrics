url <- "https://www.principlesofeconometrics.com/poe5/data/rdata/cps5_small.rdata"
file_path <- "cps5_small.rdata"
download.file(url, file_path, mode = "wb")
load(file_path)
ls()
head(cps5_small)

model <- lm(log(wage) ~ educ + I(educ^2) + exper + I(exper^2) + I(educ * exper), data = cps5_small)
summary(model)

cps5_small$marginal_educ <- coef(model)["educ"] +
  2 * coef(model)["I(educ^2)"] * cps5_small$educ +
  coef(model)["I(educ * exper)"] * cps5_small$exper

hist(cps5_small$marginal_educ,
     breaks = 40,
     main = "Marginal Effect of EDUC",
     xlab = "∂ln(WAGE)/∂EDUC",
     col = "lightblue",
     border = "black")


quantile(cps5_small$marginal_educ, probs = c(0.05, 0.5, 0.95))
cps5_small$marginal_exper <- coef(model)["exper"] +
  2 * coef(model)["I(exper^2)"] * cps5_small$exper +
  coef(model)["I(educ * exper)"] * cps5_small$educ

hist(cps5_small$marginal_exper,
     breaks = 40,
     main = "Marginal Effect of EXPER",
     xlab = "∂ln(WAGE)/∂EXPER",
     col = "lightgreen",
     border = "black")

quantile(cps5_small$marginal_exper, probs = c(0.05, 0.5, 0.95))

vcov_matrix2 <- vcov(model)
c_vec2 <- c(0, 1, 33, -10,-260,-152)
coef_column_vector <- matrix(coef(model), ncol = 1)
betaf <- c_vec2 %*% coef_column_vector
sef <- sqrt(t(c_vec2) %*% vcov_matrix2 %*% c_vec2)
sef
qt(0.95,1194)

vcov_matrix2 <- vcov(model)
c_vec3 <- c(0, 1, 33, -10,-420,-144)
betag <- c_vec3 %*% coef_column_vector
seg <- sqrt(t(c_vec3) %*% vcov_matrix2 %*% c_vec3)
seg


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
se_exper_zero
exper_zero <- (-beta_values["exper"] - beta_values["I(educ * exper)"] * 16) / (2 * beta_values["I(exper^2)"])-11
lower_bound <- exper_zero - qt(0.025,1194) * se_exper_zero
upper_bound <- exper_zero + qt(0.025,1194) * se_exper_zero
qt(0.025,1194)