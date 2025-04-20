data("cps5_small.csv")

# (a) Estimation du modèle
model <- lm(log(WAGE) ~ EDUC + I(EDUC^2) + EXPER + I(EXPER^2) + I(EDUC * EXPER), data = data)
summary_model <- summary(model)
cat("\n(a) Résultats de la régression :\n")
print(summary_model)

# (b) Effet marginal ∂E[ln(WAGE)]/∂EDUC = β2 + 2β3*EDUC + β6*EXPER
coef_vals <- coef(model)
marginal_educ <- with(data, coef_vals["EDUC"] + 2 * coef_vals["I(EDUC^2)"] * EDUC + coef_vals["I(EDUC * EXPER)"] * EXPER)
cat("\n(b) Exemple d'effet marginal d'EDUC sur log(WAGE) :\n")
cat("Formule : β2 + 2β3*EDUC + β6*EXPER\n")
cat("Croît avec EXPER et EDUC\n")

# (c) Histogramme et percentiles des effets marginaux EDUC
ggplot(data.frame(marginal_educ), aes(x = marginal_educ)) +
  geom_histogram(bins = 30, fill = "skyblue", color = "black") +
  ggtitle("Histogramme des effets marginaux de EDUC") +
  xlab("Effet marginal") + ylab("Fréquence")

cat("\n(c) Statistiques des effets marginaux EDUC:\n")
cat("Médiane:", median(marginal_educ), "\n")
cat("5e percentile:", quantile(marginal_educ, 0.05), "\n")
cat("95e percentile:", quantile(marginal_educ, 0.95), "\n")

# (d) Effet marginal ∂E[ln(WAGE)]/∂EXPER = β4 + 2β5*EXPER + β6*EDUC
marginal_exper <- with(data, coef_vals["EXPER"] + 2 * coef_vals["I(EXPER^2)"] * EXPER + coef_vals["I(EDUC * EXPER)"] * EDUC)
cat("\n(d) Exemple d'effet marginal d'EXPER sur log(WAGE) :\n")
cat("Formule : β4 + 2β5*EXPER + β6*EDUC\n")

# (e) Histogramme et stats marginal_exper
ggplot(data.frame(marginal_exper), aes(x = marginal_exper)) +
  geom_histogram(bins = 30, fill = "orange", color = "black") +
  ggtitle("Histogramme des effets marginaux de EXPER") +
  xlab("Effet marginal") + ylab("Fréquence")

cat("\n(e) Statistiques des effets marginaux EXPER:\n")
cat("Médiane:", median(marginal_exper), "\n")
cat("5e percentile:", quantile(marginal_exper, 0.05), "\n")
cat("95e percentile:", quantile(marginal_exper, 0.95), "\n")

# (f) Comparaison David vs Svetlana
# David: EDUC=17, EXPER=8
# Svetlana: EDUC=16, EXPER=18

X_david <- c(1, 17, 17^2, 8, 8^2, 17 * 8)
X_svet <- c(1, 16, 16^2, 18, 18^2, 16 * 18)
diff_vec <- X_david - X_svet
var_covar <- vcov(model)
se_diff <- sqrt(t(diff_vec) %*% var_covar %*% diff_vec)
diff_mean <- sum(diff_vec * coef_vals)
t_stat <- diff_mean / se_diff
p_val <- pt(t_stat, df = df.residual(model), lower.tail = FALSE)

cat("\n(f) Test David > Svetlana (one-sided):\n")
cat("t =", t_stat, ", p-value =", p_val, "\n")
cat(ifelse(p_val < 0.05, "Conclusion: David a une espérance de salaire log > Svetlana\n", "Conclusion: pas de différence significative\n"))

# (g) Après 8 ans d'expérience
X_david2 <- c(1, 17, 17^2, 16, 16^2, 17 * 16)
X_svet2 <- c(1, 16, 16^2, 26, 26^2, 16 * 26)
diff_vec2 <- X_david2 - X_svet2
se_diff2 <- sqrt(t(diff_vec2) %*% var_covar %*% diff_vec2)
diff_mean2 <- sum(diff_vec2 * coef_vals)
t_stat2 <- diff_mean2 / se_diff2
p_val2 <- pt(t_stat2, df = df.residual(model), lower.tail = FALSE)

cat("\n(g) Même test après 8 ans:\n")
cat("t =", t_stat2, ", p-value =", p_val2, "\n")
cat(ifelse(p_val2 < 0.05, "Conclusion: David > Svetlana après 8 ans\n", "Conclusion: pas de différence significative\n"))

# (h) Comparaison Wendy vs Jill (marginal effects EXPER)
# Wendy: EDUC=12, EXPER=17
# Jill: EDUC=16, EXPER=11
wendy_marg <- coef_vals["EXPER"] + 2 * coef_vals["I(EXPER^2)"] * 17 + coef_vals["I(EDUC * EXPER)"] * 12
jill_marg <- coef_vals["EXPER"] + 2 * coef_vals["I(EXPER^2)"] * 11 + coef_vals["I(EDUC * EXPER)"] * 16
diff_marg <- wendy_marg - jill_marg

# Approximate standard error of difference using delta method
dW <- c(0, 0, 0,
