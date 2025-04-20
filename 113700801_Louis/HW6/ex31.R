data("commute5")

# (a) Régression
model <- lm(TIME ~ DEPART + REDS + TRAINS, data = data)
summary_model <- summary(model)
cat("\n(a) Estimation OLS:\n")
print(summary_model)

# interpretation
cat("\nInterprétation:\n")
cat("Intercept: Durée attendue quand toutes les variables explicatives sont nulles (départ à 6h30, aucun feu ni train).\n")
cat("DEPART: Chaque minute de départ plus tard augmente ou réduit le temps de trajet en moyenne.\n")
cat("REDS: Temps additionnel moyen par feu rouge.\n")
cat("TRAINS: Temps additionnel moyen par train.\n")

# (b) Intervalles de confiance à 95%
confint_95 <- confint(model, level = 0.95)
cat("\n(b) Intervalles de confiance à 95%:\n")
print(confint_95)

# (c) H0: effet d’un feu rouge >= 2 minutes vs H1: effet < 2
b3 <- coef(model)["REDS"]
se_b3 <- summary_model$coefficients["REDS", "Std. Error"]
t_c <- (b3 - 2) / se_b3
p_c <- pt(t_c, df = df.residual(model)) # unilatéral à gauche

cat("\n(c) Test effet feu rouge < 2 min:\n")
cat("t =", t_c, ", p-value =", p_c, "\n")
cat(ifelse(p_c < 0.05, "Conclusion: On rejette H0 (effet < 2 minutes)\n", "Conclusion: On ne rejette pas H0\n"))

# (d) H0: effet train = 3 vs H1: ≠ 3
b4 <- coef(model)["TRAINS"]
se_b4 <- summary_model$coefficients["TRAINS", "Std. Error"]
t_d <- (b4 - 3) / se_b4
p_d <- 2 * pt(-abs(t_d), df = df.residual(model)) # bilatéral

cat("\n(d) Test effet train = 3 min (vs ≠ 3):\n")
cat("t =", t_d, ", p-value =", p_d, "\n")
cat(ifelse(p_d < 0.10, "Conclusion: On rejette H0 au seuil de 10%\n", "Conclusion: On ne rejette pas H0\n"))

# (e) Test si départ à 7h30 dure ≥10 min de plus que 7h00
delta_depart <- 30  # minutes de différence
beta_depart <- coef(model)["DEPART"]
se_depart <- summary_model$coefficients["DEPART", "Std. Error"]
t_e <- (delta_depart * beta_depart - 10) / (delta_depart * se_depart)
p_e <- pt(t_e, df = df.residual(model))  # unilatéral à gauche

cat("\n(e) Test si 30 min plus tard => ≥ 10 min de plus:\n")
cat("t =", t_e, ", p-value =", p_e, "\n")
cat(ifelse(p_e < 0.05, "Conclusion: On rejette H0 (effet < 10 min)\n", "Conclusion: On ne rejette pas H0\n"))

# (f) H0: effet train ≥ 3 × effet feu rouge vs H1: effet train < 3 × effet feu rouge
t_f <- (b4 - 3 * b3) / sqrt(se_b4^2 + 9 * se_b3^2)  # approximation de la variance
p_f <- pt(t_f, df = df.residual(model))  # unilatéral à gauche

cat("\n(f) Test: effet train < 3 × effet feu:\n")
cat("t =", t_f, ", p-value =", p_f, "\n")
cat(ifelse(p_f < 0.05, "Conclusion: On rejette H0 (effet train < 3x effet feu)\n", "Conclusion: On ne rejette pas H0\n"))

# (g) Estimation de E(TIME | X) avec DEPART = 30 (7h00), REDS = 6, TRAINS = 1
x_g <- data.frame(DEPART = 30, REDS = 6, TRAINS = 1)
pred_g <- predict(model, newdata = x_g, interval = "confidence", level = 0.95)
cat("\n(g) Prévision à 7h00 avec 6 feux rouges et 1 train:\n")
print(pred_g)
cat(ifelse(pred_g[1] <= 45, "Conclusion: Pas de preuve que Bill sera en retard\n", "Conclusion: Bill risque d'être en retard\n"))

# (h) Inversion des hypothèses
cat("\n(h) Hypothèses inversées:\n")
cat("Si être à l'heure est impératif, H0 devrait être 'le trajet dure > 45 minutes'.\n")
cat("Si on rejette cette H0, alors on conclut que Bill sera probablement à l'heure.\n")
cat("Sinon, on ne peut pas garantir qu’il soit à l’heure. L’hypothèse H0 devrait protéger contre le pire cas.\n")
