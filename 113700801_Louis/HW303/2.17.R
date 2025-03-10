# Nettoyer l'environnement (optionnel)
rm(list=ls())

# Charger les bibliothèques nécessaires
library(POE5Rdata)
library(dplyr)
library(ggplot2)

# Charger les données
data("collegetown")

# ----- Modèle linéaire : PRICE = b1 + b2 * SQFT + e -----
mod1 <- lm(price ~ sqft, data = collegetown)
sum_mod1 <- summary(mod1)

# Afficher le résumé du modèle linéaire
print(sum_mod1)

# Extraire et afficher les coefficients
b1 <- coef(mod1)[[1]]
b2 <- coef(mod1)[[2]]
cat("b1:", b1, "\n")
cat("b2:", b2, "\n")

# Visualisation du modèle linéaire
p1 <- ggplot(collegetown, aes(x = sqft, y = price)) +
  geom_point(color = "blue", alpha = 0.7) +
  geom_smooth(method = "lm", color = "red") +
  labs(title = "Prix des maisons en fonction de la taille",
       x = "Taille de la maison (sq ft)",
       y = "Prix de la maison ($)") +
  theme_minimal()
print(p1)

# ----- Modèle quadratique : PRICE = α₁ + α₂ * SQFT² + e -----
mod_quad <- lm(price ~ I(sqft^2), data = collegetown)
sum_mod_quad <- summary(mod_quad)
print(sum_mod_quad)

# Extraire les coefficients
alpha1 <- coef(mod_quad)[1]
alpha2 <- coef(mod_quad)[2]
cat("α₁:", round(alpha1, 4), "\n")
cat("α₂:", round(alpha2, 6), "\n")

# Calcul du l'effet marginal pour 2000 sq ft (converti en centaines de sq ft)
sqft_2000 <- 20
marginal_effect <- 2 * alpha2 * sqft_2000
cat("\nEffet marginal à 2000 sq ft:", round(marginal_effect, 4), "k$\n")

# ----- Visualisation du modèle quadratique -----
sqft_range <- seq(min(collegetown$sqft), max(collegetown$sqft), length.out = 200)
price_pred <- predict(mod_quad, newdata = data.frame(sqft = sqft_range))
price_at_2000 <- alpha1 + alpha2 * sqft_2000^2
tangent_line <- price_at_2000 + marginal_effect * (sqft_range - sqft_2000)

plot(collegetown$sqft, collegetown$price,
     xlab = "Taille (100 sq ft)", ylab = "Prix (k$)",
     main = "Régression quadratique du prix des maisons",
     pch = 19, col = adjustcolor("gray30", alpha.f = 0.6), cex = 0.8)
lines(sqft_range, price_pred, col = "blue", lwd = 2.5)
lines(sqft_range, tangent_line, col = "red", lwd = 2, lty = 2)
legend("topleft", legend = c("Régression quadratique", "Tangente à 2000 sq ft"),
       col = c("blue", "red"), lty = c(1, 2), lwd = c(2.5, 2), bg = "white")

# ----- Calcul de l'élasticité -----
price_at_2000 <- alpha1 + alpha2 * sqft_2000^2
elasticity <- marginal_effect * (sqft_2000 / price_at_2000)
cat("\nÉlasticité du prix par rapport à la taille:", round(elasticity, 4), "\n")

# ----- Analyse des résidus -----
residuals_mod1 <- resid(mod1)
residuals_mod_quad <- resid(mod_quad)

plot(collegetown$sqft, residuals_mod1,
     main="Résidus du modèle linéaire",
     xlab="SQFT", ylab="Résidus", pch=16)
plot(collegetown$sqft, residuals_mod_quad,
     main="Résidus du modèle quadratique",
     xlab="SQFT", ylab="Résidus", pch=16)

# ----- Comparaison de SSE -----
sse_mod1 <- sum(residuals_mod1^2)
sse_mod_quad <- sum(residuals_mod_quad^2)
cat("SSE du modèle linéaire:", sse_mod1, "\n")
cat("SSE du modèle quadratique:", sse_mod_quad, "\n")

# ----- Critères de sélection du modèle -----
cat("\nComparaison des critères statistiques:\n")
cat("R² du modèle linéaire:", summary(mod1)$r.squared, "\n")
cat("R² du modèle quadratique:", summary(mod_quad)$r.squared, "\n")
cat("AIC du modèle linéaire:", AIC(mod1), "\n")
cat("AIC du modèle quadratique:", AIC(mod_quad), "\n")
cat("BIC du modèle linéaire:", BIC(mod1), "\n")
cat("BIC du modèle quadratique:", BIC(mod_quad), "\n")
