# Charger les données mroz (par exemple depuis le package AER)
data("mroz", package = "AER")

# Garder uniquement les femmes qui participent au marché du travail
mroz_active <- subset(Mroz, inlf == 1)
n <- nrow(mroz_active)

# Créer variables utiles
mroz_active$expersq <- mroz_active$exper^2

# Modèle IV/2SLS de base
iv_model <- ivreg(lwage ~ educ + exper + expersq | mothereduc + fathereduc + exper + expersq, data = mroz_active)
summary(iv_model)

# Résidus
mroz_active$resid_iv <- resid(iv_model)

# Graphe résidus vs expérience
plot(mroz_active$exper, mroz_active$resid_iv,
     main = "Résidus IV vs EXPER", xlab = "EXPER", ylab = "Résidus IV")
abline(h = 0, col = "red", lty = 2)

# Carré des résidus
mroz_active$resid_sq <- mroz_active$resid_iv^2

# Régression des carrés des résidus sur constante et EXPER
aux_reg <- lm(resid_sq ~ exper, data = mroz_active)
R2 <- summary(aux_reg)$r.squared
NR2 <- n * R2
pval <- 1 - pchisq(NR2, df = 1)

cat("Test NR²:\nStatistic:", NR2, "\nP-value:", pval, "\n")

# Estimation IV avec erreurs standards robustes
coeftest(iv_model, vcov = vcovHC(iv_model, type = "HC1"))

# Intervalle de confiance à 95% pour le coefficient de EDUC
coef_educ <- coef(iv_model)["educ"]
se_robust <- sqrt(diag(vcovHC(iv_model, type = "HC1")))["educ"]
ci_robust <- coef_educ + c(-1, 1) * 1.96 * se_robust
cat("IC 95% robuste pour EDUC:", ci_robust, "\n")

# Fonction de bootstrap
iv_boot <- function(data, indices) {
  d <- data[indices, ]
  model <- ivreg(lwage ~ educ + exper + expersq | mothereduc + fathereduc + exper + expersq, data = d)
  return(coef(model)["educ"])
}

set.seed(123)
boot_results <- boot(data = mroz_active, statistic = iv_boot, R = 200)

# Erreur standard bootstrap
se_boot <- sd(boot_results$t)
coef_educ  # repris du modèle initial
ci_boot <- coef_educ + c(-1, 1) * 1.96 * se_boot

cat("Erreur standard bootstrap pour EDUC:", se_boot, "\n")
cat("IC 95% bootstrap pour EDUC:", ci_boot, "\n")
