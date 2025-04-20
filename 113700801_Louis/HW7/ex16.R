remotes::install_github("ccolonescu/POE5Rdata", force = TRUE)
library(POE5Rdata)
data(vacation)

ols_model <- lm(MILES ~ INCOME + AGE + KIDS, data = vacation)
summary(ols_model)
confint(ols_model, "KIDS", level = 0.95)

residuals <- resid(ols_model)

plot(vacation$INCOME, residuals,
     xlab = "INCOME", ylab = "Résidus",
     main = "Résidus vs INCOME")
abline(h = 0, col = "red")

plot(vacation$AGE, residuals,
     xlab = "AGE", ylab = "Résidus",
     main = "Résidus vs AGE")
abline(h = 0, col = "red")

# Trier les données selon INCOME
vacation_sorted <- vacation[order(vacation$INCOME), ]

# Choisir les 90 premières et 90 dernières observations
low_group <- vacation_sorted[1:90, ]
high_group <- vacation_sorted[(nrow(vacation_sorted)-89):nrow(vacation_sorted), ]

# Estimer les modèles sur les deux groupes
model_low <- lm(MILES ~ INCOME + AGE + KIDS, data = low_group)
model_high <- lm(MILES ~ INCOME + AGE + KIDS, data = high_group)

# Somme des carrés des résidus
SSR_low <- sum(resid(model_low)^2)
SSR_high <- sum(resid(model_high)^2)

# Calcul de la statistique F
F_stat <- SSR_high / SSR_low
df1 <- 90 - 4
df2 <- 90 - 4
critical_value <- qf(0.95, df1, df2)

cat("F-statistic =", F_stat, "\n")
cat("Critical value (5%) =", critical_value, "\n")

if (F_stat > critical_value) {
  cat("On rejette H0 : hétéroscédasticité présente.\n")
} else {
  cat("On ne rejette pas H0 : pas de preuve d'hétéroscédasticité.\n")
}

# Coefficient + erreurs standards robustes (type White)
coeftest(ols_model, vcov = vcovHC(ols_model, type = "HC0"))

# Intervalle de confiance robuste pour KIDS
robust_se <- sqrt(diag(vcovHC(ols_model, type = "HC0")))
coef_kids <- coef(ols_model)["KIDS"]
se_kids <- robust_se["KIDS"]
CI_robust <- coef_kids + c(-1, 1) * 1.96 * se_kids
CI_robust

# Créer les poids inverses : 1 / INCOME^2
vacation$weights <- 1 / (vacation$INCOME^2)

# Estimation GLS pondérée
gls_model <- lm(MILES ~ INCOME + AGE + KIDS, data = vacation, weights = weights)

# Intervalle de confiance GLS classique
confint(gls_model, "KIDS", level = 0.95)

# Coefficients + erreurs robustes sur GLS
coeftest(gls_model, vcov = vcovHC(gls_model, type = "HC0"))

# Intervalle de confiance robuste pour KIDS (GLS)
robust_gls_se <- sqrt(diag(vcovHC(gls_model, type = "HC0")))
coef_kids_gls <- coef(gls_model)["KIDS"]
se_kids_gls <- robust_gls_se["KIDS"]
CI_robust_gls <- coef_kids_gls + c(-1, 1) * 1.96 * se_kids_gls
CI_robust_gls
