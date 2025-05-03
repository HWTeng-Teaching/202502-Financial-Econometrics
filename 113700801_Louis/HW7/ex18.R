remotes::install_github("ccolonescu/POE5Rdata", force = TRUE)
library(POE5Rdata)
data(cps5)

# Chargement des packages nécessaires
install.packages(c("lmtest", "sandwich", "car", "foreign")) # si pas déjà installés
library(lmtest)
library(sandwich)
library(car)


data$lnWAGE <- log(data$WAGE)
model <- lm(lnWAGE ~ EDUC + EXPER + I(EXPER^2) + FEMALE + BLACK + METRO + SOUTH + MIDWEST + WEST, data = data)

# (a) Test de Goldfeld-Quandt pour variances entre hommes et femmes
data_male <- subset(data, FEMALE == 0)
data_female <- subset(data, FEMALE == 1)

model_male <- lm(lnWAGE ~ EDUC + EXPER + I(EXPER^2) + METRO, data = data_male)
model_female <- lm(lnWAGE ~ EDUC + EXPER + I(EXPER^2) + METRO, data = data_female)

SSR_male <- sum(resid(model_male)^2)
SSR_female <- sum(resid(model_female)^2)

n_male <- nrow(data_male)
n_female <- nrow(data_female)
k <- 4  # nombre de régressseurs

F_stat <- (SSR_female / (n_female - k)) / (SSR_male / (n_male - k))
crit_val <- qf(0.975, df1 = n_female - k, df2 = n_male - k)

cat("\n(a) Goldfeld–Quandt Test:\n")
cat("F-statistic =", F_stat, "\n")
cat("Critical value (5%) =", crit_val, "\n")
cat(ifelse(F_stat > crit_val, "Conclusion: Reject H0 (heteroskedasticity likely between genders)\n", "Conclusion: Do not reject H0\n"))

# (b) NR² test avec METRO, FEMALE, BLACK
residuals_sq <- resid(model)^2
nr2_model1 <- lm(residuals_sq ~ METRO + FEMALE + BLACK, data = data)
NR2_stat1 <- summary(nr2_model1)$r.squared * nrow(data)
crit_val_nr2_1 <- qchisq(0.99, df = 3)

# NR² test avec toutes les variables
nr2_model2 <- lm(residuals_sq ~ EDUC + EXPER + I(EXPER^2) + FEMALE + BLACK + METRO + SOUTH + MIDWEST + WEST, data = data)
NR2_stat2 <- summary(nr2_model2)$r.squared * nrow(data)
crit_val_nr2_2 <- qchisq(0.99, df = 9)

cat("\n(b) NR² Test:\n")
cat("NR² (partial) =", NR2_stat1, " Critical value =", crit_val_nr2_1, "\n")
cat(ifelse(NR2_stat1 > crit_val_nr2_1, "Conclusion: Heteroskedasticity detected (partial)\n", "No heteroskedasticity (partial)\n"))
cat("NR² (full) =", NR2_stat2, " Critical value =", crit_val_nr2_2, "\n")
cat(ifelse(NR2_stat2 > crit_val_nr2_2, "Conclusion: Heteroskedasticity detected (full model)\n", "No heteroskedasticity (full model)\n"))

# (c) White test
white_test <- bptest(model, ~ fitted(model) + I(fitted(model)^2))
cat("\n(c) White test:\n")
cat("Statistic =", white_test$statistic, " df =", white_test$parameter, " p-value =", white_test$p.value, "\n")
cat(ifelse(white_test$p.value < 0.05, "Conclusion: Heteroskedasticity present.\n", "No heteroskedasticity.\n"))

# (d) Estimation avec erreurs robustes
robust_se <- coeftest(model, vcov = vcovHC(model, type = "HC0"))
classic_se <- summary(model)$coefficients[, 2]
robust_interval <- confint(model, level = 0.95)
robust_confint <- confint.default(model, level = 0.95)

cat("\n(d) Comparison of standard errors:\n")
for (i in 1:length(classic_se)) {
  wider <- ifelse(robust_se[i,2] > classic_se[i], "wider", "narrower")
  cat(names(classic_se)[i], ": robust SE is", wider, "\n")
}

# (e) FGLS avec variables METRO et EXPER
aux_gls <- lm(log(resid(model)^2) ~ METRO + EXPER, data = data)
weights_gls <- 1 / exp(fitted(aux_gls))
fgls_model <- lm(lnWAGE ~ EDUC + EXPER + I(EXPER^2) + FEMALE + BLACK + METRO + SOUTH + MIDWEST + WEST, data = data, weights = weights_gls)
fgls_confint <- confint(fgls_model)

cat("\n(e) FGLS 95% confidence intervals:\n")
print(fgls_confint)

# (f) FGLS + erreurs robustes
fgls_robust_se <- coeftest(fgls_model, vcov = vcovHC(fgls_model, type = "HC0"))
cat("\n(f) FGLS with robust SEs:\n")
print(fgls_robust_se)

# (g) Recommandation
cat("\n(g) Recommendation:\n")
cat("If heteroskedasticity is detected (as seen in NR² and White tests), reporting FGLS with robust standard errors\nis most appropriate for valid inference. It corrects for both heteroskedasticity and inefficiency of OLS.\n")
