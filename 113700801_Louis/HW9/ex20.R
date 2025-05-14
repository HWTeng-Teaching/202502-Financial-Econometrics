
# a. Estimation OLS
capm5$risk_premium_j <- capm5$rj - capm5$rf
capm5$risk_premium_m <- capm5$rm - capm5$rf

ols_model <- lm(risk_premium_j ~ risk_premium_m, data = capm5)
summary(ols_model)

# b. Création de RANK IV
capm5$RANK <- rank(capm5$risk_premium_m)
first_stage_b <- lm(risk_premium_m ~ RANK, data = capm5)
summary(first_stage_b)  # Coefficient significatif ?
cat("R2:", summary(first_stage_b)$r.squared)

# c. Test d’endogénéité (résidus de première étape)
capm5$v_hat <- resid(first_stage_b)
augmented_model <- lm(risk_premium_j ~ risk_premium_m + v_hat, data = capm5)
summary(augmented_model)  # Tester la significativité de v_hat à 1%

# d. Estimation IV avec RANK comme IV
iv_model_d <- ivreg(risk_premium_j ~ risk_premium_m | RANK, data = capm5)
summary(iv_model_d)

# e. Ajouter la variable POS
capm5$POS <- as.numeric(capm5$risk_premium_m > 0)
first_stage_e <- lm(risk_premium_m ~ RANK + POS, data = capm5)
summary(first_stage_e)
linearHypothesis(first_stage_e, c("RANK = 0", "POS = 0"))  # Test de significativité conjointe
cat("R2 (2 IVs):", summary(first_stage_e)$r.squared)

# f. Test de Hausman
capm5$v_hat2 <- resid(first_stage_e)
hausman_model <- lm(risk_premium_j ~ risk_premium_m + v_hat2, data = capm5)
summary(hausman_model)  # Tester la significativité de v_hat2 à 1%

# g. Estimation IV avec RANK et POS comme IVs
iv_model_g <- ivreg(risk_premium_j ~ risk_premium_m | RANK + POS, data = capm5)
summary(iv_model_g)

# h. Test de Sargan (test de suridentification)
# Obtenir les résidus
capm5$resid_iv <- resid(iv_model_g)

# Régresser les résidus IV sur les instruments
sargan_test <- lm(resid_iv ~ RANK + POS, data = capm5)
n <- nrow(capm5)
R2_sargan <- summary(sargan_test)$r.squared
sargan_stat <- n * R2_sargan
p_value <- 1 - pchisq(sargan_stat, df = 1)  # df = nb instruments - nb variables instrumentées

cat("Sargan statistic:", sargan_stat, "\nP-value:", p_value, "\n")
