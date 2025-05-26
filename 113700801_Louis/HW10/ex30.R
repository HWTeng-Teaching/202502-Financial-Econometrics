
klein <- read.csv("klein.csv")

# a. Estimate investment function (eq 11.18) by OLS
# Equation: I_t = beta1 + beta2*P_t + beta3*P_{t-1} + beta4*K_{t-1} + error
i_ols <- lm(I ~ P + P_1 + K_1, data = klein)
summary(i_ols)

# b. Estimate reduced-form for P using all 8 exogenous + predetermined variables
p_rf <- lm(P ~ G + T + W + W_1 + K + K_1 + P_1 + X, data = klein)
summary(p_rf)

# Test joint significance of all variables except P_1 and K_1
linearHypothesis(p_rf, c("G = 0", "T = 0", "W = 0", "W_1 = 0", "K = 0", "X = 0"))

# Save residuals and fitted values
klein$v_hat <- resid(p_rf)
klein$P_hat <- fitted(p_rf)

# c. Hausman test: add v_hat to structural equation
hausman_test <- lm(I ~ P + P_1 + K_1 + v_hat, data = klein)
summary(hausman_test)
# Check significance of v_hat (null: delta = 0)

# d. 2SLS estimation using all 8 instruments (G, T, W, W_1, K, K_1, P_1, X)
i_iv <- ivreg(I ~ P + P_1 + K_1 | G + T + W + W_1 + K + K_1 + P_1 + X, data = klein)
summary(i_iv)

# e. Manual 2SLS second stage: I ~ P_hat + P_1 + K_1
stage2 <- lm(I ~ P_hat + P_1 + K_1, data = klein)
summary(stage2)

# f. Sargan test for overidentifying restrictions
# Residuals from 2SLS (stage2)
klein$e2 <- resid(stage2)
sargan_test <- lm(e2 ~ G + T + W + W_1 + K + K_1 + P_1 + X, data = klein)
sargan_r2 <- summary(sargan_test)$r.squared

# Test statistic: TR^2
n <- nrow(klein)
TR2 <- n * sargan_r2
chi_crit <- qchisq(0.95, df = 4)  # 4 = L - B = 5 - 1
cat("Sargan test statistic:", TR2, "\n")
cat("Critical value (Chi2, 0.95, df=4):", chi_crit, "\n")

if (TR2 > chi_crit) {
  cat("Reject H0: Instruments may not be valid.\n")
} else {
  cat("Do not reject H0: Instruments appear valid.\n")
}
