library(lmtest)
library(sandwich)
library(POE5Rdata)
data(cps5)
?data
#a
data$lnWAGE <- log(data$wage)
formula <- lnWAGE ~ educ + exper + I(exper^2) + metro

male_data <- subset(data, female == 0)
fit_male <- lm(formula, data = male_data)
rss_male <- sum(resid(fit_male)^2)
df_male <- df.residual(fit_male)

female_data <- subset(data, female == 1)
fit_female <- lm(formula, data = female_data)
rss_female <- sum(resid(fit_female)^2)
df_female <- df.residual(fit_female)

F_stat <- max(rss_male / df_male, rss_female / df_female) /
  min(rss_male / df_male, rss_female / df_female)

df1 <- max(df_male, df_female)
df2 <- min(df_male, df_female)
F_critical <- qf(0.975, df1, df2)  

cat("===== 8.18(a) Goldfeld–Quandt Test (revised) =====\n")
cat("F-statistic =", round(F_stat, 4), "\n")
cat("Critical value (2.5% upper tail) =", round(F_critical, 4), "\n")

if (F_stat > F_critical) {
  cat("→ Reject H₀: There is evidence of heteroskedasticity (σ²_M ≠ σ²_F).\n")
} else {
  cat("→ Fail to reject H₀: No significant difference in error variances.\n")
}
#b
formula_all <- lnWAGE ~ educ + exper + I(exper^2) + female + black +
  metro + south + midwest + west
ols_model <- lm(formula_all, data = data)

data$resid2 <- resid(ols_model)^2

aux_model1 <- lm(resid2 ~ metro + female + black, data = data)
N <- nrow(data)
R2 <- summary(aux_model1)$r.squared
NR2_stat1 <- N * R2
p_val1 <- 1 - pchisq(NR2_stat1, df = 3)

cat("\n(b) NR² Test with (metro, female, black):\n")
cat("NR² =", round(NR2_stat1, 3), "  p-value =", round(p_val1, 4), "\n")
if (p_val1 < 0.01) {
  cat("→ Reject H₀ at 1% level: Evidence of heteroskedasticity.\n")
} else {
  cat("→ Fail to reject H₀: No strong evidence of heteroskedasticity.\n")
}

aux_model2 <- lm(resid2 ~ educ + exper + I(exper^2) + female + black +
                   metro + south + midwest + west, data = data)
R2_full <- summary(aux_model2)$r.squared
df_full <- length(coef(aux_model2)) - 1
NR2_stat2 <- N * R2_full
p_val2 <- 1 - pchisq(NR2_stat2, df = df_full)

cat("\nNR² Test using ALL RHS variables:\n")
cat("NR² =", round(NR2_stat2, 3), "  p-value =", round(p_val2, 4), "\n")

#c
data$resid_sq <- resid(ols_model)^2

X <- model.matrix(ols_model)[, -1]
X_df <- as.data.frame(X)
var_names <- colnames(X_df)

for (v in var_names) {
  X_df[[paste0(v, "_sq")]] <- X_df[[v]]^2
}

for (i in 1:(ncol(X) - 1)) {
  for (j in (i + 1):ncol(X)) {
    X_df[[paste0(var_names[i], "_x_", var_names[j])]] <- X[, i] * X[, j]
  }
}
aux_white <- lm(resid_sq ~ ., data = X_df)
N  <- nrow(data)
R2 <- summary(aux_white)$r.squared
q  <- length(coef(aux_white)) - 1
LM <- N * R2
crit_white <- qchisq(0.95, df = q)
p_white <- 1 - pchisq(LM, df = q)

cat("\n(c) White Test results:\n")
cat("LM (NR²) =", round(LM, 3), "\n")
cat("Degrees of freedom =", q, "\n")
cat("5% critical value =", round(crit_white, 3), "\n")
cat("p-value =", round(p_white, 4), "\n")
if (LM > crit_white) {
  cat("→ Reject H₀: Evidence of heteroskedasticity.\n")
} else {
  cat("→ Fail to reject H₀: No strong evidence of heteroskedasticity.\n")
}

#d
alpha <- 0.05
t_crit <- qt(1 - alpha/2, df = df.residual(ols_model))

coef_ols <- coef(ols_model)
se_ols   <- sqrt(diag(vcov(ols_model)))
lower_ols <- coef_ols - t_crit * se_ols
upper_ols <- coef_ols + t_crit * se_ols

rob_cov  <- vcovHC(ols_model, type = "HC1")
se_rob   <- sqrt(diag(rob_cov))
lower_rob <- coef_ols - t_crit * se_rob   
upper_rob <- coef_ols + t_crit * se_rob

ci_tbl <- data.frame(
  Variable   = names(coef_ols),
  OLS_Lower  = round(lower_ols, 4),
  OLS_Upper  = round(upper_ols, 4),
  ROB_Lower  = round(lower_rob, 4),
  ROB_Upper  = round(upper_rob, 4),
  Width_OLS  = round(upper_ols - lower_ols, 4),
  Width_ROB  = round(upper_rob - lower_rob, 4)
)

ci_tbl$Direction <- ifelse(ci_tbl$Width_ROB > ci_tbl$Width_OLS,
                           "Robust CI wider",
                           "Robust CI narrower")

cat("\n(d) 95% Confidence Intervals: OLS vs Robust\n")
print(ci_tbl[, c("Variable", "OLS_Lower", "OLS_Upper",
                 "ROB_Lower", "ROB_Upper", "Direction")],
      row.names = FALSE)
#e
data$abs_e <- abs(resid(ols_model))
var_fit <- lm(abs_e ~ metro + exper, data = data)
sigma2_hat <- fitted(var_fit)^2       

# FGLS (WLS) estimation
fgls <- lm(formula_all, data = data, weights = 1 / sigma2_hat)
fgls_ci <- confint(fgls, level = 0.95)

cat("\n(e) FGLS (conventional SE) – 95% CIs for key variables:\n")
print(round(fgls_ci, 4))

#f
fgls_covH <- vcovHC(fgls, type = "HC1")
fgls_rob_ci <- cbind(
  Estimate = coef(fgls),
  Lower    = coef(fgls) - 1.96 * sqrt(diag(fgls_covH)),
  Upper    = coef(fgls) + 1.96 * sqrt(diag(fgls_covH))
)

cat("\n(f) FGLS with Robust SE – 95% CIs for key variables:\n")
print(round(fgls_rob_ci, 4))
