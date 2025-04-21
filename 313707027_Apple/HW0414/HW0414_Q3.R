# 8.18
# (a)
data(cps5)
model <- lm(log(wage) ~ educ + exper + I(exper^2) + female + black + 
             metro + south + midwest + west, data = cps5)
cps5$resid_sq <- resid(model)^2
male <- subset(cps5, female == 0)
female <- subset(cps5, female == 1)
rss1 <- sum(male$resid_sq)
rss2 <- sum(female$resid_sq)
df1 <- nrow(male)-10
df2 <- nrow(female)-10
F_stat <- (rss1 / df1) / (rss2 / df2)

alpha <- 0.05
F_critical_upper <- qf(1 - alpha / 2, df1 , df2 )
F_critical_lower <- 1 / F_critical_upper

cat("F statistic:", F_stat, "\n")
cat("Critical region: F <", F_critical_lower, "or F >", F_critical_upper, "\n")

if (F_stat < F_critical_lower | F_stat > F_critical_upper) {
  cat("F statistic falls in the rejection region, so we reject the null hypothesis.\n")
} else {
  cat("F statistic NOT falls in the rejection region, so we cannot reject the null hypothesis.\n")
}

# (b)
aux_mod1 <- lm(resid_sq ~ metro + female + black, data = cps5)
R2_1 <- summary(aux_mod1)$r.squared
n <- nrow(cps5)
NR2_1 <- n * R2_1
df1 <- 3  
crit_val_1pct <- qchisq(0.99, df1)

cat("NR² test statistic (selected vars):", NR2_1, "\n")
cat("Critical value (1% level, df = 3):", crit_val_1pct, "\n")
if (NR2_1 > crit_val_1pct) {
  cat("F statistic falls in the rejection region, so we reject the null hypothesis.\n")
} else {
  cat("F statistic NOT falls in the rejection region, so we cannot reject the null hypothesis.\n")
}

aux_mod2 <- lm(resid_sq ~ educ + exper + I(exper^2) + female + black + 
                   metro + south + midwest + west, data = cps5)
R2_2 <- summary(aux_mod2)$r.squared
df2 <- 9
NR2_2 <- n * R2_2
crit_val_2 <- qchisq(0.99, df2)

cat("\nNR² test statistic (all vars):", NR2_2, "\n")
cat("Critical value (1% level, df = 9):", crit_val_2, "\n")
if (NR2_2 > crit_val_2) {
  cat("Test statistic falls in the rejection region, so we reject the null hypothesis.\n")
} else {
  cat("Test statistic NOT falls in the rejection region, so we cannot reject the null hypothesis.\n")
}

# (c)
cps5$educ_sq <- cps5$educ^2
cps5$exper_sq <- cps5$exper^2
cps5$female_black <- cps5$female * cps5$black
cps5$metro_female <- cps5$metro * cps5$female

aux_model <- lm(resid_sq ~ educ + exper + I(exper^2) + female + black + 
                  metro + south + midwest + west +
                  educ_sq + exper_sq + female_black + metro_female, 
                data = cps5)
n <- nrow(cps5)
R2 <- summary(aux_model)$r.squared
white_stat <- n * R2
df <- length(coef(aux_model)) - 1  # minus intercept
critical_val <- qchisq(0.95, df)

cat("White test statistic:", white_stat, "\n")
cat("Critical value at 5% level", critical_val, "\n")

if (white_stat > critical_val) {
  cat("Test statistic falls in the rejection region, so we reject the null hypothesis.\n")
} else {
  cat("Test statistic NOT falls in the rejection region, so we cannot reject the null hypothesis.\n")
}

# (d)
confint_ols <- confint(model)
robust_se <- vcovHC(model, type = "HC0")  # White robust SE
coeftest(model, vcov. = robust_se)
robust_coef <- coeftest(model, vcov. = robust_se)
robust_ci <- cbind(
  Estimate = robust_coef[, 1],
  Lower = robust_coef[, 1] - 1.96 * robust_coef[, 2],
  Upper = robust_coef[, 1] + 1.96 * robust_coef[, 2]
)

result_compare <- data.frame(
  OLS_Lower = confint_ols[, 1],
  OLS_Upper = confint_ols[, 2],
  Robust_Lower = robust_ci[, "Lower"],
  Robust_Upper = robust_ci[, "Upper"]
)

result_compare$OLS_Width <- result_compare$OLS_Upper - result_compare$OLS_Lower
result_compare$Robust_Width <- result_compare$Robust_Upper - result_compare$Robust_Lower
result_compare$Change <- ifelse(result_compare$Robust_Width > result_compare$OLS_Width,
                                "Wider", "Narrower")
print(result_compare)


# (e)
log_resid_sq <- log(cps5$resid_sq)
var_model <- lm(log_resid_sq ~ metro + exper, data = cps5)
log_fitted_var <- fitted(var_model)
weights <- 1 / exp(log_fitted_var)
fgls_model <- lm(log(wage) ~ educ + exper + I(exper^2) + female + black +
                   metro + south + midwest + west, data = cps5, weights = weights)
confint_fgls <- confint(fgls_model)
confint_robust <- cbind(
  Lower = coef(tab2) - 1.96 * sqrt(diag(vcovHC(tab2, type = "HC0"))),
  Upper = coef(tab2) + 1.96 * sqrt(diag(vcovHC(tab2, type = "HC0")))
)
comparison_df2 <- data.frame(
  FGLS_Lower = confint_fgls[, 1],
  FGLS_Upper = confint_fgls[, 2],
  Robust_Lower = confint_robust[, "Lower"],
  Robust_Upper = confint_robust[, "Upper"]
)
comparison_df2$FGLS_Width <- comparison_df2$FGLS_Upper - comparison_df2$FGLS_Lower
comparison_df2$Robust_Width <- comparison_df2$Robust_Upper - comparison_df2$Robust_Lower
comparison_df2$Change <- ifelse(comparison_df2$FGLS_Width > comparison_df2$Robust_Width, "Wider", "Narrower")

print(comparison_df2)


# (f)
robust_se_fgls <- coeftest(fgls_model, vcov. = vcovHC(fgls_model, type = "HC0"))

robust_ci_fgls <- cbind(
  Estimate = robust_se_fgls[, 1],
  Lower = robust_se_fgls[, 1] - 1.96 * robust_se_fgls[, 2],
  Upper = robust_se_fgls[, 1] + 1.96 * robust_se_fgls[, 2]
)

robust_ols_ci <- cbind(
  Lower = coef(tab2) - 1.96 * sqrt(diag(vcovHC(tab2, type = "HC0"))),
  Upper = coef(tab2) + 1.96 * sqrt(diag(vcovHC(tab2, type = "HC0")))
)
fgls_ci <- confint(fgls_model)
comparison_df3 <- data.frame(
  RobustOLS_Lower = robust_ols_ci[, 1],
  RobustOLS_Upper = robust_ols_ci[, 2],
  FGLSRobust_Lower = robust_ci_fgls[, "Lower"],
  FGLSRobust_Upper = robust_ci_fgls[, "Upper"]
)

comparison_df3$OLSRobust_Width <- comparison_df3$RobustOLS_Upper - comparison_df3$RobustOLS_Lower
comparison_df3$FGLSRobust_Width <- comparison_df3$FGLSRobust_Upper - comparison_df3$FGLSRobust_Lower
comparison_df3$Change <- ifelse(comparison_df3$FGLSRobust_Width > comparison_df3$OLSRobust_Width, "Wider", "Narrower")

print(comparison_df3)