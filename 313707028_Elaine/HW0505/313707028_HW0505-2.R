#11.28
#b
install.packages("AER")
library(AER)
data("truffles")
str(truffles)
demand_iv <- ivreg(p ~ q + ps + di | ps + di + pf, data = truffles)
summary(demand_iv)

supply_iv <- ivreg(p ~ q + pf | ps + di + pf, data = truffles)
summary(supply_iv)

#c
coef_demand <- coef(demand_iv)
delta1 <- coef_demand["q"]
mean_Q <- mean(truffles$q, na.rm = TRUE)
mean_P <- mean(truffles$p, na.rm = TRUE)
elasticity_demand <- (1 / delta1) * (mean_P / mean_Q)
cat("Price Elasticity of Demand at the Means:", elasticity_demand, "\n")

#d
Q_vals <- seq(0, 100, by = 1)
DI_star <- 3.5
PS_star <- 22
PF_star <- 23
coef_demand <- coef(demand_iv)
a0 <- coef_demand["(Intercept)"]
a1 <- coef_demand["q"]
a2 <- coef_demand["ps"]
a3 <- coef_demand["di"]
coef_supply <- coef(supply_iv)
b0 <- coef_supply["(Intercept)"]
b1 <- coef_supply["q"]
b2 <- coef_supply["pf"]

P_demand <- a0 + a1 * Q_vals + a2 * PS_star + a3 * DI_star
P_supply <- b0 + b1 * Q_vals + b2 * PF_star
plot(Q_vals, P_demand, type = "l", col = "blue", lwd = 2,
     ylim = range(c(P_demand, P_supply)),
     xlab = "Quantity (Q)", ylab = "Price (P)",
     main = "Supply and Demand for Truffles")

lines(Q_vals, P_supply, col = "red", lwd = 2)

legend("topleft", legend = c("Demand", "Supply"),
       col = c("blue", "red"), lty = 1, lwd = 2)


#e
rhs_demand <- a0 + a2 * PS_star + a3 * DI_star
rhs_supply <- b0 + b2 * PF_star

# 聯立解方程：
# rhs_demand + a1 * Q = rhs_supply + b1 * Q
# → (a1 - b1) * Q = rhs_supply - rhs_demand
eq_Q <- (rhs_supply - rhs_demand) / (a1 - b1)
eq_P <- rhs_demand + a1 * eq_Q  # 代回任一條式子都可以

# 輸出結果
cat("Equilibrium Quantity (Q*):", round(eq_Q, 2), "\n")
cat("Equilibrium Price (P*):", round(eq_P, 2), "\n")
# Reduced-form 模型（這邊是假設你已經估計過了）
# Q ~ PS + DI + PF
# P ~ PS + DI + PF

# Reduced-form for Q
rf_q <- lm(q ~ ps + di + pf, data = truffles)
coef_q <- coef(rf_q)

# Reduced-form for P
rf_p <- lm(p ~ ps + di + pf, data = truffles)
coef_p <- coef(rf_p)

# 計算 predicted Q and P using exogenous values
Q_rf <- coef_q["(Intercept)"] + coef_q["ps"] * PS_star + coef_q["di"] * DI_star + coef_q["pf"] * PF_star
P_rf <- coef_p["(Intercept)"] + coef_p["ps"] * PS_star + coef_p["di"] * DI_star + coef_p["pf"] * PF_star

cat("Reduced-form predicted Q:", round(Q_rf, 2), "\n")
cat("Reduced-form predicted P:", round(P_rf, 2), "\n")

#f
ols_demand <- lm(p ~ q + ps + di, data = truffles)
summary(ols_demand)
ols_supply <- lm(p ~ q + pf, data = truffles)
summary(ols_supply)


#11.30
#a
data("klein")
str(klein)
klein <- na.omit(klein)
investment_ols <- lm(i ~ p + plag + klag, data = klein)
summary(investment_ols)

#b
profit_rf <- lm(p ~ g + w2 + tx + plag + klag + time + elag, data = klein)
summary(profit_rf)

restricted_model <- lm(p ~ plag + klag, data = klein)
anova(restricted_model, profit_rf)
cat('Conclusion:\nThe result (F = 1.93, p = 0.1566) indicates that the additional variables are not jointly statistically significant at the 5% level. Thus, conditional on lagged profit and capital stock, these exogenous variables do not significantly improve the explanatory power of the model.')

klein$vt_hat <- residuals(profit_rf)
klein$pt_hat <- fitted(profit_rf)
klein[, c("p", "pt_hat", "vt_hat")]

#c
hausman_model <- lm(i ~ p + plag + klag + vt_hat, data = klein)
summary(hausman_model)
cat('Conclusion:\nSince the p-value is well below 0.05, we reject the null hypothesis H0:delta=0. This means:The residual vhat_t from the reduced-form equation is statistically significant in the structural equation, providing evidence that current profits P_t are endogenous in the investment equation.This result validates the concern that OLS estimates in the structural equation may be biased, because P_t is correlated with the error term.\nIn the context of this simultaneous equations model, we should expect to find that P_t  is endogenous.')

#d
iv_2sls <- ivreg(i ~ p + plag + klag | g + w2 + tx + plag + klag + time + elag, data = klein)
summary(iv_2sls)
cat('Conclusion:\nThe 2SLS estimation shows that the coefficient on current profits (P_t ) drops from 0.48 (OLS, significant at 1%) to 0.15 (2SLS, not significant), indicating that the OLS estimate was likely biased due to endogeneity. In contrast, the coefficients on lagged profits and lagged capital remain significant and even increase in magnitude under 2SLS. This confirms the result of the Hausman test and supports using 2SLS for consistent estimation in the Klein model.')

#e
klein$pt_hat <- fitted(profit_rf)
second_stage <- lm(i ~ pt_hat + plag + klag, data = klein)
summary(second_stage)
cat('Conclusion:\nThe second-stage OLS regression using the fitted values Phat_t  reproduces the exact same point estimates as the ivreg 2SLS. However, the standard errors are noticeably larger in the manual two-stage OLS , leading to higher p-values. This discrepancy arises because naive second-stage OLS underestimates the sampling variability of Phat_t, whereas ivreg reports the correct 2SLS standard errors.')

#f
klein$e2_hat <- residuals(second_stage)
sargan_reg <- lm(e2_hat ~ g + w2 + tx + plag + klag + time + elag, data = klein)
summary(sargan_reg)
R2 <- summary(sargan_reg)$r.squared
T1 <- nrow(klein)  # number of observations

Sargan_stat <- T1 * R2
cat("Sargan test statistic: ", Sargan_stat, "\n")

# Compare with critical value of chi-square(4) at 95% confidence
crit_val <- qchisq(0.95, df = 4)
cat("Critical value (chi^2(4), 95%): ", crit_val, "\n")

# Conclusion
if (Sargan_stat > crit_val) {
  cat("Reject the null: Instruments may not be valid.\n")
} else {
  cat("Do not reject the null: Instruments appear valid.\n")
}

cat('Conclusion:\nThe Sargan test indicates that the surplus instruments used in the 2SLS estimation of the investment equation are statistically valid. The test statistic (1.28) is well below the 95% critical value from the chi-square distribution with 4 degrees of freedom (9.49). Therefore, we do not reject the null hypothesis that the instruments are uncorrelated with the structural error term, supporting the appropriateness of the chosen instrumental variables.')
