# C11Q28 Truffle Market: Supply & Demand System

### Environment & Packages
rm(list = ls())
if (!require("pacman")) install.packages("pacman")
pacman::p_load(AER, modelsummary, dplyr, ggplot2, POE5Rdata, tibble)
options(digits = 6)

### Load & Prepare Data
# (Using dataset bundled in POE5Rdata; if you have a CSV, replace here)
truf <- POE5Rdata::truffles %>%
  rename(P = p, Q = q, PS = ps, DI = di, PF = pf)

n_obs <- nrow(truf)

### (a) Rewrite equations & anticipated signs
cat("\n========================== (a) ==========================\n")
cat("Re‑arranged structural equations (price on LHS):\n")
cat("Demand :  P = β0 + β1·Q  + β2·DI  + β3·PS           + u_d\n")
cat("          with β1 < 0  (inverse PxQ);  β2, β3 > 0\n")
cat("Supply :  P = α0 + α1·Q  + α2·PF                    + u_s\n")
cat("          with α1 > 0  (positive PxQ); α2 > 0 (cost ↑ ⇒ price ↑)\n")

### (b) Estimate Demand & Supply (2SLS vs OLS)
cat("\n========================== (b) ==========================\n")
# OLS -------------------------------------------------------
dem_ols <- lm(P ~ Q + DI + PS, data = truf)
sup_ols <- lm(P ~ Q + PF      , data = truf)
# 2SLS ------------------------------------------------------
dem_iv  <- ivreg(P ~ Q + DI + PS | DI + PS + PF, data = truf)
sup_iv  <- ivreg(P ~ Q + PF      | DI + PS + PF, data = truf)

# Show coefficients (estimate & SE) ------------------------
print(summary(dem_iv)$coefficients)
print(summary(sup_iv)$coefficients)

# Sign check ------------------------------------------------
sign_d  <- ifelse(coef(dem_iv)["Q"] < 0 , "✔", "✘")
sign_s  <- ifelse(coef(sup_iv)["Q"] > 0 , "✔", "✘")
cat(sprintf("\nDemand slope β1  = %.4f  (should be <0)  %s", coef(dem_iv)["Q"], sign_d))
cat(sprintf("\nSupply slope α1  = %.4f  (should be >0)  %s\n", coef(sup_iv)["Q"], sign_s))

# Nicely formatted table -----------------------------------
coef_map <- c("(Intercept)"="C","Q"="Q","PS"="PS","DI"="DI","PF"="PF")
msummary(list("Dem 2SLS"=dem_iv, "Dem OLS"=dem_ols,
              "Sup 2SLS"=sup_iv, "Sup OLS"=sup_ols),
         coef_map  = coef_map,
         estimate   = "{estimate}{stars}",
         statistic  = "({std.error})",
         stars      = c("*"=.05,"**"=.01,"***"=.001),
         gof_omit   = "AIC|BIC|Log.Lik|RMSE|SER|Adj|F|Deviance|Sigma",
         add_rows   = tribble(~term,~`Dem 2SLS`,~`Dem OLS`,~`Sup 2SLS`,~`Sup OLS`,
                              "N",n_obs,n_obs,n_obs,n_obs),
         title      = "*OLS vs 2SLS")

### (c) Price Elasticity of Demand at the Means
cat("\n========================== (c) ==========================\n")
P_bar <- mean(truf$P)
Q_bar <- mean(truf$Q)
beta1 <- coef(dem_iv)["Q"]
# Elasticity ε = (1/β1)*(P_bar/Q_bar)
elas  <- (1/beta1)*(P_bar/Q_bar)
cat(sprintf("Mean P = %.3f , Mean Q = %.3f ,  β1 = %.4f\n", P_bar, Q_bar, beta1))
cat(sprintf("Price elasticity of demand at means  ε = %.4f\n", elas))

### (d) Sketch lines & structural equilibrium
cat("\n========================== (d) ==========================\n")
DI_star <- 3.5 ; PS_star <- 22 ; PF_star <- 23
# Demand intercept (constant + DI* + PS*)
B0 <- coef(dem_iv)["(Intercept)"] + coef(dem_iv)["DI"]*DI_star + coef(dem_iv)["PS"]*PS_star
# Supply intercept (constant + PF*)
A0 <- coef(sup_iv)["(Intercept)"] + coef(sup_iv)["PF"]*PF_star
beta1 <- coef(dem_iv)["Q"]; alpha1 <- coef(sup_iv)["Q"]
# Solve equilibrium
Q_eq <- (A0 - B0)/(beta1 - alpha1)
P_eq <- A0 + alpha1*Q_eq
cat(sprintf("Demand:  P = %.4f %+.4f·Q\n", B0, beta1))
cat(sprintf("Supply:  P = %.4f %+.4f·Q\n", A0, alpha1))
cat(sprintf("→ Equilibrium  Q* = %.4f ,  P* = %.4f\n", Q_eq, P_eq))

# Quick plot ------------------------------------------------
plot_df <- data.frame(Q = seq(0, max(truf$Q)*1.2, length = 100)) %>%
  mutate(P_D = B0 + beta1*Q, P_S = A0 + alpha1*Q)

ggplot(plot_df, aes(Q))+
  geom_line(aes(y=P_D, color="Demand"), size=1)+
  geom_line(aes(y=P_S, color="Supply"), size=1)+
  geom_point(aes(x=Q_eq,y=P_eq), size=3)+
  labs(y="Price (P)", color="Curve", title="Truffle Market – Structural Equilibrium")+
  theme_minimal()

### (e) Reduced‑form predicted equilibrium & comparison
cat("\n========================== (e) ==========================\n")
rf_P <- lm(P ~ DI + PS + PF, data = truf)
rf_Q <- lm(Q ~ DI + PS + PF, data = truf)
P_rf <- predict(rf_P, newdata = data.frame(DI=DI_star,PS=PS_star,PF=PF_star))
Q_rf <- predict(rf_Q, newdata = data.frame(DI=DI_star,PS=PS_star,PF=PF_star))
cat(sprintf("Reduced‑form:  Ŷ_P = %.4f  ,  Ŷ_Q = %.4f\n", P_rf, Q_rf))
cat(sprintf("Compare with structural:  ΔP = %.4f ,  ΔQ = %.4f\n", P_rf - P_eq, Q_rf - Q_eq))

### (f) OLS vs 2SLS discussion
cat("\n========================== (f) ==========================\n")
cat("Demand OLS yields β1 = ", round(coef(dem_ols)["Q"],4),
    " (positive, wrong sign) versus 2SLS β1 = ", round(beta1,4),
    " (negative, correct). Supply estimates are similar under both methods.\n")

cat("\nSCRIPT FINISHED. All answers (a)–(f) above.\n")

