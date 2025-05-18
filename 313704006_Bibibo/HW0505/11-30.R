library(POE5Rdata)
library(car) 
data("klein")

#a
#It = β1 + β2Pt + β3Pt−1 + β4Kt−1 + e2t (11.18)
klein <- na.omit(klein)
inv_ols <- lm(i ~ p + plag + klag, data = klein)
summary(inv_ols)

#b
reduced_P <- lm(p ~ g + w2 + tx + plag + klag + time + elag + e, data = klein)
linearHypothesis(reduced_P, c(
  "g = 0",
  "w2 = 0",
  "tx = 0",
  "time = 0",
  "elag = 0",
  "e = 0"
))
#p-value 7.29e-08
v_hat <- resid(reduced_P)
P_hat <- fitted(reduced_P)

#c
hmtest = lm(i~p+plag+klag+v_hat, data = klein)
summary(hmtest)
#p-value 0.091673 non-reject H0
#P is not significantly endogenous

#d
library(AER)
iv_2sls <- ivreg(i ~ p + plag + klag | g + w2 + tx + elag + time + plag + klag + e, data = klein)
summary(iv_2sls)

#e
second_stage <- lm(i ~ P_hat + plag + klag, data = klein)
summary(second_stage)

#f
e2_hat <- residuals(second_stage)
sargan_reg <- lm(e2_hat ~ g + w2 + tx + plag + klag + time + elag + e, data = klein)
summary(sargan_reg)
R2 <- summary(sargan_reg)$r.squared
T1 <- nrow(klein)  

Sargan_stat <- T1 * R2
cat("Sargan test statistic: ", Sargan_stat, "\n")

crit_val <- qchisq(0.95, df = 4)
cat("Critical value (chi^2(4), 95%): ", crit_val, "\n")
