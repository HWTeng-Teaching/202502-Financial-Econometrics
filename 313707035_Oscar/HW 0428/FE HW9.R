#10.18
library(POE5Rdata)
library(wooldridge)   # contains the mroz data frame
library(AER)          # ivreg(), coeftest(), linearHypothesis(), etc.
library(dplyr)
library(broom)

data("mroz")                    # 753 obs. of married women
mroz <- mroz %>% 
  filter(inlf == 1)             # keep the 428 working wives

mroz <- mroz %>%
  mutate(
    mothercoll  = as.integer(motheduc  > 12),
    fathercoll  = as.integer(fatheduc  > 12)
  )

#(a) Percentage of parents with some college
pct_mothercoll <- mean(mroz$mothercoll) * 100
pct_fathercoll <- mean(mroz$fathercoll) * 100
c(pct_mothercoll, pct_fathercoll)

#(b) Correlations among educ, mothercoll, and fathercoll
with(mroz, cor(cbind(educ, mothercoll, fathercoll)))

#(c) 2SLS using only mothercoll as the instrument for educ
iv1 <- ivreg(lwage ~ educ + exper + expersq + age + kidslt6 + kidsge6
             | mothercoll + exper + expersq + age + kidslt6 + kidsge6,
             data = mroz)

coeftest(iv1, vcov = vcovHC, type = "HC1")["educ", ]
confint(iv1, level = .95, vcov. = vcovHC, type = "HC1")["educ", ]

#(d) First‑stage regression and the F‑statistic for mothercoll
fs1 <- lm(educ ~ mothercoll + exper + expersq + age + kidslt6 + kidsge6,
          data = mroz)
summary(fs1)$fstatistic                 # overall joint F
linearHypothesis(fs1, "mothercoll = 0") # single‑restriction F

#(e) 2SLS using both mothercoll and fathercoll as instruments
iv2 <- ivreg(lwage ~ educ + exper + expersq + age + kidslt6 + kidsge6
             | mothercoll + fathercoll + exper + expersq + age + kidslt6 + kidsge6,
             data = mroz)

coeftest(iv2, vcov = vcovHC, type = "HC1")["educ", ]
confint(iv2, level = .95, vcov. = vcovHC, type = "HC1")["educ", ]

#(f) First‑stage with two instruments: joint significance of mothercoll, fathercoll
fs2 <- lm(educ ~ mothercoll + fathercoll + exper + expersq + age + kidslt6 + kidsge6,
          data = mroz)

linearHypothesis(fs2, c("mothercoll = 0", "fathercoll = 0"))

#(g) Over‑identification (Sargan) test for validity of the surplus instrument
u_hat <- residuals(iv2)                                 # IV 殘差
Z     <- model.matrix(~ mothercoll + fathercoll + exper + expersq +
                        age + kidslt6 + kidsge6, mroz)[ , -1]

R2    <- summary(lm(u_hat ~ Z - 1))$r.squared           # 沒有截距
Sarg  <- nrow(mroz) * R2                                # n * R^2
df    <- ncol(Z) - 1                                    # L - K (2 instruments - 1 endogenous)

pval  <- 1 - pchisq(Sarg, df)
c(Sargan_stat = Sarg, df = df, p.value = pval)

#10.20
library(POE5Rdata)
data("capm5")                 # 180 monthly obs., 1978m1–1992m12
library(tidyverse)     # dplyr + readr + ggplot2
library(AER)           # ivreg()
library(lmtest)        # sargan(), coeftest()
library(sandwich)      # vcovHC()
library(car)           # linearHypothesis()
head(capm5)
# Create excess returns:
capm5$ex_msft <- capm5$msft - capm5$riskfree
capm5$ex_mkt  <- capm5$mkt  - capm5$riskfree

#––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––
# (a) OLS estimate of the CAPM:  ex_msft ~ ex_mkt
#––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––
model_a <- lm(ex_msft ~ ex_mkt, data=capm5)
summary(model_a)

# → β̂ = slope from summary(model_a)
#    If β̂ > 1, MSFT is “risky” (more volatile than market);
#    if β̂ < 1, MSFT is “relatively safe.”

#––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––
# (b) First‐stage with RANK as IV
#     RANK = rank(ex_mkt) from smallest to largest
#––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––
capm5$RANK <- rank(capm5$ex_mkt)
model_b_fs <- lm(ex_mkt ~ RANK, data=capm5)
summary(model_b_fs)

# → Check the coefficient on RANK (is it very significant?)
#   and the R² (from summary(model_b_fs)$r.squared).
#   If RANK explains much of ex_mkt, it’s (IV1) relevant.

#––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––
# (c) Augmented regression (OLS + residuals)
#     v̂ = residuals from first stage
#––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––
capm5$vhat_b <- residuals(model_b_fs)
model_c <- lm(ex_msft ~ ex_mkt + vhat_b, data=capm5)
summary(model_c)

# → Look at the p‐value on vhat_b.  If vhat_b is significant at 1%,  
#   we reject exogeneity of ex_mkt.

#––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––
# (d) IV (2SLS) using RANK only
#––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––
library(AER)    # for ivreg()
model_d <- ivreg(ex_msft ~ ex_mkt | RANK, data=capm5)
summary(model_d, diagnostics=TRUE)

# → Compare the IV slope to the OLS slope from (a).  Does it move 
#   in the direction you expect?

#––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––
# (e) Add a second instrument POS = I(ex_mkt > 0)
#––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––
capm5$POS <- as.numeric(capm5$ex_mkt > 0)
model_e_fs <- lm(ex_mkt ~ RANK + POS, data=capm5)
summary(model_e_fs)

# → From summary(model_e_fs):
#    • Joint F-stat on (RANK, POS) → instrument strength
#    • R²  → goodness of fit of first stage

#––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––
# (f) Hausman test via augmented regression (with both IVs)
#––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––
capm5$vhat_e <- residuals(model_e_fs)
model_f <- lm(ex_msft ~ ex_mkt + vhat_e, data=capm5)
summary(model_f)

# → Again, look at the p‐value on vhat_e; significant at 1%?
#   If yes, ex_mkt is endogenous.

#––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––
# (g) IV2SLS with both RANK and POS
#––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––
model_g <- ivreg(ex_msft ~ ex_mkt | RANK + POS, data=capm5)
summary(model_g, diagnostics=TRUE)

# → Compare this 2SLS β̂ to the OLS β̂ from (a).

#––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––
# (h) Sargan (overid) test using residuals from (g)
#––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––
# AER’s ivreg summary with diagnostics=TRUE already reports
# the Sargan (J) statistic and p‐value.  Alternatively:
library(lmtest)
sargan <- summary(model_g, diagnostics=TRUE)$diagnostics["Sargan", ]
print(sargan)

# → If the Sargan p‐value > 0.05, you cannot reject that the extra IV is valid.

#––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––
# End of solution
#––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––

#10.24

library(POE5Rdata)   # data package
library(AER)         # for ivreg()
library(lmtest)      # for coeftest()
library(sandwich)    # for robust vcov
library(boot)        # for bootstrap

data("mroz")

# 1) Keep only positive, non‐missing wages:
mroz2 <- subset(mroz, !is.na(wage) & wage > 0)

# 2) Create the log‐wage variable:
mroz2$lwage <- log(mroz2$wage)

# 3) (Optional) Drop any remaining rows with NA in the key vars:
mroz2 <- na.omit(mroz2[, c("lwage","educ","exper","mothereduc","fathereduc")])

# 4) Specify and fit the IV model:
iv_formula <- lwage ~ educ + exper + I(exper^2) |
  mothereduc + fathereduc + exper + I(exper^2)

model_iv <- ivreg(iv_formula, data = mroz2)
summary(model_iv)
#––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––


#––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––
# (a) Compute 2SLS residuals ê_IV and plot vs. EXPER
#––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––
mroz2$e_iv <- residuals(model_iv)
plot(mroz2$exper, mroz2$e_iv,
     xlab="EXPER", ylab=expression(hat(e)[IV]),
     main="IV Residuals vs. EXPER")
abline(h=0, lty=2)


#–– Interpretation:  look for any funnel or systematic pattern around zero.


#––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––
# (b) NR^2 test for heteroskedasticity:
#     regress e_iv^2 on constant + EXPER, then LM = n * R^2
#––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––
aux_b <- lm(I(e_iv^2) ~ exper, data = mroz2)
sum_b <- summary(aux_b)
n      <- nobs(aux_b)
R2_b   <- sum_b$r.squared
LM_stat<- n * R2_b
p_val  <- 1 - pchisq(LM_stat, df = 1)

cat("\n(b) NR^2 test:\n",
    "  n =", n, 
    ",  R^2 =", round(R2_b,4), 
    ",  LM =", round(LM_stat,3),
    ",  p-value =", round(p_val,4), "\n")
# If p-value < 0.05 ⇒ reject homoskedasticity.


#––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––
# (c) IV/2SLS with heteroskedasticity‐robust SEs
#––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––
robust_vcov <- vcovHC(model_iv, type = "HC1")
robust_coefs <- coeftest(model_iv, vcov = robust_vcov)

cat("\n(c) Robust IV estimates:\n")
print(robust_coefs)

# Compare the robust SE on EDUC to the baseline:
baseline_se <- summary(model_iv)$coefficients["educ","Std. Error"]
robust_se   <- sqrt(diag(robust_vcov))["educ"]
cat(sprintf("  EDUC: baseline SE=%.4f, robust SE=%.4f\n",
            baseline_se, robust_se))

# 95% CI for EDUC using robust SE:
ci_rob <- coef(model_iv)["educ"] + c(-1,1)*qnorm(0.975)*robust_se
cat("  95% CI for EDUC (robust) = [",
    round(ci_rob[1],4), ", ", round(ci_rob[2],4), "]\n")


#––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––
# (d) IV/2SLS with bootstrap SEs (B = 200)
#––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––
iv_boot <- function(data, i) {
  d <- data[i, ]
  fit <- ivreg(iv_formula, data = d)
  coef(fit)
}

set.seed(123)
boot_iv <- boot(data = mroz2, statistic = iv_boot, R = 200)

boot_se <- apply(boot_iv$t, 2, sd)
cat("\n(d) Bootstrap SEs (B=200):\n")
print(boot_se)

# Compare bootstrap SE on EDUC to baseline & robust:
cat(sprintf("  EDUC: baseline SE=%.4f, robust SE=%.4f, bootstrap SE=%.4f\n",
            baseline_se, robust_se, boot_se[2]))

# 95% CI for EDUC using bootstrap SE:
ci_boot <- coef(model_iv)["educ"] + c(-1,1)*qnorm(0.975)*boot_se[2]
cat("  95% CI for EDUC (bootstrap) = [",
    round(ci_boot[1],4), ", ", round(ci_boot[2],4), "]\n")
