#8.16
library(POE5Rdata)
data("vacation")

install.packages("lmtest") 
install.packages("sandwich")
install.packages("car")

library(lmtest)
library(sandwich)
library(car) 

##(a) OLS estimation and 95% CI for β₄ (effect of one more child)
# fit OLS
ols <- lm(miles ~ income + age + kids, data=vacation)
summary(ols)

# 95% conf. interval for β4:
confint(ols, "kids", level = 0.95)

##(b) Plot residuals vs INCOME and AGE
resid_ols <- resid(ols)

# Residuals vs income
# 方法 A：在 plot 裡直接用 data frame$
plot(vacation$income, resid_ols,
     xlab="Income ($1000s)", ylab="OLS residuals",
     main="Residuals vs Income")
abline(h=0, lty=2)


# Residuals vs age
plot(vacation$age, resid_ols,
     xlab="Age (years)", ylab="OLS residuals",
     main="Residuals vs Age")
abline(h=0, lty=2)

##(c) Goldfeld–Quandt test for heteroskedasticity
# Goldfeld–Quandt via car::gqtest
gqtest(ols,
       order.by = vacation$income,
       fraction = 0.1,        # drops 20 obs (10% top & bottom)
       alternative = "greater")
##(e)OLS with heteroskedasticity‐robust SEs
# robust (White) covariance
vcov_white <- sandwich(ols)
coeftest(ols, vcov_white)

# 95% CI for β4 with robust SE:
b4_hat <- coef(ols)["kids"]
se4_rob <- sqrt(vcov_white["kids","kids"])
CI_rob <- b4_hat + c(-1,1)*qnorm(0.975)*se4_rob
CI_rob

##(e) GLS
# WLS / “GLS”
gls_fit <- lm(miles ~ income + age + kids,
              data = vacation,
              weights = 1/income^2)

summary(gls_fit)

# (e1) conventional 95% CI for β4:
confint(gls_fit, "kids", level=0.95)

# (e2) robust‐sandwich SEs on the WLS fit
vcov_gls_white <- sandwich(gls_fit)
coeftest(gls_fit, vcov_gls_white)

# robust 95% CI for β4 under GLS:
b4_g <- coef(gls_fit)["kids"]
se4_grob <- sqrt(vcov_gls_white["kids","kids"])
CI_gls_rob <- b4_g + c(-1,1)*qnorm(0.975)*se4_grob
CI_gls_rob

#8.18
#(a) Goldfeld–Quandt test for σ²ₘ = σ²𝐹
# packages
library(lmtest)

# 1. Fit separate regressions for males and females (holding educ, exper, exper^2, metro constant)
mod_m <- lm(log(wage) ~ educ + exper + I(exper^2) + metro,
            data = cps5, subset = (female==0))
mod_f <- lm(log(wage) ~ educ + exper + I(exper^2) + metro,
            data = cps5, subset = (female==1))

# 2. Compute SSRs and do F = (SSR_f/(n_f–k)) / (SSR_m/(n_m–k))
SSR_m <- sum(resid(mod_m)^2); n_m <- length(resid(mod_m))
SSR_f <- sum(resid(mod_f)^2); n_f <- length(resid(mod_f))
k    <- length(coef(mod_m))   # number of parameters

F_stat <- (SSR_f/(n_f - k)) / (SSR_m/(n_m - k))
df1     <- n_f - k
df2     <- n_m - k
p_val   <- 2 * pf(F_stat, df1, df2, lower.tail = FALSE)

# 3. Output
F_stat; df1; df2; p_val
# critical region: reject H0 if F_stat > qf(0.975, df1, df2)

#(b) NR² (Breusch–Pagan) test
library(lmtest)

# 1. Full model
base <- lm(log(wage) ~ educ + exper + I(exper^2)
           + female + black + metro
           + south  + midwest + west,
           data = cps5)

# 2. NR² test ≃ Breusch–Pagan
#    (i) metro, female, black
bptest(base,
       varformula = ~ metro + female + black,
       data = cps5)

#    (ii) all explanatory vars
bptest(base,
       varformula = ~ educ + exper + I(exper^2)
       + female + black + metro
       + south  + midwest + west,
       data = cps5)

#(c) White test
# (i) Using the built‐in “white” option in bptest()
bptest(base,
       varformula = ~ .^2,      # “.^2” = all terms + all pairwise interactions + squares
       data = cps5)

# (ii) Or manually:
aux <- lm(resid(base)^2 ~ educ + exper + I(exper^2)
          + female + black + metro
          + south  + midwest + west
          + I(educ^2) + I(exper^2)
          + educ:exper,
          data = cps5)
LM_stat     <- nrow(cps5) * summary(aux)$r.squared
df_white    <- length(coef(aux)) - 1
p_val_white <- 1 - pchisq(LM_stat, df_white)

# Critical value at 5%:
crit_white <- qchisq(0.95, df_white)

# Output
LM_stat; df_white; p_val_white; crit_white

#(d) OLS with White‐robust SEs
library(sandwich)
library(lmtest)

# Conventional OLS CIs
conf_int_ols <- confint(base)

# Robust SEs (HC0)
robust_cov  <- vcovHC(base, type = "HC0")
robust_tst  <- coeftest(base, robust_cov)
conf_int_hc <- confint(robust_tst)

list(OLS_CI = conf_int_ols,
     Robust_CI = conf_int_hc)

#(e) FGLS using METRO & EXPER
# 1. Estimate variance function
vmod <- lm(resid(base)^2 ~ metro + exper, data = cps5)
sigma2_hat <- predict(vmod)

# 2. FGLS (WLS) fit
gls1 <- lm(log(wage) ~ educ + exper + I(exper^2)
           + female + black + metro
           + south  + midwest + west,
           weights = 1/sigma2_hat,
           data = cps5)

summary(gls1)

# 3. 95% CIs
confint(gls1)

#(f) FGLS + robust SEs
# Robust SEs on gls1
robust_cov_gls1 <- vcovHC(gls1, type = "HC0")
coeftest(gls1, robust_cov_gls1)
confint(coeftest(gls1, robust_cov_gls1))










