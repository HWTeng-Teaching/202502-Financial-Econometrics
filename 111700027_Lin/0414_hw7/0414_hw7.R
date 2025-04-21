# 8.16 
# (a)
install.packages(POE5Rdata)
library(POE5Rdata)
data("vacation")

ols_model = lm(miles ~ income + age + kids, data = vacation)
summary(ols_model)
confint(ols_model, "kids", level = 0.95)

# (b)
resid_vals = resid(ols_model)
plot(vacation$income, resid_vals, main = "Residuals vs Income", xlab = "Income", ylab = "Residuals")
abline(h = 0, col = "red")

plot(vacation$age, resid_vals, main = "Residuals vs Age", xlab = "Age", ylab = "Residuals")
abline(h = 0, col = "blue")

# (c) 
vac_sorted = vacation[order(vacation$income), ]
low_group = vac_sorted[1:90, ]
high_group = vac_sorted[111:200, ]

mod_low = lm(miles ~ income + age + kids, data = low_group)
mod_high = lm(miles ~ income + age + kids, data = high_group)

sse_low = sum(resid(mod_low)^2)
sse_high = sum(resid(mod_high)^2)
df_gq = 90 - 4
f_val = (sse_high / df_gq) / (sse_low / df_gq)
qf(0.95, df_gq, df_gq)

install.packages(lmtest)
library(lmtest)
gq_test_res = gqtest(miles ~ income + age + kids, data = vac_sorted,
                      order.by = ~ income, alternative = "greater", fraction = 0.1)
print(gq_test_res)

# (d)
install.packages(sandwich)
library(sandwich)
robust_errors = sqrt(diag(vcovHC(ols_model, type = "HC1")))
est_kids = coef(ols_model)["kids"]
conf_interval = est_kids + c(-1.96, 1.96) * robust_errors["kids"]
conf_interval

# (e)
gls_wts = 1 / (vacation$income^2)
gls_model = lm(miles ~ income + age + kids, data = vacation, weights = gls_wts)
summary(gls_model)
confint(gls_model, level = 0.95)
gls_se_robust = vcovHC(gls_model, type = "HC1")
coeftest(gls_model, vcov. = gls_se_robust)
coefci(gls_model, vcov. = gls_se_robust, level = 0.95)

# 8.18
# (a)
data("cps5")
male_data = subset(cps5, female == 0)
female_data = subset(cps5, female == 1)

reg_m = lm(log(wage) ~ educ + exper + I(exper^2) + black + metro + south + midwest + west, data = male_data)
reg_f = lm(log(wage) ~ educ + exper + I(exper^2) + black + metro + south + midwest + west, data = female_data)

rss_m = sum(resid(reg_m)^2)
rss_f = sum(resid(reg_f)^2)

gq_val = (rss_m / df.residual(reg_m)) / (rss_f / df.residual(reg_f))
gq_val
qf(c(0.025, 0.975), df1 = df.residual(reg_m), df2 = df.residual(reg_f))

# (b)
mod_combined = lm(log(wage) ~ educ + exper + I(exper^2) + female + black + metro + south + midwest + west, data = cps5)
squared_resids = resid(mod_combined)^2
aux1 = lm(squared_resids ~ metro + female + black, data = cps5)
n_val = nobs(mod_combined)
lm_stat = n_val * summary(aux1)$r.squared
qchisq(0.99, df = length(coef(aux1)) - 1)

aux2 = lm(squared_resids ~ educ + exper + I(exper^2) + female + black + metro + south + midwest + west, data = cps5)
lm_stat_all = n_val * summary(aux2)$r.squared
qchisq(0.99, df = length(coef(aux2)) - 1)

# (c)
bptest(mod_combined, ~ .^2 + I(exper^4), data = cps5)

# (d)
summary(mod_combined)
robust_out = coeftest(mod_combined, vcov = vcovHC(mod_combined, type = "HC0"))

# Confidence interval comparison
ci_ols = confint(mod_combined)
ci_robust = coefci(mod_combined, vcov. = vcovHC(mod_combined, type = "HC0"))

comp_d = data.frame(
  OLS = diff(t(ci_ols)),
  Robust = diff(t(ci_robust)),
  Change = ifelse(diff(t(ci_robust)) > diff(t(ci_ols)), "wider", 
                  ifelse(diff(t(ci_robust)) < diff(t(ci_ols)), "narrower", "same"))
)
print(comp_d)

# Significance test comparison
p_vals_ols <- summary(mod_combined)$coefficients[, "Pr(>|t|)"]
p_vals_robust <- robust_out[, "Pr(>|t|)"]

sig_comp <- data.frame(
  OLS_p = p_vals_ols,
  Robust_p = p_vals_robust,
  OLS_sig = p_vals_ols < 0.05,
  Robust_sig = p_vals_robust < 0.05
)
print(sig_comp)

# (e)
log_u2 = log(resid(mod_combined)^2)
var_model = lm(log_u2 ~ metro + exper, data = cps5)
pred_var = exp(fitted(var_model))
gls_weights = 1 / sqrt(pred_var)

fgls_model = lm(log(wage) ~ educ + exper + I(exper^2) + female + black + metro + south + midwest + west,
                 data = cps5, weights = gls_weights)

summary(fgls_model)
se_fgls = summary(fgls_model)$coefficients[, "Std. Error"]
ci_fgls = confint(fgls_model)

comp_e = data.frame(
  Robust = diff(t(ci_robust)),
  FGLS = diff(t(ci_fgls)),
  Change = ifelse(diff(t(ci_fgls)) < diff(t(ci_robust)), "narrower", 
                  ifelse(diff(t(ci_fgls)) > diff(t(ci_robust)), "wider", "same"))
)
print(comp_e)

# (f)
coeftest(fgls_model, vcov. = vcovHC(fgls_model, type = "HC0"))
ci_fgls_robust = coefci(fgls_model, vcov. = vcovHC(fgls_model, type = "HC0"))

comp_f1 = data.frame(
  Robust = diff(t(ci_robust)),
  FGLS_Robust = diff(t(ci_fgls_robust)),
  Change = ifelse(diff(t(ci_fgls_robust)) < diff(t(ci_robust)), "narrower", 
                  ifelse(diff(t(ci_fgls_robust)) > diff(t(ci_robust)), "wider", "same"))
)
print(comp_f1)

comp_f2 = data.frame(
  FGLS = diff(t(ci_fgls)),
  FGLS_Robust = diff(t(ci_fgls_robust)),
  Change = ifelse(diff(t(ci_fgls)) < diff(t(ci_fgls_robust)), "wider",
                  ifelse(diff(t(ci_fgls)) > diff(t(ci_fgls_robust)), "narrower", "same"))
)
print(comp_f2)
