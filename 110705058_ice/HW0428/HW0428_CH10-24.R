url <- "https://www.principlesofeconometrics.com/poe5/data/rdata/mroz.rdata"
file_path <- "mroz.rdata"
download.file(url, file_path, mode = "wb")
load(file_path)
ls()
head(mroz)

mroz_lfp <- subset(mroz, lfp == 1)
#a
model_iv = ivreg(log(wage)~educ+exper+I(exper^2)|
                  exper+I(exper^2)+mothereduc+fathereduc, data = mroz_lfp)
vcov_mroz = vcov(model_iv)
residual_iv <- resid(model_iv)
plot(mroz_lfp$exper, residual_iv)

#b
re2 = residual_iv^2
model = lm(re2~exper, data = mroz_lfp)
R2 = summary(model)$r.squared
N = nrow(mroz_lfp)
NR2 = N*R2 #7.4385
p_value = 1 - pchisq(NR2, df = 1)
NR2
p_value

#c
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

ci_base <- coef(model_iv)["educ"] + c(-1, 1) * qnorm(0.975) * baseline_se
cat("  95% CI for EDUC (baseline) = [", round(ci_base[1], 4), ", ", round(ci_base[2], 4), "]\n")

iv_boot <- function(data, i) {
  d <- data[i, ]
  fit <- ivreg(log(wage)~educ+exper+I(exper^2)|
                 exper+I(exper^2)+mothereduc+fathereduc, data = d)
  coef(fit)
}

set.seed(123)
boot_iv <- boot(data = mroz_lfp, statistic = iv_boot, R = 200)

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