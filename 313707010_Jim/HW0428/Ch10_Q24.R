library(AER)
library(sandwich)
library(lmtest)
library(boot)
library(POE5Rdata)

data('mroz')
mroz_lfp <- subset(mroz, lfp == 1)
mroz_lfp$lwage <- log(mroz_lfp$wage)
mroz_lfp$exper2 <- mroz_lfp$exper^2

# (a)
iv_base <- ivreg(log(wage) ~ exper + exper2 + educ | exper + exper2 + mothereduc + fathereduc, data = mroz_lfp)
summary(iv_base)
e_iv <- resid(iv_base)

plot(mroz_lfp$exper, e_iv,
     xlab = "EXPER", ylab = "IV Residuals",
     main = "IV Residuals vs. Experience")
abline(h = 0, col = "red")

# (b)
e_iv2 <- e_iv^2

aux_reg <- lm(e_iv2 ~ mroz_lfp$exper)
summary(aux_reg)

n <- length(e_iv2) 
R2 <- summary(aux_reg)$r.squared
NR2_stat <- n * R2

pchisq(NR2_stat, df = 1, lower.tail = FALSE)
p_value <- pchisq(NR2_stat, df = 1, lower.tail = FALSE) 
cat("NR2 test stastic =", NR2_stat, "\n")
cat("p-value =", p_value, "\n")

# (c)
robust_se <- vcovHC(iv_base, type = "HC1")
coeftest(iv_base, vcov = robust_se)

b_educ <- coef(iv_base)["educ"]
se_educ <- sqrt(robust_se["educ", "educ"])
ci_lower <- b_educ - 1.96 * se_educ
ci_upper <- b_educ + 1.96 * se_educ

baseline_se <- coeftest(iv_base, vcov = vcov(iv_base))[, 2]
robust_se_vec <- sqrt(diag(robust_se))
estimates <- coef(iv_base)
se_comparison <- data.frame(
  Estimate     = round(estimates, 5),
  Baseline_SE  = round(baseline_se, 5),
  Robust_SE    = round(robust_se_vec, 5),
  Increased_SE = ifelse(robust_se_vec > baseline_se, "Yes", "No")
)

print(se_comparison)
cat("95% Robust CI for EDUC: [", round(ci_lower, 4), ",", round(ci_upper, 4), "]\n")

# (d)

boot_iv <- function(data, indices) {
  d <- data[indices, ]  
  model <- ivreg(lwage ~ exper + exper2 + educ |
                   exper + exper2 + mothereduc + fathereduc,
                 data = d)
  return(coef(model))
}

set.seed(10)
boot_result <- boot(data = mroz_lfp, statistic = boot_iv, R = 200)
boot_se <- apply(boot_result$t, 2, sd)

se_compare <- data.frame(
  Coef         = names(baseline_se),
  Baseline_SE  = round(baseline_se, 5),
  Robust_SE    = round(robust_se_vec, 5),
  Bootstrap_SE = round(boot_se, 5),
  Larger_than_Baseline_SE = ifelse(boot_se > baseline_se, "Yes", "No"),
  Larger_than_Robust_SE = ifelse(boot_se > robust_se_vec, "Yes", "No")
)

print(se_compare)
(boot_ci_educ <- boot.ci(boot_result, type = "norm", index = 4))
