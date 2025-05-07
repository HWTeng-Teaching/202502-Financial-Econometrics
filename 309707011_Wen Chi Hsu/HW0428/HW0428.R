#10.18
#a
remotes::install_github("ccolonescu/POE5Rdata", force = TRUE)
library(POE5Rdata)
data("mroz")

data <- subset(mroz, lfp == 1)

data$MOTHERCOLL <- ifelse(data$mothereduc > 12, 1, 0)
data$FATHERCOLL <- ifelse(data$fathereduc > 12, 1, 0)

mean(data$MOTHERCOLL) 
mean(data$FATHERCOLL)

#b
cor(data$educ, data$MOTHERCOLL, use = "complete.obs")
cor(data$educ, data$FATHERCOLL, use = "complete.obs")

#c
install.packages("AER")
library("AER")
IV1 <- ivreg(log(wage) ~ educ + exper + I(exper^2) | MOTHERCOLL + exper + I(exper^2), data = data)
summary(IV1)
confint(IV1, "educ", level = 0.95)

#d
install.packages("car")   
library(car)  
first_stage <- lm(educ ~ MOTHERCOLL + exper + I(exper^2), data = data)
summary(first_stage)
Anova(first_stage, type = "II")

#e
IV2 <- ivreg(log(wage) ~ educ + exper + I(exper^2) | MOTHERCOLL + FATHERCOLL + exper + I(exper^2), data = data)
summary(IV2)
confint(IV2, "educ", level = 0.95)

#f
first_stage2 <- lm(educ ~ MOTHERCOLL + FATHERCOLL + exper + I(exper^2), data = data)
summary(first_stage2)
linearHypothesis(first_stage2, c("MOTHERCOLL = 0", "FATHERCOLL = 0"))

#g
summary(IV2, diagnostics = TRUE)

#10.20
#a
remotes::install_github("ccolonescu/POE5Rdata", force = TRUE)
library(POE5Rdata)
library(dplyr)
data("capm5")

msft_rp <- capm5$msft - capm5$riskfree
mkt_rp <- capm5$mkt - capm5$riskfree
combined_data <- data.frame(
  msft_rp = msft_rp,
  mkt_rp = mkt_rp
)

mod_ols <- lm(msft_rp ~ mkt_rp, data = combined_data)
summary(mod_ols)

beta_ols <- coef(mod_ols)[2]
if(beta_ols > 1){
  cat("Microsoft stock is risky ( beta =", round(beta_ols,2), "> 1)\n")
} else if(beta_ols < 1){
  cat("Microsoft stock is relatively safe ( beta =", round(beta_ols,2), "< 1)\n")
} else{
  cat("Microsoft stock is same as market ( beta = 1 )\n")
}

#b
combined_data <- combined_data %>%
  mutate(
    RANK = rank(mkt_rp)
  )

first_stg <- lm(mkt_rp ~ RANK, data = combined_data)
summary(first_stg)

#c
combined_data$v_hat <- residuals(first_stg)
mod_aux <- lm(msft_rp ~ mkt_rp + v_hat, data = combined_data)
summary(mod_aux)

p_value <- coef(summary(mod_aux))["v_hat", "Pr(>|t|)"]
if(p_value < 0.01){
  cat("We reject H0, it means market risk premium is endogeneity.\n")
} else{
  cat("We fail to reject H0, it means market risk premium might be exogeneity.\n")
}

#d
library(AER)
mod_iv <- ivreg(msft_rp ~ mkt_rp | RANK, data = combined_data)
summary(mod_iv, diagnostics = TRUE)

mod_ols2 <- lm(msft_rp ~ mkt_rp, data = combined_data)

results <- data.frame(
  Model = c("OLS", "2SLS"),
  Beta = c(coef(mod_ols2)[2], coef(mod_iv)[2]),
  SE = c(summary(mod_ols2)$coefficients[2,2],
         summary(mod_iv)$coefficients[2,2]),
  R2 = c(summary(mod_ols2)$r.squared,
         summary(mod_iv)$r.squared)
)
print(results)

#e
combined_data <- combined_data %>%
mutate(
  POS = ifelse(mkt_rp > 0, 1, 0)
)

first_stg20 <- lm(mkt_rp ~ RANK + POS, data = combined_data)
summary(first_stg20)
linearHypothesis(first_stg20, c("RANK = 0", "POS = 0"))

#f
hausman_test <- lm(msft_rp ~ mkt_rp + v_hat, data = combined_data)
summary(hausman_test)

#g
mod_iv20 <- ivreg(msft_rp ~ mkt_rp | RANK + POS, data = combined_data)
summary(mod_ols2)
summary(mod_iv20)

#h
iv_resid <- residuals(mod_iv20)
sargan <- lm(iv_resid ~ RANK + POS, data = combined_data)
sargan_stat <- nobs(sargan) * summary(sargan)$r.squared
p_value_sargan <- 1 - pchisq(sargan_stat, df = 1)

sargan_stat
p_value_sargan

#10.24
#a
data("mroz")
install.packages("dplyr")   
library(dplyr)
install.packages("AER")  
library(AER)             
mroz_lfp <- mroz %>% filter(lfp == 1)

mod_iv24 <- ivreg(log(wage) ~ educ + exper + I(exper)^2 | mothereduc + fathereduc + exper + I(exper)^2, data = mroz_lfp)
summary(mod_iv24)
mroz_lfp$e_iv24 <- residuals(mod_iv24)

plot(mroz_lfp$exper, mroz_lfp$e_iv24,
     xlab = "Experience",
     ylab = "Residuals of 2SLS",
     abline(h = 0, col = "red"))

#b
aux_reg <- lm(e_iv24^2 ~ exper, data = mroz_lfp)
nr2_test <- nobs(aux_reg) * summary(aux_reg)$r.squared
p_value <- 1 - pchisq(nr2_test, df = 1)
nr2_test
p_value

#c
library(AER)         
library(sandwich)    
library(lmtest)      
library(dplyr)       

robust_se <- vcovHC(mod_iv24, type = "HC1")

robust_test <- coeftest(mod_iv24, vcov = robust_se)

b_educ <- coef(mod_iv24)["educ"]
se_educ <- sqrt(robust_se["educ", "educ"])
ci <- b_educ + c(-1, 1) * 1.96 * se_educ  # 95% CI = b ± 1.96 * SE

baseline_se <- coef(summary(mod_iv24))[, "Std. Error"]
robust_se_vec <- sqrt(diag(robust_se))

common_vars <- intersect(names(baseline_se), names(robust_se_vec))

se_comparison <- data.frame(
  Estimate = round(coef(mod_iv24)[common_vars], 5),
  Baseline_SE = round(baseline_se[common_vars], 5),
  Robust_SE = round(robust_se_vec[common_vars], 5),
  Increased_SE = ifelse(robust_se_vec[common_vars] > baseline_se[common_vars], "Yes", "No")
)

print(se_comparison)
cat("Conclusion:\nRobust SEs are larger, indicating heteroskedasticity.\n")
cat(sprintf("95%% Robust CI for EDUC: [%.4f, %.4f]\n", ci[1], ci[2]))

#d
library(boot)
iv_formula <- log(wage) ~ educ + exper + I(exper^2) |
  mothereduc + fathereduc + exper + I(exper^2)
model_iv <- ivreg(iv_formula, data = mroz_lfp)
iv_boot <- function(data, i) {
  d <- data[i, ]
  fit <- ivreg(iv_formula, data = d)
  coef(fit)
}

set.seed(123)
boot_iv <- boot(data = mroz_lfp, statistic = iv_boot, R = 200)

boot_se <- apply(boot_iv$t, 2, sd)
cat("\n(d) Bootstrap SEs (B=200):\n")
print(boot_se)

cat(sprintf("  EDUC: baseline SE=%.4f, robust SE=%.4f, bootstrap SE=%.4f\n",
            baseline_se, robust_se, boot_se[2]))

ci_boot <- coef(model_iv)["educ"] + c(-1,1)*qnorm(0.975)*boot_se[2]
cat("  95% CI for EDUC (bootstrap) = [",
    round(ci_boot[1],4), ", ", round(ci_boot[2],4), "]\n")