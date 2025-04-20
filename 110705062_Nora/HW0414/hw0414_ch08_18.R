url <- "https://www.principlesofeconometrics.com/poe5/data/rdata/cps5.rdata"
file_path <- "cps5.rdata"
download.file(url, file_path, mode = "wb")
load(file_path)
ls()
head(cps5)

##8.18.1
install.packages("readr")
install.packages("dplyr")
install.packages("car")
library(readr)
library(dplyr)
l# Load necessary library
library(dplyr)

cps5_male <- cps5 %>% filter(female == 0)
cps5_female <- cps5 %>% filter(female == 1)
formula <- log(wage) ~ educ + exper + I(exper^2) + metro
model_male <- lm(formula, data = cps5_male)
model_female <- lm(formula, data = cps5_female)
rss_male <- sum(resid(model_male)^2)
rss_female <- sum(resid(model_female)^2)
df_male <- model_male$df.residual
df_female <- model_female$df.residual
F_stat <- (rss_female / df_female) / (rss_male / df_male)
F_stat

alpha <- 0.05
F_crit <- qf(1 - alpha / 2, df_female, df_male)
F_crit_lower <- 1 / F_crit
F_crit_upper <- F_crit
if (F_stat < F_crit_lower | F_stat > F_crit_upper) {
  cat("Reject the null hypothesis: variance differs between males and females\n")
} else {
  cat("Fail to reject the null hypothesis: no evidence of different variance\n")
}

##8.18.2
model_main <- lm(log(wage) ~ educ + exper + I(exper^2) + female + black + metro +
                   south + midwest + west, data = cps5)

cps5$resid_sq <- resid(model_main)^2
model_aux <- lm(resid_sq ~ metro + female + black, data = cps5)
R2_aux <- summary(model_aux)$r.squared
N <- nrow(cps5)
NR2_stat <- N * R2_aux
NR2_stat
df <- length(coef(model_aux)) - 1  # excluding intercept
critical_value <- qchisq(0.99, df)

if (NR2_stat > critical_value) {
  cat("Reject the null hypothesis: evidence of heteroskedasticity at the 1% level\n")
} else {
  cat("Fail to reject the null hypothesis: no significant heteroskedasticity at the 1% level\n")
}

##8.18.3
library(lmtest)
model_main <- lm(log(wage) ~ educ + exper + I(exper^2) + female + black + metro +
                   south + midwest + west, data = cps5)

white_test <- bptest(model_main, ~ educ + exper + I(exper^2) + female + black + metro +
                       south + midwest + west +
                       I(educ^2) + I(exper^2) + I(female^2) + I(black^2) + I(metro^2) +
                       I(south^2) + I(midwest^2) + I(west^2), data = cps5)

print(white_test)

##8.18.4
library(sandwich)
library(lmtest) 
library(dplyr)

model_main <- lm(log(wage) ~ educ + exper + I(exper^2) + female + black + metro +
                   south + midwest + west, data = cps5)
ols_se <- summary(model_main)$coefficients[, "Std. Error"]
robust_vcov <- vcovHC(model_main, type = "HC1")
robust_se <- sqrt(diag(robust_vcov))
change_pct <- 100 * (robust_se - ols_se) / ols_se

comparison_table <- data.frame(
  變數 = names(ols_se),
  `OLS 標準誤` = round(ols_se, 4),
  `Robust 標準誤` = round(robust_se, 4),
  `改變幅度 (%)` = paste0(ifelse(change_pct >= 0, "+", ""), round(change_pct, 2), "%")
)

print(comparison_table, row.names = FALSE)
ols_ci_female <- confint(model_main)["female", ]
robust_ci_female <- coefci(model_main, vcov. = robust_vcov)["female", ]

cat("\n95% Confidence Interval for 'female':\n")
cat("OLS:     [", round(ols_ci_female[1], 4), ",", round(ols_ci_female[2], 4), "]\n")
cat("Robust:  [", round(robust_ci_female[1], 4), ",", round(robust_ci_female[2], 4), "]\n")

##8.18.5
model_ols <- lm(log(wage) ~ educ + exper + I(exper^2) + female + black + metro +
                  south + midwest + west, data = cps5)
cps5$resid_sq <- resid(model_ols)^2
aux_model <- lm(log(resid_sq) ~ metro + exper, data = cps5)

log_sigma2_hat <- fitted(aux_model)
sigma2_hat <- exp(log_sigma2_hat)
weights <- 1 / sigma2_hat

model_fgls <- lm(log(wage) ~ educ + exper + I(exper^2) + female + black + metro +
                   south + midwest + west, data = cps5, weights = weights)

coefs <- coef(model_fgls)
se <- summary(model_fgls)$coefficients[, "Std. Error"]
ci <- confint(model_fgls)

fgls_table <- data.frame(
  變數 = names(coefs),
  估計值 = round(coefs, 4),
  標準誤 = round(se, 4),
  `95% 信賴區間` = paste0("[", round(ci[, 1], 4), ", ", round(ci[, 2], 4), "]")
)

print(fgls_table, row.names = FALSE)

##8.18.6
fgls_coef <- coef(model_fgls)
fgls_se <- summary(model_fgls)$coefficients[, "Std. Error"]
fgls_vcov_robust <- vcovHC(model_fgls, type = "HC1")
fgls_se_robust <- sqrt(diag(fgls_vcov_robust))

se_change_pct <- 100 * (fgls_se_robust - fgls_se) / fgls_se

fgls_se_table <- data.frame(
  變數 = names(fgls_coef),
  `FGLS 係數` = round(fgls_coef, 4),
  `FGLS SE` = round(fgls_se, 4),
  `Robust SE` = round(fgls_se_robust, 4),
  `SE 變化 (%)` = paste0(ifelse(se_change_pct >= 0, "+", ""), round(se_change_pct, 2), "%")
)

print(fgls_se_table, row.names = FALSE)



























