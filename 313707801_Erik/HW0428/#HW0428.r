#Question 10.18
setwd("/Users/erikparkhomenko/Library/Mobile Documents/com~apple~CloudDocs/Studies/NYCU/Financial Econometrics/Data/poe5rdata")
load("mroz.rdata")
# Keep only married women in labor force and drop any incomplete rows
mroz <- subset(mroz, lfp == 1)
mroz <- na.omit(mroz)
library(AER)

mroz$MOTHERCOLL <- ifelse(mroz$mothereduc > 12, 1, 0)
mroz$FATHERCOLL <- ifelse(mroz$fathereduc > 12, 1, 0)
pct_mother <- mean(mroz$MOTHERCOLL) * 100
pct_father <- mean(mroz$FATHERCOLL) * 100
cat("Percentage of mothers with some college education:", round(pct_mother, 2), "%\n")
cat("Percentage of fathers with some college education:", round(pct_father, 2), "%\n")

# Part (2): Correlations among EDUC, MOTHERCOLL, and FATHERCOLL
corr_mat <- cor(mroz[, c("educ", "MOTHERCOLL", "FATHERCOLL")], use = "complete.obs")
print(corr_mat)

# Load required packages for IV estimation and hypothesis testing
if (!requireNamespace("AER", quietly = TRUE)) install.packages("AER")
if (!requireNamespace("car", quietly = TRUE)) install.packages("car")

# Part (3): IV estimate using MOTHERCOLL as the instrument for EDUC
iv_model_a <- ivreg(log(wage) ~ educ + exper + I(educ * exper) + I(exper^2) |
                   MOTHERCOLL + exper + I(MOTHERCOLL * exper) + I(exper^2),
                   data = mroz)
summary(iv_model_a, diagnostics = TRUE)
ci_a <- confint(iv_model_a, "educ", level = 0.95)
print(ci_a)

# Part (4): First‐stage regression for EDUC on MOTHERCOLL and exogenous terms
fs1 <- lm(educ ~ MOTHERCOLL + exper + I(exper^2) + I(MOTHERCOLL * exper),
          data = mroz)
summary(fs1)
# F‐test for H0: coefficient on MOTHERCOLL = 0
f_test_mother <- linearHypothesis(fs1, "MOTHERCOLL = 0")
print(f_test_mother)

# Part (5): IV estimate using both MOTHERCOLL and FATHERCOLL as instruments
iv_model_b <- ivreg(log(wage) ~ educ + exper + I(educ * exper) + I(exper^2) |
                   MOTHERCOLL + FATHERCOLL + exper +
                   I(MOTHERCOLL * exper) + I(FATHERCOLL * exper) + I(exper^2),
                   data = mroz)
summary(iv_model_b, diagnostics = TRUE)
ci_b <- confint(iv_model_b, "educ", level = 0.95)
print(ci_b)

# Part (6): First‐stage regression with both instruments and joint significance test
fs2 <- lm(educ ~ MOTHERCOLL + FATHERCOLL + exper +
          I(MOTHERCOLL * exper) + I(FATHERCOLL * exper) + I(exper^2),
          data = mroz)
summary(fs2)
# Joint F‐test for H0: MOTHERCOLL = 0 and FATHERCOLL = 0
f_test_both <- linearHypothesis(fs2, c("MOTHERCOLL = 0", "FATHERCOLL = 0"))
print(f_test_both)

# Part (7): Overidentification (Sargan) test for surplus instrument validity
# (only valid if model is overidentified)
diag_b <- summary(iv_model_b, diagnostics = TRUE)
# Sargan statistic appears in diagnostics; print it
print(diag_b$diagnostics["Sargan", ])

#Question 10.20
setwd("/Users/erikparkhomenko/Library/Mobile Documents/com~apple~CloudDocs/Studies/NYCU/Financial Econometrics/Data/poe5rdata")
# Load the CAPM data

load("capm5.rdata")
# Ensure the loaded data is in 'capm5'
if (!exists("capm5")) {
    df_names <- ls()[sapply(ls(), function(x) is.data.frame(get(x)))]
    if (length(df_names) == 1) {
        capm5 <- get(df_names)
        cat("Assigned data frame", df_names, "to capm5\n")
    } else {
        stop("Multiple or no data frames found; please assign the correct one to capm5.")
    }
}
# Display column names for verification
cat("Column names in capm5:", paste(names(capm5), collapse = ", "), "\n")

# (a) Construct excess returns
capm5$rj_rf <- capm5$msft - capm5$riskfree # Microsoft excess return
capm5$mkt_rf <- capm5$mkt - capm5$riskfree # Market excess return

# (a) OLS estimate of CAPM for Microsoft
ols_msft <- lm(rj_rf ~ mkt_rf, data = capm5)
print(summary(ols_msft))
beta_ols <- coef(ols_msft)["mkt_rf"]
cat("OLS beta:", beta_ols, "\n")

# (b) Create RANK instrument
capm5$RANK <- rank(capm5$mkt_rf)
# First-stage: mkt_rf ~ RANK
fs_rank <- lm(mkt_rf ~ RANK, data = capm5)
print(summary(fs_rank))

# (c) Augmented regression for endogeneity test
capm5$vhat <- residuals(fs_rank)
ols_aug <- lm(rj_rf ~ mkt_rf + vhat, data = capm5)
print(summary(ols_aug))

# (d) IV estimate using RANK
if (!requireNamespace("AER", quietly = TRUE)) install.packages("AER")
library(AER)
iv_rank <- ivreg(rj_rf ~ mkt_rf | RANK, data = capm5)
print(summary(iv_rank))

# (e) Create POS instrument
capm5$POS <- as.integer(capm5$mkt_rf > 0)
# First-stage with RANK & POS
fs_rank_pos <- lm(mkt_rf ~ RANK + POS, data = capm5)
print(summary(fs_rank_pos))

# (f) Hausman endogeneity test using residuals from (e)
capm5$vhat2 <- residuals(fs_rank_pos)
ols_aug2 <- lm(rj_rf ~ mkt_rf + vhat2, data = capm5)
print(summary(ols_aug2))

# (g) IV/2SLS with RANK & POS
iv_rank_pos <- ivreg(rj_rf ~ mkt_rf | RANK + POS, data = capm5)
print(summary(iv_rank_pos))

# (h) Sargan overidentification test
if (!requireNamespace("car", quietly = TRUE)) install.packages("car")
library(car)
sargan_test <- summary(iv_rank_pos, diagnostics = TRUE)$diagnostics["Sargan", ]
print(sargan_test)

#Question 10.24
# Set working directory and load the data
setwd("/Users/erikparkhomenko/Library/Mobile Documents/com~apple~CloudDocs/Studies/NYCU/Financial Econometrics/Data/poe5rdata")
load("mroz.rdata")

# Restrict to married women participating in the labor force
mroz <- subset(mroz, lfp == 1)

# Drop rows with missing values in variables used
mroz <- mroz[complete.cases(mroz[, c("wage", "educ", "exper", "mothereduc", "fathereduc")]), ]

# Ensure log wage variable exists
if (!"lwage" %in% names(mroz)) {
    mroz$lwage <- log(mroz$wage)
}

# Load required packages
library(AER) # for ivreg()
library(sandwich) # for heteroskedasticity-robust vcov
library(lmtest) # for coeftest()
library(boot) # for bootstrap

# (a) Baseline IV/2SLS estimation (Example 10.5)
iv_mod <- ivreg(
    lwage ~ educ + exper + I(exper^2) |
        mothereduc + fathereduc + exper + I(exper^2),
    data = mroz
)
print(summary(iv_mod, diagnostics = TRUE))

# Calculate and plot 2SLS residuals vs. EXPER
res_iv <- residuals(iv_mod)
plot(mroz$exper, res_iv,
    xlab = "Experience",
    ylab = "2SLS Residuals",
    main = "2SLS Residuals vs Experience"
)
abline(h = 0, lty = 2)

# (b) NR² test for heteroskedasticity
res2 <- res_iv^2
aux_mod <- lm(res2 ~ exper, data = mroz)
print(summary(aux_mod))
n <- nrow(mroz)
nr2_stat <- n * summary(aux_mod)$r.squared
p_value <- 1 - pchisq(nr2_stat, df = 1)
cat("NR2 test statistic:", nr2_stat, "p-value:", p_value, "\n")

# (c) IV/2SLS with heteroskedasticity-robust standard errors
robust_vcov <- vcovHC(iv_mod, type = "HC1")
robust_se <- sqrt(diag(robust_vcov))
print(coeftest(iv_mod, vcov = robust_vcov))

# 95% CI for EDUC using robust SE
est_educ <- coef(iv_mod)["educ"]
ci_lower <- est_educ - 1.96 * robust_se["educ"]
ci_upper <- est_educ + 1.96 * robust_se["educ"]
cat("95% CI for EDUC (robust):", ci_lower, ci_upper, "\n")

# (d) IV/2SLS with bootstrap standard errors (B = 200)
boot_fn <- function(data, i) {
    d <- data[i, ]
    mod <- ivreg(
        lwage ~ educ + exper + I(exper^2) |
            mothereduc + fathereduc + exper + I(exper^2),
        data = d
    )
    coef(mod)["educ"]
}
set.seed(123)
boot_res <- boot(data = mroz, statistic = boot_fn, R = 200)
print(boot_res)
print(boot.ci(boot_res, type = "perc"))

# Bootstrap standard error and CIs
boot_se <- sd(boot_res$t)
boot_norm_ci <- c(est_educ - 1.96 * boot_se, est_educ + 1.96 * boot_se)
boot_pct_ci <- boot.ci(boot_res, type = "perc")$percent[4:5]
cat("Bootstrap SE for EDUC:", boot_se, "\n")
cat("95% CI for EDUC (normal):", boot_norm_ci, "\n")
cat("95% CI for EDUC (percentile):", boot_pct_ci, "\n")