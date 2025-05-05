#── 清除環境 ─────────────────────────────
rm(list = ls())

#── 載入套件 ─────────────────────────────
library(POE5Rdata)
library(dplyr)
library(AER)
library(car)
library(sandwich)
library(lmtest)
library(boot)

#── 載入資料 ─────────────────────────────
data("mroz")
d <- mroz

#── 濾掉 wage <= 0 並取 log ──────────────
d <- d %>%
  filter(!is.na(wage) & wage > 0) %>%
  mutate(
    lwage = log(wage),
    MOTHERCOLL = as.integer(mothereduc > 12),
    FATHERCOLL = as.integer(fathereduc > 12),
    expersq = exper^2
  )

#── (a) 計算 IV/2SLS 殘差並畫圖 ──────────────
iv_model <- ivreg(lwage ~ educ + exper + expersq | mothereduc + fathereduc + exper + expersq, data = d)
d$residuals_iv <- residuals(iv_model)

# 畫圖：殘差 vs exper
plot(d$exper, d$residuals_iv, xlab = "Exper", ylab = "IV Residuals", main = "(a) Residuals vs Exper")
abline(h = 0, col = "red")

#── (b) 殘差平方回歸並做 NR² 檢定 ──────────────
d$residuals_iv_sq <- d$residuals_iv^2
lm_b <- lm(residuals_iv_sq ~ exper, data = d)
summary_b <- summary(lm_b)
n <- nrow(d)
R2_b <- summary_b$r.squared
NR2 <- n * R2_b

# 檢查 NR² 的卡方臨界值 (自由度 1)
p_value <- 1 - pchisq(NR2, df = 1)
cat("(b) NR² =", NR2, ", p-value =", p_value, "\n")

#── (c) Heteroskedasticity Robust Standard Errors ──────────────
coeftest_c <- coeftest(iv_model, vcov = vcovHC(iv_model, type = "HC1"))
cat("(c) Robust standard errors:\n")
print(coeftest_c)

# 95% 信賴區間
se_educ_c <- sqrt(diag(vcovHC(iv_model, type = "HC1"))["educ"])
educ_coef <- coef(iv_model)["educ"]
confint_c <- c(educ_coef - 1.96 * se_educ_c, educ_coef + 1.96 * se_educ_c)
cat("(c) 95% CI for educ (robust SE):", confint_c, "\n")

#── (d) Bootstrap standard errors ──────────────
boot_iv <- function(data, indices) {
  d_boot <- data[indices, ]
  fit <- ivreg(lwage ~ educ + exper + expersq | mothereduc + fathereduc + exper + expersq, data = d_boot)
  return(coef(fit)["educ"])
}

set.seed(123)
boot_results <- boot(d, statistic = boot_iv, R = 200)
se_boot <- sd(boot_results$t)
educ_coef_boot <- coef(iv_model)["educ"]
confint_d <- c(educ_coef_boot - 1.96 * se_boot, educ_coef_boot + 1.96 * se_boot)
cat("(d) Bootstrap SE:", se_boot, "\n")
cat("(d) 95% CI for educ (bootstrap SE):", confint_d, "\n")

