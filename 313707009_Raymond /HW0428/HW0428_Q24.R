if (!requireNamespace("remotes", quietly = TRUE)) install.packages("remotes")
remotes::install_github("ccolonescu/POE5Rdata", force = TRUE)
install.packages("AER")
install.packages("car")
install.packages("lmtest")
install.packages("sandwich")
install.packages("boot")
library(POE5Rdata)
data('mroz')
#(a) IV/2SLS的殘差與EXPER趨勢圖
library(AER)
iv_model <- ivreg(log(wage) ~ exper + I(exper^2) + educ |
                    exper + I(exper^2) + mothereduc + fathereduc,
                  data = subset(mroz, wage > 0))
eIV <- residuals(iv_model)
mroz_filtered <- subset(mroz, wage > 0)
exper <- mroz_filtered$exper
plot(exper, eIV,
     xlab = "EXPER (工作年數)",
     ylab = "IV/2SLS 殘差 (eIV)",
     main = "eIV 對 EXPER 的散佈圖")

# 加上平滑線檢查趨勢（例如用 lowess）
lines(lowess(exper, eIV), col = "blue", lwd = 2)
# (b) NR2 Test
eIV <- residuals(iv_model)
eIV2 <- eIV^2
aux_model <- lm(eIV2 ~ exper)

n <- length(eIV2)  
R2 <- summary(aux_model)$r.squared
NR2 <- n * R2

p_value <- pchisq(NR2, df = 1, lower.tail = FALSE) #僅EXPER一個變數
cat("NR² 檢定統計量 =", NR2, "\n")
cat("p-value =", p_value, "\n")
# (c)與Robust SE比較
library(sandwich)
library(lmtest)
robust_se <- vcovHC(iv_model, type = "HC1")  # HC1 是常見的 heteroskedasticity-robust 方法

coeftest(iv_model, vcov. = robust_se)
summary(iv_model)

educ_coef <- coef(iv_model)["educ"]
educ_se_robust <- sqrt(robust_se["educ", "educ"])

lower <- educ_coef - 1.96 * educ_se_robust
upper <- educ_coef + 1.96 * educ_se_robust

cat("EDUC 的 95% 信賴區間為： [", lower, ",", upper, "]\n")
# (d) Bootstrap SE
library(boot)
iv_formula <- log(wage) ~ exper + I(exper^2) + educ |
  mothereduc + fathereduc + exper + I(exper^2)
model_iv <- ivreg(iv_formula, data = mroz_filtered)
iv_boot <- function(data, i) {
  d <- data[i, ]
  fit <- ivreg(iv_formula, data = d)
  coef(fit)
}

set.seed(123)
boot_iv <- boot(data = mroz_filtered, statistic = iv_boot, R = 200)

boot_se <- apply(boot_iv$t, 2, sd)
cat("\n(d) Bootstrap SEs (B=200):\n")
print(boot_se)


# 95% CI for EDUC using bootstrap SE:
ci_boot <- coef(model_iv)["educ"] + c(-1,1)*qnorm(0.975)*boot_se[2]
cat("  95% CI for EDUC (bootstrap) = [",
    round(ci_boot[1],4), ", ", round(ci_boot[2],4), "]\n")
