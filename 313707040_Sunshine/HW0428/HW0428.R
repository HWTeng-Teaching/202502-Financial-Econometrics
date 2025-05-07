#CH10Q18
library(AER)       # 用於 ivreg
library(dplyr)     # 資料處理
library(stargazer) # 美化結果
library(sandwich)  # robust 標準誤
library(lmtest)    # coeftest
library(POE5Rdata)
data("mroz")

# 檢視資料結構
str(mroz)
#(a)
# 建立變數 `mothercoll` 與 `fathercoll`，並計算有大學教育的比例
# 新增變數
# 保留只參與勞動市場的觀察值（與前面一樣）
mroz_lfp <- subset(mroz, lfp == 1) 
mroz_lfp$mothercoll <- ifelse(mroz_lfp$mothereduc > 12, 1, 0)
mroz_lfp$fathercoll <- ifelse(mroz_lfp$fathereduc > 12, 1, 0)
mroz_lfp$parentcoll <- ifelse(mroz_lfp$mothereduc > 12 | mroz_lfp$fathereduc > 12 , 1, 0)
mean(mroz_lfp$parentcoll)

# (b)
subset_data <- mroz_lfp[, c("educ", "mothercoll", "fathercoll")]
cor_matrix <- cor(subset_data)
cor_matrix

# (c)
iv_model <- ivreg(log(wage) ~ exper + I(exper^2) + educ |
                    exper + I(exper^2) + mothercoll,
                  data = subset(mroz_lfp, wage > 0))
summary(iv_model)
confint(iv_model, level = 0.95)["educ", ]

# (d)
first_stage <- lm(educ ~ exper + I(exper^2) + mothercoll, data = mroz_lfp)
summary(first_stage)
anova(first_stage)

# (e)
iv_model_2 <- ivreg(log(wage) ~ exper + I(exper^2) + educ |
                      exper + I(exper^2) + mothercoll + fathercoll,
                    data = subset(mroz_lfp, wage > 0))
summary(iv_model_2)
confint(iv_model_2, level = 0.95)["educ", ]

# (f)
first_stage_2 <- lm(educ ~ exper + I(exper^2) + mothercoll + fathercoll, data = mroz_lfp)
summary(first_stage_2)
linearHypothesis(first_stage_2, c("mothercoll = 0", "fathercoll = 0"))

# (g)
resid_iv <- resid(iv_model_2)
sargan_test <- lm(resid_iv ~ mothercoll + fathercoll, data = mroz_lfp)
summary(sargan_test)
S <- nrow(mroz_lfp) * summary(sargan_test)$r.squared
(p_value <- 1 - pchisq(S, df = 1))

#CH10Q20
data("capm5")
#(a)
y <- capm5$msft - capm5$riskfree 
x <- capm5$mkt - capm5$riskfree

data_capm <- data.frame(y = y, x = x)
model <- lm(y ~ x, data = data_capm)
summary(model)

# (b)
capm5$mkt_excess <- capm5$mkt - capm5$riskfree
capm5$RANK <- rank(capm5$mkt_excess, ties.method = "first")

first_stage <- lm(mkt_excess ~ RANK, data = capm5)
summary(first_stage)

# (c)
capm5$v <- residuals(first_stage)
capm5$y <- capm5$msft - capm5$riskfree
augmented_model <- lm(y ~ mkt_excess + v, data = capm5)
summary(augmented_model)

# (d)
iv_model <- ivreg(y ~ mkt_excess | RANK, data = capm5)
summary(iv_model)

# (e)
capm5$POS <- ifelse(capm5$mkt > capm5$riskfree, 1, 0)
first_stage_2 <- lm(mkt_excess ~ RANK + POS, data = capm5)
summary(first_stage_2)

linearHypothesis(first_stage_2, c("RANK = 0", "POS = 0"))

# (f)
capm5$v_2 <- residuals(first_stage_2)
augmented_model_2 <- lm(y ~ mkt_excess + v_2, data = capm5)
summary(augmented_model_2, diagnostics = TRUE)

# (g)
iv_model <- ivreg(y ~ mkt_excess | RANK + POS, data = capm5)
summary(iv_model)

# (h)
v_hat <- residuals(iv_model)

sargan_aux <- lm(v_hat ~ RANK + POS, data = capm5)
summary(sargan_aux)

n <- nobs(sargan_aux)                   
R2 <- summary(sargan_aux)$r.squared     
J_stat <- n * R2

df <- 2 - 1
p_value <- pchisq(J_stat, df = df, lower.tail = FALSE)

cat("NR-square:", J_stat, "\n")
cat("P-value:", p_value, "\n")

#CH10Q24
library(boot)
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
