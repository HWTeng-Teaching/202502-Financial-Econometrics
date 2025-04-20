
# 套件載入 -------------------------------
library(dplyr)
library(lmtest)
library(car)
library(sandwich)

# 資料載入 -------------------------------
cps5 <- read.csv("https://www.principlesofeconometrics.com/poe5/data/rdata/cps5.rdata")

# OLS 模型 -------------------------------
ols_model <- lm(log(WAGE) ~ EDUC + EXPER + I(EXPER^2) + FEMALE + BLACK +
                  METRO + SOUTH + MIDWEST + WEST, data = cps5)

# a. Goldfeld–Quandt test -----------------
gq_result <- gqtest(ols_model, order.by = ~ FEMALE, data = cps5, fraction = 0.2)
print(gq_result)
# 解釋：檢定殘差變異在 FEMALE=0 與 FEMALE=1 是否相等。若 p < 0.05，則拒絕 H0，有異質變異。

# b. NR^2 Test ----------------------------
# 以 METRO, FEMALE, BLACK 為解釋變數檢定異質變異
res_sq <- residuals(ols_model)^2
nr2_model <- lm(res_sq ~ METRO + FEMALE + BLACK, data = cps5)
nr2 <- summary(nr2_model)$r.squared
n <- nrow(cps5)
LM_stat <- n * nr2
pval_b1 <- 1 - pchisq(LM_stat, df = 3)
print(paste0("NR^2 Test Stat = ", round(LM_stat, 4), ", p-value = ", round(pval_b1, 4)))

# 全部解釋變數做 NR^2 test
nr2_full_model <- lm(res_sq ~ EDUC + EXPER + I(EXPER^2) + FEMALE + BLACK +
                       METRO + SOUTH + MIDWEST + WEST, data = cps5)
nr2_full <- summary(nr2_full_model)$r.squared
LM_stat_full <- n * nr2_full
pval_b2 <- 1 - pchisq(LM_stat_full, df = 9)
print(paste0("Full NR^2 Test Stat = ", round(LM_stat_full, 4), ", p-value = ", round(pval_b2, 4)))

# c. White Test ---------------------------
white_test <- bptest(ols_model, ~ fitted(ols_model) + I(fitted(ols_model)^2))
print(white_test)
# 解釋：White test 的臨界值為 χ²(2) 的 5% 臨界值，若 p < 0.05，表示存在異質變異。

# d. OLS with Robust SE -------------------
coeftest_robust <- coeftest(ols_model, vcov = vcovHC(ols_model, type = "HC1"))
print(coeftest_robust)

# e. FGLS using METRO and EXPER ----------
# Step 1: 用 METRO 和 EXPER 擬合殘差平方
res_sq <- residuals(ols_model)^2
log_res_model <- lm(log(res_sq) ~ METRO + EXPER, data = cps5)
pred_log_var <- predict(log_res_model)
weights <- 1 / exp(pred_log_var)

# Step 2: 加權最小平方法 (WLS)
fgls_model <- lm(log(WAGE) ~ EDUC + EXPER + I(EXPER^2) + FEMALE + BLACK +
                   METRO + SOUTH + MIDWEST + WEST, data = cps5, weights = weights)
summary(fgls_model)

# f. FGLS with robust SE ------------------
coeftest_fgls_robust <- coeftest(fgls_model, vcov = vcovHC(fgls_model, type = "HC1"))
print(coeftest_fgls_robust)

# g. 報告建議 -----------------------------
cat("\n建議：\n")
cat("如果模型存在顯著的異質變異數（如 a~c 顯示），建議使用 FGLS 或 OLS with robust SE。\n")
cat("FGLS 更有效率（smaller standard errors），但其估計需要對變異結構做出假設。\n")
cat("若報告需強調穩健性，可優先選擇 OLS with robust SE。\n")
