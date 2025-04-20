# C08Q18 異質變異數檢定與 FGLS
##############################################################################
# 前置設定：清除環境、載入套件與資料
##############################################################################
rm(list = ls())  # 清除工作環境

# 載入必要套件
library(dplyr)
library(ggplot2)
library(car)
library(lmtest)
library(sandwich)
library(POE5Rdata)  # 資料內含 cps5 資料集

# 載入資料
data("cps5")

# 產生 ln_wage 與 exper2 變數，並過濾缺失值
cps5 <- cps5 %>% 
  mutate(
    ln_wage = log(wage),    # 取對數薪資
    exper2  = exper^2       # 經驗平方
  ) %>% 
  filter(
    if_all(
      c("ln_wage","educ","exper","exper2","female","black",
        "metro","south","midwest","west"),
      ~ !is.na(.x) & is.finite(.x)
    )
  )

# -------------------- (1) 擬合基準 OLS 模型 ---------------------------------
formula_full <- ln_wage ~ educ + exper + exper2 + female + black +
  metro + south + midwest + west
model_ols <- lm(formula_full, data = cps5)
summary(model_ols)

# -------------------- (a) Goldfeld–Quandt 檢定 ------------------------------
male_df   <- cps5 %>% filter(female == 0)
female_df <- cps5 %>% filter(female == 1)

SSE_male   <- sum(residuals(lm(formula_full, data = male_df))^2)
SSE_female <- sum(residuals(lm(formula_full, data = female_df))^2)

df_male    <- nrow(male_df)   - length(coef(model_ols))
df_female  <- nrow(female_df) - length(coef(model_ols))

F_gq <- (SSE_male/df_male) / (SSE_female/df_female)
crit_L <- qf(0.025, df_male, df_female)
crit_U <- qf(0.975, df_male, df_female)

cat("(a) Goldfeld–Quandt 檢定結果：\n",
    "  F =", round(F_gq, 4),
    "  臨界區間 = (", round(crit_L,3), ",", round(crit_U,3), ")\n",
    if (F_gq < crit_L | F_gq > crit_U) "→ 拒絕 H0\n" else "→ 無法拒絕 H0\n")

# -------------------- (b) NR² (Breusch–Pagan) 檢定 ---------------------------
u2 <- residuals(model_ols)^2

# (b-1) 候選變數：METRO, FEMALE, BLACK
aux1 <- lm(u2 ~ metro + female + black, data = cps5)
LM1  <- nrow(cps5) * summary(aux1)$r.squared
crit1 <- qchisq(0.99, df = 3)

# (b-2) 使用所有自變數
aux2 <- lm(u2 ~ educ + exper + exper2 + female + black +
             metro + south + midwest + west,
           data = cps5)
LM2   <- nrow(cps5) * summary(aux2)$r.squared
crit2 <- qchisq(0.99, df = 9)

cat("(b) NR² 檢定：\n",
    "  (候選 3 變數) LM =", round(LM1,3), "  臨界值 =", round(crit1,3),
    if (LM1 > crit1) "→ 拒絕 H0\n" else "→ 無法拒絕 H0\n",
    "  (全部變數)   LM =", round(LM2,3), "  臨界值 =", round(crit2,3),
    if (LM2 > crit2) "→ 拒絕 H0\n" else "→ 無法拒絕 H0\n")

# -------------------- (c) White 檢定 ----------------------------------------
# 對 u^2 作輔助回歸：fitted值及平方項
white_aux <- lm(u2 ~ fitted(model_ols) + I(fitted(model_ols)^2), data = cps5)
LM_white  <- nrow(cps5) * summary(white_aux)$r.squared
crit_white <- qchisq(0.95, df = 2)
cat("(c) White 檢定： LM =", round(LM_white,3), "  臨界值 =", round(crit_white,3),
    if (LM_white > crit_white) "→ 拒絕 H0" else "→ 無法拒絕 H0", "\n")

# -------------------- (d) OLS + White 穩健標準誤 ----------------------------
robust_vcov <- vcovHC(model_ols, type = "HC0")
coef_robust <- coeftest(model_ols, vcov = robust_vcov)
print(coef_robust)

ci_ols    <- confint(model_ols)
ci_robust <- confint(model_ols, vcov. = robust_vcov)
width_cmp <- data.frame(
  term   = rownames(ci_ols),
  OLS    = ci_ols[,2] - ci_ols[,1],
  Robust = ci_robust[,2] - ci_robust[,1]
)
print(width_cmp)

# -------------------- (e) FGLS：METRO 與 EXPER 為變異函數 --------------------
sigma_reg <- lm(log(u2) ~ metro + exper, data = cps5)
sigma2_hat <- exp(fitted(sigma_reg))
wts <- 1 / sigma2_hat
model_fgls <- lm(formula_full, data = cps5, weights = wts)
summary(model_fgls)
ci_fgls <- confint(model_fgls)

# -------------------- (f) FGLS + HC0 穩健標準誤 -----------------------------
coef_fgls_rob <- coeftest(model_fgls, vcov = vcovHC(model_fgls, type = "HC0"))
ci_fgls_rob   <- confint(model_fgls, vcov. = vcovHC(model_fgls, type = "HC0"))

ci_compare <- data.frame(
  term        = rownames(ci_ols),
  OLS_Robust  = width_cmp$Robust,
  FGLS        = ci_fgls[,2] - ci_fgls[,1],
  FGLS_Robust = ci_fgls_rob[,2] - ci_fgls_rob[,1]
)
print(ci_compare)

#(g) 建議
cat("建議：在研究報告中，建議報告 OLS 估計值並搭配 White 穩健標準誤，以獲得在存在異質變異數情況下更可靠的推論。\n")

