# ==========================================================
#  CAPM 10.20 – Microsoft 股票　OLS、IV/2SLS、Hausman 與 BP 檢定
#  作者：ChatGPT（繁體中文註解）
#  需要的套件：tidyverse, AER, lmtest, sandwich
# ==========================================================

# ---- 0. 套件與資料 ---------------------------------------------------------
packages <- c("tidyverse", "AER", "lmtest", "sandwich")
lapply(packages, require, character.only = TRUE)

capm  <- read_csv("capm5.csv")      # 與程式同資料夾；或自行修改路徑

# ---- 1. 建立超額報酬與工具變數 --------------------------------------------
capm <- capm %>% 
  mutate(
    msft_rf = msft - riskfree,      # Microsoft 超額報酬
    rm_rf   = mkt  - riskfree,      # 市場超額報酬
    RANK    = rank(rm_rf, ties.method = "first"),
    POS     = if_else(rm_rf > 0, 1, 0)
  )

# ---- 2. (a) OLS 估計 -------------------------------------------------------
ols_fit <- lm(msft_rf ~ rm_rf, data = capm)
summary(ols_fit)

# ---- 3. (b) 第一階段：rm_rf ~ RANK ----------------------------------------
first_rank <- lm(rm_rf ~ RANK, data = capm)
summary(first_rank)
capm <- capm %>% mutate(vhat = resid(first_rank))

# ---- 4. (c) 外生性檢定（加入 vhat） ---------------------------------------
aug_fit <- lm(msft_rf ~ rm_rf + vhat, data = capm)
coeftest(aug_fit)          # vhat 的顯著性
# 若需 1% 顯著水準比較：查看 p-value < 0.01？

# ---- 5. (d) 2SLS：單一工具 RANK -------------------------------------------
iv1_fit <- ivreg(msft_rf ~ rm_rf | RANK, data = capm)
summary(iv1_fit, diagnostics = TRUE)

# ---- 6. (e) 第一階段：rm_rf ~ RANK + POS ----------------------------------
first_rank_pos <- lm(rm_rf ~ RANK + POS, data = capm)
summary(first_rank_pos)
capm <- capm %>% mutate(vhat2 = resid(first_rank_pos))

# ---- 7. (f) Hausman 檢定 ---------------------------------------------------
hausman_test <- lm(msft_rf ~ rm_rf + vhat2, data = capm)
coeftest(hausman_test)     # 檢查 vhat2 在 1% 水準是否顯著

# ---- 8. (g) 2SLS：雙工具 RANK + POS ---------------------------------------
iv2_fit <- ivreg(msft_rf ~ rm_rf | RANK + POS, data = capm)
summary(iv2_fit, diagnostics = TRUE)

# ---- 9. (h) Breusch–Pagan 檢定（2SLS 殘差） -------------------------------
capm <- capm %>% mutate(iv_resid = resid(iv2_fit))

bp_test <- bptest(iv_resid ~ fitted(iv2_fit), data = capm)
bp_test    # 在 5% 顯著水準檢視 p-value
