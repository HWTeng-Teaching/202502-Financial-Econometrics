#── (0) 清除環境變數（可選）───────────────────────────────────────────
rm(list = ls())

#── (1) 載入套件 ─────────────────────────────────────────────────────
# install.packages(c("wooldridge","dplyr","AER","sandwich","lmtest","car"))
library(POE5Rdata)
library(dplyr)        # 資料處理
library(AER)          # ivreg()
library(sandwich)     # vcovHC()
library(lmtest)       # coeftest()
library(car)          # linearHypothesis()

#── (2) 載入資料並預處理 ─────────────────────────────────────────────
data("capm5")         
# 檢查欄位名稱，如果需要：
# names(capm5)

capm5 <- capm5 %>%
  mutate(
    y    = msft    - riskfree,   # Microsoft 超額報酬
    x    = mkt     - riskfree,   # 市場超額報酬
    RANK = rank(x),               # 工具變數1：排名
    POS  = as.integer(x > 0)      # 工具變數2：市場報酬正負指標
  )

#── (3) 各小題的迴歸方程式及擬合 ────────────────────────────────────

# (a) OLS 估計 CAPM 模型
ols_a <- lm(y ~ x, data = capm5)

# (b) 第一階段：用 RANK 解釋市場超額報酬 x
fs_b  <- lm(x ~ RANK, data = capm5)

# (c) Augmented regression：加入第一階段殘差 v_hat
v_hat <- resid(fs_b)
aug_c <- lm(y ~ x + v_hat, data = capm5)

# (d) IV/2SLS：以 RANK 作為唯一工具
iv_d  <- ivreg(y ~ x | RANK, data = capm5)

# (e) 第一階段：用 RANK + POS 解釋 x
fs_e  <- lm(x ~ RANK + POS, data = capm5)

# (f) Hausman 測試：加入第二階段殘差 v_hat2
v_hat2 <- resid(fs_e)
haus_f <- lm(y ~ x + v_hat2, data = capm5)

# (g) IV/2SLS：以 RANK & POS 作為工具
iv_g  <- ivreg(y ~ x | RANK + POS, data = capm5)

# (h) Sargan 過度識別檢定
sargan_test <- summary(iv_g, diagnostics = TRUE)$diagnostics["Sargan", ]

#── (4) 檢視結果 ─────────────────────────────────────────────────────
summary(ols_a)
summary(fs_b)
summary(aug_c)
summary(iv_d, diagnostics = TRUE)
summary(fs_e)
summary(haus_f)
summary(iv_g, diagnostics = TRUE)
sargan_test

