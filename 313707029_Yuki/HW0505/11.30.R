#── 0. 清空環境與載入套件 ────────────────────────────
rm(list = ls())

library(POE5Rdata)
library(dplyr)
library(AER)
library(lmtest)

data("klein")  # 載入資料

#─────────────────────────────────────────────────────
# (a) OLS 估計投資函數：i ~ p + plag + klag
#─────────────────────────────────────────────────────
model_a <- lm(i ~ p + plag + klag, data = klein)
summary(model_a)

#─────────────────────────────────────────────────────
# (b) 利潤的 reduced-form：用外生+預定變數估 p
#─────────────────────────────────────────────────────
model_b <- lm(p ~ cn + w1 + g + tx + e, data = klein)
summary(model_b)

# 存下殘差與預測值
klein$vhat <- resid(model_b)
klein$phat <- fitted(model_b)

#─────────────────────────────────────────────────────
# (c) Hausman test：是否 p 是內生的？
#─────────────────────────────────────────────────────
model_c <- lm(i ~ p + plag + klag + vhat, data = klein)
summary(model_c)

# 若 vhat 的係數顯著，則 p 為內生變數 ⇒ 需用 2SLS

#─────────────────────────────────────────────────────
# (d) 2SLS 估計投資函數：IV = cn, w1, g, tx, e
#─────────────────────────────────────────────────────
model_d <- ivreg(i ~ p + plag + klag | cn + w1 + g + tx + e + plag + klag, data = klein)
summary(model_d, diagnostics = TRUE)

#─────────────────────────────────────────────────────
# (e) 第二階段手動 2SLS：用 phat 取代 p 再用 OLS
#─────────────────────────────────────────────────────
model_e <- lm(i ~ phat + plag + klag, data = klein)
summary(model_e)

#─────────────────────────────────────────────────────
# (f) Sargan test：檢定工具變數是否有效
#─────────────────────────────────────────────────────
#── 先取得參與 model_e 的資料（含索引與變數） ───────────────────────
mf_e <- model.frame(model_e)             # 這裡包含 phat、plag、klag、i
used_idx <- as.numeric(rownames(mf_e))   # 參與回歸的原始列號

#── 建立 Sargan 回歸所需的資料框 ────────────────────────────────
df_f <- data.frame(
  ehat2sls = resid(model_e),
  cn = klein$cn[used_idx],
  w1 = klein$w1[used_idx],
  g  = klein$g[used_idx],
  tx = klein$tx[used_idx],
  e  = klein$e[used_idx]
)

#── 執行 Sargan test 回歸 ──────────────────────────────────────
model_f <- lm(ehat2sls ~ cn + w1 + g + tx + e, data = df_f)
r2_f <- summary(model_f)$r.squared
n    <- nrow(df_f)

#── 計算 TR^2 與 p 值 ──────────────────────────────────────────
sargan_stat <- r2_f * n
df          <- 5 - 1  # 5 個工具變數，1 個內生變數
pval_sargan <- 1 - pchisq(sargan_stat, df)

#── 輸出結果 ──────────────────────────────────────────────────
cat("Sargan test TR^2 =", round(sargan_stat, 3), 
    "\n自由度 =", df, 
    "\nP 值 =", round(pval_sargan, 4), "\n")
