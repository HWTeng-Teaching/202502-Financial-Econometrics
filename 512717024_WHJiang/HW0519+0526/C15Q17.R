############################################################################
# POE5  Chapter 15 ‧ Exercise 15.17
# 目標：(a) FD-OLS  (b) Random-Effects  (c) LM test  (d) Mundlak test
############################################################################
options(digits = 8, width = 80)

## ── 0. 套件 ───────────────────────────────────────────────────────────────
pkgs <- c("tidyverse", "plm", "lmtest", "sandwich", "car")
to_add <- setdiff(pkgs, rownames(installed.packages()))
if(length(to_add)) install.packages(to_add, quiet = TRUE)
suppressPackageStartupMessages(lapply(pkgs, library, character.only = TRUE))

## ── 1. 讀檔並整理成 120×4 的資料框 df ─────────────────────────────────────
url_liq <- "https://www.principlesofeconometrics.com/poe5/data/rdata/liquor5.rdata"
load(url(url_liq))                # 會載入 list 物件 liquor5，含 4 個向量

df <- as_tibble(liquor5) |>
  setNames(c("ID","YEAR","LIQUOR","INCOME")) |>  # 直接改欄名
  mutate(across(everything(), as.numeric)) |>    # 向量原為 character
  arrange(ID, YEAR)

## 檢查
stopifnot(nrow(df)==120, setequal(names(df), c("ID","YEAR","LIQUOR","INCOME")))

## 轉成 plm panel 物件（個體 = ID，時間 = YEAR）
pdata <- pdata.frame(df, index = c("ID","YEAR"))

## ── 2. (a) 一階差分 OLS（無截距）──────────────────────────────────────────
fd <- df |>
  group_by(ID) |>
  mutate(
    LIQUORD = LIQUOR - lag(LIQUOR),
    INCOMED = INCOME - lag(INCOME)
  ) |>
  ungroup() |>
  drop_na()                          # 去掉每戶第一年 (lag 為 NA)

fd_ols <- lm(LIQUORD ~ 0 + INCOMED, data = fd)  # 無截距
ci_a   <- confint(fd_ols)["INCOMED", ]

cat("\n(a) 1st-difference OLS（無截距）\n")
print(summary(fd_ols))
cat("95% CI for β2 :", round(ci_a, 6), "\n")

## ── 3. (b) 隨機效果 (Swamy–Arora) ────────────────────────────────────────
re <- plm(LIQUOR ~ INCOME, data = pdata,
          model = "random", effect = "individual",
          random.method = "swar")

se_re <- sqrt(diag(vcovHC(re, type = "HC0")))   # White (robust) SE
ci_b  <- coef(re)["INCOME"] + qnorm(c(.025,.975))*se_re["INCOME"]

cat("\n(b) Random-Effects (Swamy-Arora)\n")
print(summary(re))
cat("95% CI for β2 :", round(ci_b, 6), "\n")

## ── 4. (c) LM 檢定 (Breusch–Pagan) ────────────────────────────────────────
cat("\n(c) Breusch–Pagan LM test for RE\n")
print(plmtest(re, type = "bp"))      # 與課本公式 (15.35) 結果一致

## ── 5. (d) Mundlak 檢定 ──────────────────────────────────────────────────
pdata_m <- pdata |>
  group_by(ID) |>
  mutate(INCOMEM = mean(INCOME, na.rm = TRUE)) |>
  ungroup() |>
  pdata.frame(index = c("ID","YEAR"))

mund <- plm(LIQUOR ~ INCOME + INCOMEM, data = pdata_m,
            model = "random", effect = "individual",
            random.method = "swar")

cat("\n(d) Mundlak RE（含 INCOMEM）\n")
print(summary(mund))

cat("\nH0 : γ = 0  (INCOMEM 無效 → u_i 與 INCOME 不相關)\n")
print(car::linearHypothesis(mund, "INCOMEM = 0", test = "F"))

