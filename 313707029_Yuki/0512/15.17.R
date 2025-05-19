#── 0. 清空環境 ───────────────────────────────
rm(list = ls())

#── 1. 載入所需套件 ───────────────────────────
# 如尚未安裝，先執行：install.packages(c("POE5Rdata","dplyr","plm","lmtest"))
library(POE5Rdata)
library(dplyr)
library(plm)
library(lmtest)

#── 2. 讀入 liquor5 資料並檢查欄位 ────────────
data("liquor5")
liquor <- liquor5
# → 應包含：hh, year, income, liquor（全為小寫）

#── 3. (a) 一階差分 + 無截距 OLS ──────────────
# 先按 hh, year 排序，確保 lag() 正確
liquor <- liquor %>% arrange(hh, year)

# 計算差分變數 liquord, incomed
liquor_diff <- liquor %>%
  group_by(hh) %>%
  mutate(
    liquord = liquor - dplyr::lag(liquor),
    incomed = income - dplyr::lag(income)
  ) %>%
  filter(!is.na(liquord) & !is.na(incomed)) %>%  # 去掉第一年 NA
  ungroup()

# 檢視差分後前幾筆
head(liquor_diff)

# 無截距 OLS 回歸
ols_diff <- lm(liquord ~ 0 + incomed, data = liquor_diff)
summary(ols_diff)            # 估計結果
confint(ols_diff, level = 0.95)  # 95% 信賴區間

#── 4. (b) 隨機效果模型估計 + 群聚 robust SE ─────
pdata <- pdata.frame(liquor, index = c("hh","year"))
re_model <- plm(liquor ~ income, data = pdata, model = "random")
summary(re_model)

# cluster‐robust SE
rob_se <- sqrt(diag(vcovHC(re_model, method="arellano", type="HC1")))
coeftest(re_model, vcov=vcovHC(re_model, method="arellano", type="HC1"))

# 手動算 95% CI for income
est <- coef(re_model)["income"]
se  <- rob_se["income"]
ci_re <- est + c(-1.96,1.96) * se
cat("RE 95% CI for income: [", round(ci_re[1],4), ", ", round(ci_re[2],4), "]\n", sep="")

#── 5. (c) Breusch–Pagan LM 檢定 ────────────
plmtest(re_model, type="bp")

#── 6. (d) Mundlak 檢定 ───────────────────
liquor2 <- liquor %>%
  group_by(hh) %>%
  mutate(incomem = mean(income, na.rm = TRUE)) %>%
  ungroup()

#── 建 panel frame ───────────────────────────
pdata2 <- pdata.frame(liquor2, index = c("hh", "year"))

#── 隨機效果模型（Mundlak 規格）──────────────
re_ext <- plm(liquor ~ income + incomem,
              data  = pdata2,
              model = "random")

#── 檢視估計結果 ───────────────────────────
summary(re_ext)

#── 以 cluster‐robust SE 檢定 γ（incomem）顯著性 ────
coeftest(re_ext,
         vcov = vcovHC(re_ext,
                       method = "arellano",
                       type   = "HC1"))

