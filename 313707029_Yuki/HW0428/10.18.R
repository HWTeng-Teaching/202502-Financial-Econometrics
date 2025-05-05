#── 清除環境 ─────────────────────────────
rm(list = ls())

#── 載入套件 ─────────────────────────────
library(POE5Rdata)
library(dplyr)
library(AER)
library(car)

#── 載入資料 ─────────────────────────────
data("mroz")
# 用全部資料，因為沒有 inlf 變數
d <- mroz

#── (a) 建立母親與父親大學 dummy 變數 ─────────────────────────────
d <- d %>%
  mutate(
    MOTHERCOLL = as.integer(mothereduc > 12),
    FATHERCOLL = as.integer(fathereduc > 12),
    expersq = exper^2
  ) %>%
  filter(!is.na(wage) & wage > 0) %>%  # 濾掉 wage 為 NA 或 <=0 的觀測
  mutate(lwage = log(wage))            # 取 log(wage)

# 計算百分比
perc_mothercoll <- mean(d$MOTHERCOLL, na.rm = TRUE) * 100
perc_fathercoll <- mean(d$FATHERCOLL, na.rm = TRUE) * 100

cat("(a) 母親有大學教育比例:", round(perc_mothercoll, 2), "%\n")
cat("(a) 父親有大學教育比例:", round(perc_fathercoll, 2), "%\n")

#── (b) 計算變數之間的相關係數 ─────────────────────────────
cor_matrix <- cor(d %>% select(educ, MOTHERCOLL, FATHERCOLL), use = "complete.obs")
cat("(b) 相關矩陣:\n")
print(cor_matrix)

#── (c) 使用 MOTHERCOLL 作為 EDU 的工具變數估計工資方程 ──────────────
iv1 <- ivreg(lwage ~ educ + exper + expersq | MOTHERCOLL + exper + expersq, data = d)
summary(iv1)

# 95%信賴區間
cat("(c) 教育係數 95% 信賴區間:\n")
print(confint(iv1, 'educ', level = 0.95))

#── (c) 使用 MOTHERCOLL 作為 EDU 的工具變數估計工資方程 ──────────────
iv1 <- ivreg(lwage ~ educ + exper + expersq | MOTHERCOLL + exper + expersq, data = d)
summary(iv1)

# 95%信賴區間
cat("(c) 教育係數 95% 信賴區間:\n")
print(confint(iv1, 'educ', level = 0.95))

#── (d) 第一階段回歸，檢查 MOTHERCOLL 對 EDU 的影響 ──────────────
first_stage1 <- lm(educ ~ MOTHERCOLL + exper + expersq, data = d)
summary(first_stage1)

# F檢定 MOTHERCOLL 的效果
cat("(d) F 檢定 MOTHERCOLL:\n")
print(linearHypothesis(first_stage1, "MOTHERCOLL = 0"))

#── (e) 使用 MOTHERCOLL 和 FATHERCOLL 作為工具變數 ──────────────
iv2 <- ivreg(lwage ~ educ + exper + expersq | MOTHERCOLL + FATHERCOLL + exper + expersq, data = d)
summary(iv2)

# 95%信賴區間
cat("(e) 教育係數 95% 信賴區間:\n")
print(confint(iv2, 'educ', level = 0.95))

#── (f) 檢查 MOTHERCOLL 和 FATHERCOLL 對 EDU 的聯合顯著性 ────────
first_stage2 <- lm(educ ~ MOTHERCOLL + FATHERCOLL + exper + expersq, data = d)
summary(first_stage2)

# F檢定 MOTHERCOLL 和 FATHERCOLL 聯合效果
cat("(f) F 檢定 MOTHERCOLL 和 FATHERCOLL:\n")
print(linearHypothesis(first_stage2, c("MOTHERCOLL = 0", "FATHERCOLL = 0")))

#── (g) 工具變數的過度識別檢定（檢查剩餘工具變數是否有效） ────────
cat("(g) 工具變數診斷檢定:\n")
print(summary(iv2, diagnostics = TRUE))

