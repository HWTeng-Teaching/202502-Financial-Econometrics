rm(list=ls())
# 🔗 下載並載入 vacation 資料集
temp_file <- tempfile(fileext = ".rdata")
download.file(url = "https://www.principlesofeconometrics.com/poe5/data/rdata/mroz.rdata",
              destfile = temp_file, mode = "wb")
load(temp_file)
head(mroz)

#A

# -------------------------------------------------------------
# 1. 僅保留 1975 年有工作的樣本 (lfp == 1) —— 共 428 筆
# -------------------------------------------------------------
mroz_work = subset(mroz, lfp == 1) # 428 obs

# -------------------------------------------------------------
# 2. 建立父母「有上過大學」的虛擬變數
#    - MOTHERCOLL = 1  ⇔  mothereduc > 12
#    - FATHERCOLL = 1  ⇔  fathereduc > 12
# -------------------------------------------------------------

mroz_work$mothercoll <- as.integer(mroz_work$mothereduc > 12 )
mroz_work$fathercoll <- as.integer(mroz_work$fathereduc > 12 )

# -------------------------------------------------------------
# 3. 計算比例（%）
# -------------------------------------------------------------

pct_mothercoll <- mean(mroz_work$mothercoll) * 100
pct_fathercoll <- mean(mroz_work$fathercoll) * 100

pct_fathercoll
pct_mothercoll

# -------------------------------------------------------------
# 4. 輸出結果
# -------------------------------------------------------------
cat(sprintf("母親受過大學教育的比例：%.2f%%\n", pct_mothercoll))
cat(sprintf("父親受過大學教育的比例：%.2f%%\n", pct_fathercoll))

#B
vars <- mroz_work[, c('educ', 'mothercoll', 'fathercoll')]
corr <- cor(vars)
corr

#C

library(AER)
library(dplyr)

mroz_work <- mroz %>% 
  filter(lfp == 1, wage > 0) %>%            # 只留 428 位中 wage>0 的 426 筆
  mutate(
    MOTHERCOLL = as.integer(mothereduc > 12),  # 工具變數（大學以上 = 1）
    lnwage     = log(wage),                    # 被解釋變數
    exper2     = exper^2                       # 經驗年數平方
  )

# -------------------------------------------------------------
# 2. IV／2SLS 估計：ln(wage) ~ exper + exper² + educ
#    educ 為內生變數，工具 = MOTHERCOLL
# -------------------------------------------------------------

iv_fit <- ivreg(
  lnwage ~ exper + exper2 + educ |           # 第二階段方程
    MOTHERCOLL + exper + exper2,     # 第一步所用工具與外生變數
  data = mroz_work
)

summary(iv_fit, diagnostics = TRUE)          # 檢視估計結果與弱工具 F 統計
#工具非常強
# 3. EDUC 係數的 95% 信賴區間
confint(iv_fit)["educ", ]

#D
## 第一階段：以 EDUC 為應變數
first <- lm(educ ~ exper + exper2 + MOTHERCOLL, data = mroz_work)
summary(first)
#T值夠大，是強工具，與C結果一致

#E
mroz_work <- mroz %>% 
  filter(lfp == 1, wage > 0) %>%           # 426 筆 (wage>0)
  mutate(
    lnwage     = log(wage),               # 被解釋變數
    exper2     = exper^2,                 # 經驗平方
    MOTHERCOLL = as.integer(mothereduc  > 12),
    FATHERCOLL = as.integer(fathereduc  > 12)
  )

iv2_fit <- ivreg(
  lnwage ~ exper + exper2 + educ |
    exper + exper2 + MOTHERCOLL + FATHERCOLL,
  data = mroz_work
)

summary(iv2_fit, diagnostics = TRUE)  # 可一起查看弱工具與過度識別檢定

# 3. 取 EDUC 係數的 95 % 信賴區間
ci_educ_iv2 <- confint(iv2_fit)["educ", ]
print(ci_educ_iv2)
#變窄了 而且排除0

#F
library(car)

# -------------------------------------------------------------
# 1. 準備工作樣本與工具
# -------------------------------------------------------------
mroz_work <- mroz %>% 
  filter(lfp == 1, wage > 0) %>%           # 426 筆
  mutate(
    exper2     = exper^2,
    MOTHERCOLL = as.integer(mothereduc > 12),
    FATHERCOLL = as.integer(fathereduc > 12)
  )

# -------------------------------------------------------------
# 2. 第一階段迴歸：EDUC ~ exper + exper2 + MOTHERCOLL + FATHERCOLL
# -------------------------------------------------------------
first2 <- lm(educ ~ exper + exper2 + MOTHERCOLL + FATHERCOLL,
             data = mroz_work)

summary(first2)                          # 查看係數與個別 t 值

# -------------------------------------------------------------
# 3. 聯合檢定：兩個工具係數同時為 0
# -------------------------------------------------------------
# H0: MOTHERCOLL = 0  並且  FATHERCOLL = 0
f_test <- linearHypothesis(first2,
                           c("MOTHERCOLL = 0",
                             "FATHERCOLL = 0"))
print(f_test)
#兩工具變數係數明顯至少其一不為0，足以避免弱工具變數所帶來的不一致問題

#G
# iv2_fit 來自上一題：使用兩個工具的 2SLS 模型
summary(iv2_fit, diagnostics = TRUE)
#無法拒絕H0：所有工具皆外生
