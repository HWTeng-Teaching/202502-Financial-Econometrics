# 載入必要套件
library(dplyr)

# 1. 讀入資料
df <- read.csv("liquor5.csv")

# 2. 依 hh, year 排序，並做 first‐difference
df_diff <- df %>%
  arrange(hh, year) %>%
  group_by(hh) %>%
  mutate(
    LIQUORD  = liquor - lag(liquor),
    INCOMED  = income - lag(income)
  ) %>%
  ungroup() %>%
  filter(!is.na(LIQUORD) & !is.na(INCOMED))

# 3. OLS 無截距迴歸
model <- lm(LIQUORD ~ 0 + INCOMED, data = df_diff)

# 4. 檢視摘要
summary(model)

# 5. 95% 信賴區間
confint(model, level = 0.95)
