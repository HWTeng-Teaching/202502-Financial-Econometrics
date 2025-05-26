library(POE5Rdata)
library(tidyverse)
library(plm)
data(liquor5)

fd_ols <- liquor5 %>%
  group_by(hh) %>%      
  arrange(year) %>%               
  mutate(
    LIQUORD  = liquor  - lag(liquor),
    INCOMED  = income - lag(income)
  ) %>%
  filter(!is.na(liquor) & !is.na(income)) %>%
  ungroup()

fd_mod <- lm(LIQUORD ~ INCOMED - 1, data = fd_ols)

summary(fd_mod)
confint(fd_mod, level = 0.95)

# b
# 轉換為 panel data
pdata <- pdata.frame(liquor5, index = c("hh", "year"))

# 隨機效果模型
model_b <- plm(liquor ~ income, data = pdata, model = "random")
summary(model_b)

# 95%信賴區間
confint(model_b)

# C
# LM檢定
plmtest(liquor ~ income, data = pdata, effect = "individual", type = "bp")

# D
# 計算 INCOME 的群體平均值
pdata <- pdata %>%
  group_by(hh) %>%
  mutate(mean_income = mean(income, na.rm = TRUE)) %>%
  ungroup()

# Mundlak 模型
model_d <- plm(liquor ~ income + mean_income, data = pdata, model = "random")
summary(model_d)


