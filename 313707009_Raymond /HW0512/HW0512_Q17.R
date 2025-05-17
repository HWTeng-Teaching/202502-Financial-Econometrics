if (!requireNamespace("remotes", quietly = TRUE)) install.packages("remotes")
remotes::install_github("ccolonescu/POE5Rdata", force = TRUE)
install.packages("car")  
library(car)
install.packages("AER")  
library(AER)
library(POE5Rdata)
data('liquor5')
library(dplyr)

liquor_diff <- liquor5 %>%
  arrange(hh, year) %>%
  group_by(hh) %>%
  mutate(
    liquord = liquor - lag(liquor),
    incomed = income - lag(income)
  ) %>%
  ungroup() %>%
  filter(!is.na(liquord) & !is.na(incomed))  # 去除 NA 差分

# 執行無截距的線性回歸 liquord ~ incomed - 1
model <- lm(liquord ~ incomed - 1, data = liquor_diff)

# 顯示回歸結果
summary(model)

# 建立 95% 信賴區間
confint(model, level = 0.95)
