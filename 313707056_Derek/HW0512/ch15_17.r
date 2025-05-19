library(POE5Rdata)
library(tidyverse)
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
