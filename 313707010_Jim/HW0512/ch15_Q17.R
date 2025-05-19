library(POE5Rdata)
library(dplyr)
data('liquor5')

liquor5_diff <- liquor5 %>%
  arrange(hh, year) %>%     
  group_by(hh) %>%
  mutate(
    liquord = liquor - lag(liquor),
    incomed = income - lag(income)
  ) %>%
  ungroup() %>%
  filter(!is.na(liquord), !is.na(incomed))

model <- lm(liquord ~ incomed - 1, data = liquor5_diff)
summary(model)
confint(model, level = 0.95)

