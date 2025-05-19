url <- "https://www.principlesofeconometrics.com/poe5/data/rdata/liquor5.rdata"
file_path <- "liquor5.rdata"
download.file(url, file_path, mode = "wb")
load(file_path)
names(liquor5)

library(dplyr)

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
