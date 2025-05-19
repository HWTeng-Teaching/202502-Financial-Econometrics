url <- "https://www.principlesofeconometrics.com/poe5/data/rdata/liquor5.rdata"
file_path <- "liquor5.rdata"
download.file(url, file_path, mode = "wb")
load(file_path)
ls()
head(liquor5)


liquor_fd <- liquor5 %>%
  group_by(hh) %>%      
  arrange(year) %>%               
  mutate(
    LIQUORD  = liquor  - lag(liquor),
    INCOMED  = income - lag(income)
  ) %>%
  filter(!is.na(liquor) & !is.na(income)) %>%
  ungroup()

fd_mod <- lm(LIQUORD ~ INCOMED - 1, data = liquor_fd)

summary(fd_mod)
confint(fd_mod, level = 0.95)