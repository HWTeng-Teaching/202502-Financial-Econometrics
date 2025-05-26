
# Declare panel structure
pdata <- pdata.frame(liquor, index = c("ID", "YEAR"))

### a. First-differencing and OLS regression without intercept
pdata <- pdata %>%
  group_by(ID) %>%
  mutate(
    LIQUORD = LIQUOR - lag(LIQUOR),
    INCOMED = INCOME - lag(INCOME)
  ) %>%
  ungroup()

# Remove NAs from first-difference (i.e., first year for each household)
pdata_fd <- pdata %>% filter(!is.na(LIQUORD) & !is.na(INCOMED))

# OLS regression of LIQUORD on INCOMED without intercept
model_fd <- lm(LIQUORD ~ INCOMED - 1, data = pdata_fd)
summary(model_fd)

# 95% confidence interval for the coefficient
confint(model_fd, level = 0.95)
