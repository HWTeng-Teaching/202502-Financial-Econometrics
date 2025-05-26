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

### b. Random effects model
model_re <- plm(LIQUOR ~ INCOME, data = pdata, model = "random")
summary(model_re)

# 95% CI using robust standard errors
coeftest(model_re, vcov = vcovHC(model_re, method = "arellano", type = "HC1"))
confint_re <- confint(model_re, level = 0.95)
print(confint_re)

### c. LM test for random effects (Breusch-Pagan Lagrange Multiplier)
plmtest(LIQUOR ~ INCOME, data = pdata, effect = "individual", type = "bp")

### d. Add time average of income and test for endogeneity (Hausman-type test)

# Compute individual means
income_mean <- pdata %>%
  group_by(ID) %>%
  summarise(INCOMEM = mean(INCOME)) 

# Merge back to pdata
pdata <- merge(pdata, income_mean, by = "ID")

# Estimate augmented RE model
model_aug_re <- plm(LIQUOR ~ INCOME + INCOMEM, data = pdata, model = "random")
summary(model_aug_re)

# Test significance of gamma (coefficient on INCOMEM)
coeftest(model_aug_re, vcov = vcovHC(model_aug_re, method = "arellano", type = "HC1"))

# Interpretation:
# If gamma is statistically significant, RE assumption is invalid (i.e., corr(ui, INCOME) ≠ 0)
