#15.17.a
library(tidyverse)
library(POE5Rdata)
data("liquor5")
summary(liquor5)
liquorfd <- liquor5 %>%
  group_by(hh) %>%      
  arrange(year) %>%               
  mutate(
    LIQUORD  = liquor  - lag(liquor),
    INCOMED  = income - lag(income)
  ) %>%
  filter(!is.na(liquor) & !is.na(income)) %>%
  ungroup()

fd_mod <- lm(LIQUORD ~ INCOMED - 1, data = liquorfd)
summary(fd_mod)
confint(fd_mod, level = 0.95)

#15.20.a
data("star")
model_ols <- lm(readscore ~ small + aide + tchexper + boy + white_asian + freelunch, data = star)
summary(model_ols)


#15.20.b
install.packages("plm")
library(plm)
pa_data <- pdata.frame(star, index = c("schid", "id"))
FE_model <- plm(readscore ~ small + aide + tchexper + boy + white_asian + freelunch,
                data = pa_data, model = "within", effect = "individual")
summary(FE_model)

#15.20.c
pool_model <- plm(readscore ~ small + aide + tchexper + boy + white_asian + freelunch,
                    data = pa_data, model = "pooling")
pFtest(FE_model, pool_model)