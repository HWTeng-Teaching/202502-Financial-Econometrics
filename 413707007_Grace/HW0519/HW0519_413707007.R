#15.17

load("C:\\Users\\thaol\\Downloads\\liquor5.rdata")
View(liquor5)
library(dplyr)
liquor_diff <- liquor5 %>%
  arrange(hh, year) %>%
  group_by(hh) %>%
  mutate(
    LIQUORD = liquor - lag(liquor),
    INCOMED = income - lag(income)
  ) %>%
  filter(!is.na(LIQUORD) & !is.na(INCOMED)) 

moda <- lm(LIQUORD ~ INCOMED - 1, data = liquor_diff)
summary(moda)
coef_income <- coef(summary(moda))["INCOMED", "Estimate"]
std_income <- coef(summary(moda))["INCOMED", "Std. Error"]
conf_income <- confint(moda, level = 0.95)
conf_income

#b. RE
library(plm)
library(lmtest)
mod_re <- plm(liquor ~ income, data = liquor5, model = "random")
summary(mod_re)
conf_income_re <- confint(mod_re, level = 0.95)
conf_income_re

#c.
mod_fe <- plm(liquor ~ income, data = liquor5, model = "within")
lm_test <- plmtest(mod_fe, effect = "individual", type = "bp")  
print(lm_test)

#d.
liquor5$incomem <- ave(liquor5$income, liquor5$hh, FUN = function(x) mean(x, na.rm = TRUE))
View(liquor5)
modd <- plm(liquor ~ income + incomem, data = liquor5, model = "random")
summary(modd)


#15.20
load("C:\\Users\\thaol\\Downloads\\star.rdata")
View(star)
#a.
pdata <- pdata.frame(star, index = "schid")

mod_ols1520 <- plm(readscore ~ small + aide + tchexper + boy + white_asian + freelunch,data = pdata, model = "pooling")
summary(mod_ols1520)

#b.
mod_fe1520 <- plm(readscore ~ small + aide + tchexper + boy + white_asian + freelunch, data = pdata, model = "within")
summary(mod_fe1520)

#c.
pFtest(mod_fe1520, mod_ols1520)

#d.
