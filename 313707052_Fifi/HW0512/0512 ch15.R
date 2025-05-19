library(POE5Rdata)
data("liquor5", package = "POE5Rdata")

names(liquor5)

# 15.7 a.

library(dplyr)      # 含 %>%
library(magrittr) 

liquor_diff <- liquor5 %>%
  arrange(hh, year) %>%
  group_by(hh) %>%
  mutate(
    LIQUORD = liquor - lag(liquor),
    INCOMED = income - lag(income)
  ) %>%
  filter(!is.na(LIQUORD))  

model <- lm(LIQUORD ~ INCOMED + 0, data = liquor_diff)

summary(model)

confint(model, level = 0.95)

#ansa.INCOMED -0.02841457 0.08790818

#15.20a.
library(POE5Rdata)
data("star", package = "POE5Rdata")

names(star)

#a.
install.packages("plm")
library(dplyr)
library(plm)     # 用於固定效果模型
library(lmtest)  # 用於模型檢定
library(sandwich)  # 用於 robust 標準誤

ols_model <- lm(readscore ~ small + aide + tchexper + boy + white_asian + freelunch, data = star)
summary(ols_model)

#b.

pdata <- pdata.frame(star, index = c("schid", "id"))

fe_model <- plm(readscore ~ small + aide + tchexper + boy + white_asian + freelunch,
                data = pdata, model = "within", effect = "individual")

summary(fe_model)

#c.


ols_model_plm <- plm(readscore ~ small + aide + tchexper + boy + white_asian + freelunch,
                     data = pdata, model = "pooling")


pFtest(fe_model, ols_model_plm)

#c.ans F = 16.698, df1 = 78, df2 = 5681, p-value < 2.2e-16
#alternative hypothesis: significant effects