data <- POE5Rdata::star
#Q15.20.d
library(plm)
library(dplyr)
pdata <- pdata.frame(data, index = c("schid", "id")) 
modre <- plm(readscore ~ small + aide + tchexper + boy + white_asian + freelunch , data = pdata, model = 'random',random.method = 'swar')
summary(modre)

modpooled <- plm(readscore ~ small + aide + tchexper + boy + white_asian + freelunch , data = pdata, model = 'pooling')
lmtest <- plmtest(modpooled, effect = 'individual')
lmtest

#Q15.20.e
modfe <- plm(readscore ~ small + aide + tchexper + boy + white_asian + freelunch , data = pdata, model = 'within')

beta_fe <- coef(modfe)
se_fe <- sqrt(diag(vcov(modfe)))
beta_re <- coef(modre)
se_re <- sqrt(diag(vcov(modre)))

t  <- (beta_fe-beta_re)/(se_fe^2-se_re^2)^0.5
t

#Q15.20.f

school_averages <- data %>%
  group_by(schid) %>%
  summarise(across(c(small, aide, tchexper, boy, white_asian, freelunch), mean, na.rm = TRUE)) %>%
  rename_with(~ paste0(., "_avg"), -schid)

# 合併回原始資料
star_with_avg <- left_join(data, school_averages, by = "schid")

star_with_avg <- pdata.frame(star_with_avg, index = c("schid", "id")) 

formula_mundlak <- as.formula(
  paste("readscore ~ small + aide + tchexper + boy + white_asian + freelunch +",
        paste0(c("small_avg", "aide_avg", "tchexper_avg", "boy_avg", "white_asian_avg", "freelunch_avg"), collapse = " + "))
)

mundlak_mod <- plm(formula_mundlak, data = star_with_avg, model = "pooling")
summary(mundlak_mod)
library(car)
linearHypothesis(mundlak_mod, c(
  "small_avg = 0", "aide_avg = 0", "tchexper_avg = 0",
  "boy_avg = 0", "white_asian_avg = 0", "freelunch_avg = 0"
))

#Q15.17.b

library(tidyr)
library(dplyr)
library(plm)

data("liquor", package = "PoEdata")

# 先把 liquor 與 income 各自轉成 long 格式
liquor_long <- liquor %>%
  select(hh, starts_with("l")) %>%
  pivot_longer(cols = starts_with("l"), names_to = "time", values_to = "liquor") %>%
  mutate(time = as.numeric(sub("l", "", time)))  # 把 l1 變成 1

income_long <- liquor %>%
  select(hh, starts_with("x")) %>%
  pivot_longer(cols = starts_with("x"), names_to = "time", values_to = "income") %>%
  mutate(time = as.numeric(sub("x", "", time)))  # 把 x1 變成 1

# 合併 liquor 與 income
panel_data <- liquor_long %>%
  inner_join(income_long, by = c("hh", "time")) %>%
  arrange(hh, time)

# 建立 pdata.frame
pdata <- pdata.frame(panel_data, index = c("hh", "time"))

modre <- plm(liquor ~ income, data = pdata, model = 'random')
summary(modre)
conf <- confint(modre, level = 0.05)
conf
modpooled <- plm(liquor ~ income, data = pdata, model = 'pooling')

#Q15.17.c
lmtest <- plmtest(modpooled, effect = 'individual')
lmtest

#Q15.17.d
panel_data <- panel_data %>%
  group_by(hh) %>%
  mutate(incomem = mean(income, na.rm = TRUE)) %>%
  ungroup()
pdata <- pdata.frame(panel_data, index = c("hh", "time"))

modre2 <- plm(liquor ~ income + incomem , data = pdata , model ='random')
summary(modre2)
