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

#15.17.b
library(plm)
pdat <- pdata.frame(liquor5, index = c("hh", "year"))
random_mod <- plm(liquor ~ income, data = pdat, model = "random")
summary(random_mod)
confint(random_mod)

#15.17.c
plmtest(random_mod,effect = "individual")

#15.17.d
library(dplyr)
liquor5 <- liquor5 %>%
  group_by(hh) %>%
  mutate(INCOMEM = mean(income)) %>%
  ungroup()

pdat1 <- pdata.frame(liquor5, index = c("hh", "year"))
random_mundlak <- plm(liquor ~ income + INCOMEM,
                  data = pdat1,
                  model = "random")
summary(random_mundlak)

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

#15.20.d
library(plm)
random_mod2 <- plm(
  readscore ~ small + aide + tchexper + boy + white_asian + freelunch,
  data  = pa_data,
  model = "random"
)
summary(random_mod2)
plmtest(
  readscore ~ small + aide + tchexper + boy + white_asian + freelunch,
  data   = pa_data,
  type   = "bp",
  effect = "individual"
)

#15.20.e
hausmantest <- phtest(FE_model, random_mod2)
hausmantest
# 係數
coef_FE <- coef(FE_model)
coef_RE <- coef(random_mod2)
# 標準誤
se_FE <- sqrt(diag(vcov(FE_model)))
se_RE <- sqrt(diag(vcov(random_mod2)))
# 指定要比較的變數
vars <- c("small", "aide", "tchexper", "white_asian", "freelunch")
t_values <- sapply(vars, function(var) {
  (coef_FE[var] - coef_RE[var]) / sqrt(se_FE[var]^2 - se_RE[var]^2)
})
names(t_values) <- vars
print(t_values)

#15.20.f
library(plm)
library(lmtest)
library(sandwich)
library(car)

# 對每個學校計算變數平均值，命名為 *_m
pa_data$small_m       <- ave(pa_data$small,       pa_data$schid)
pa_data$aide_m        <- ave(pa_data$aide,        pa_data$schid)
pa_data$tchexper_m    <- ave(pa_data$tchexper,    pa_data$schid)
pa_data$boy_m         <- ave(pa_data$boy,         pa_data$schid)
pa_data$white_asian_m <- ave(pa_data$white_asian, pa_data$schid)
pa_data$freelunch_m   <- ave(pa_data$freelunch,   pa_data$schid)

# 確保無 NA 值
pdata_clean <- na.omit(pa_data)

# Mundlak 模型
mundlak_model <- plm(readscore ~ small + aide + tchexper + boy + white_asian + freelunch +
                       small_m + aide_m + tchexper_m + boy_m + white_asian_m + freelunch_m,
                     data = pdata_clean,
                     model = "random")

summary(mundlak_model)
vc_cl   <- vcovCL(mundlak_model, cluster = ~ schid)

linearHypothesis(ols_m,
                 c("small_avg = 0", "aide_avg = 0", "tchexper_avg = 0",
                   "boy_avg = 0", "white_asian_avg = 0", "freelunch_avg = 0"),
                 vcov. = vc_cl)