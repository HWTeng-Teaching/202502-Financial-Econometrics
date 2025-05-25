install.packages("plm")
library(plm)
library(dplyr)
library(lmtest)
library(sandwich)
library(car)

#15.17
data(liquor5)
liquor <- liquor5[order(liquor5$hh, liquor5$year), ]
liquor$LIQUORD <- ave(liquor$liquor, liquor$hh, FUN = function(x) c(NA, diff(x)))
liquor$INCOMED <- ave(liquor$income, liquor$hh, FUN = function(x) c(NA, diff(x)))
liquor_diff <- na.omit(liquor)

#d
liquor <- liquor %>%
  group_by(hh) %>%
  mutate(INCOMEM = mean(income)) %>%
  ungroup()
pdata <- pdata.frame(liquor, index = c("hh", "year"))
cre_model <- plm(liquor ~ income + INCOMEM, data = pdata, model = "random")
summary(cre_model)


#15.20
data("star")
#d
pdata <- pdata.frame(star, index = c("schid", "id"))
re_model <- plm(readscore ~ small + aide + tchexper + boy + white_asian + freelunch,
                data = pdata, model = "random", effect = "individual")
summary(re_model)
plmtest(re_model, type = "bp")

#e
hausman_test <- phtest(fe_model, re_model)
print(hausman_test)

#f
pdata <- pdata.frame(star, index = c("schid", "id"))

fe_b <- plm(readscore ~ small + aide + tchexper + boy + white_asian + freelunch,
            data = pdata, model = "within")

re_d <- plm(readscore ~ small + aide + tchexper + boy + white_asian + freelunch,
            data = pdata, model = "random", random.method = "swar")

haus <- phtest(fe_b, re_d)

tv_raw <- c("small", "aide", "tchexper", "freelunch")

varying <- tv_raw[
  sapply(tv_raw, function(v){
    all(tapply(star[[v]], star$schid, var, na.rm = TRUE) > 0, na.rm = TRUE)
  })
]

sch_avg <- star %>% 
  group_by(schid) %>% 
  summarise(across(c(small, aide, tchexper,
                     boy, white_asian, freelunch),
                   mean, .names = "{.col}_avg"))

star_m <- left_join(star, sch_avg, by = "schid") %>%
  pdata.frame(index = c("schid","id"))

form_m <- readscore ~ small + aide + tchexper + boy + white_asian + freelunch +
  small_avg + aide_avg + tchexper_avg +
  boy_avg + white_asian_avg + freelunch_avg

ols_m   <- lm(form_m, data = star_m)
vc_cl   <- vcovCL(ols_m, cluster = ~ schid)

linearHypothesis(ols_m,
                 c("small_avg = 0", "aide_avg = 0", "tchexper_avg = 0",
                   "boy_avg = 0", "white_asian_avg = 0", "freelunch_avg = 0"),
                 vcov. = vc_cl)

