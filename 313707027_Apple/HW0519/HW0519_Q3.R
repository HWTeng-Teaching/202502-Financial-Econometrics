# 15.20
library(POE5Rdata)
data(star) 
library(plm)

# (a)
model_a <- lm(readscore ~ small + aide + tchexper + boy + white_asian + freelunch, data = star)
summary(model_a)

# (b) fixed effects regression
pdata <- pdata.frame(star, index = c("schid", "id"))
model_b <- plm(readscore ~ small + aide + tchexper + boy + white_asian + freelunch, data = pdata,
               model = "within",  # fixed effects
               effect = "individual")  # fixed SCHID
summary(model_b)

# (c)
pFtest(model_b, model_a)

# (d) random effects model
model_d <- plm(readscore ~ small + aide + tchexper + boy + white_asian + freelunch,
               data = pdata,
               model = "random",  
               effect = "individual") 

summary(model_d)

plmtest(model_d, type = "bp")

# (e)
hausman_test <- phtest(model_b, model_d)
print(hausman_test)

# (f)
library(dplyr)
library(lmtest)
library(sandwich)
library(car)

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

star_model <- left_join(star, sch_avg, by = "schid") %>%
  pdata.frame(index = c("schid","id"))

form_model <- readscore ~ small + aide + tchexper + boy + white_asian + freelunch +
  small_avg + aide_avg + tchexper_avg +
  boy_avg + white_asian_avg + freelunch_avg

ols_model   <- lm(form_model, data = star_m)
vc_cl   <- vcovCL(ols_model, cluster = ~ schid)

linearHypothesis(ols_model,
                 c("small_avg = 0", "aide_avg = 0", "tchexper_avg = 0",
                   "boy_avg = 0", "white_asian_avg = 0", "freelunch_avg = 0"),
                 vcov. = vc_cl)
