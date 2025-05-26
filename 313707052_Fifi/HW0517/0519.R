#ch15 17.
library(POE5Rdata)
data("liquor5", package = "POE5Rdata")

names(liquor5)

#b.

library(plm)

data("liquor5", package = "POE5Rdata")

pdata <- pdata.frame(liquor5, index = c("hh", "year"))

re_model <- plm(liquor ~ income, data = pdata, model = "random")

summary(re_model)

confint(re_model)

#c.

plmtest(re_model, type = "bp")


#d.

liquor5$incomem <- ave(liquor5$income, liquor5$hh)
pdata <- pdata.frame(liquor5, index = c("hh", "year"))
re_model_d <- plm(liquor ~ income + incomem, data = pdata, model = "random")

summary(re_model_d)


#15.20.
library(POE5Rdata)
data("star", package = "POE5Rdata")

names(star)

#d.

library(plm)
data("star", package = "POE5Rdata")

pdata <- pdata.frame(star, index = c("schid", "id"))

re_model <- plm(totalscore ~ small + aide + tchexper + white_asian + freelunch, 
                data = pdata, model = "random")

summary(re_model)

plmtest(re_model, type = "bp")

#e.

fe_model <- plm(totalscore ~ small + aide + tchexper + white_asian + freelunch,
                data = pdata, model = "within")

coefs_fe <- coef(summary(fe_model))
coefs_re <- coef(summary(re_model))

hausman_t <- function(b_fe, b_re, se_fe, se_re) {
  (b_fe - b_re) / sqrt(se_fe^2 - se_re^2)
}

vars <- c("small", "aide", "tchexper", "white_asian", "freelunch")
for (v in vars) {
  tval <- hausman_t(coefs_fe[v, "Estimate"], coefs_re[v, "Estimate"],
                    coefs_fe[v, "Std. Error"], coefs_re[v, "Std. Error"])
  cat(paste(v, ": t =", round(tval, 2)), "\n")
}

#f.
library(plm)
library(dplyr)
library(lmtest)
library(sandwich)
library(POE5Rdata)
library(car)

data("star", package = "POE5Rdata")

pdata <- pdata.frame(star, index = c("schid", "id"))

fe_model <- plm(readscore ~ small + aide + tchexper + boy + white_asian + freelunch,
                data = pdata, model = "within")

re_model <- plm(readscore ~ small + aide + tchexper + boy + white_asian + freelunch,
                data = pdata, model = "random")


phtest(fe_model, re_model)


star_avg <- star %>%
  group_by(schid) %>%
  summarise(across(c(small, aide, tchexper, boy, white_asian, freelunch),
                   mean, .names = "{.col}_avg"))

star_m <- left_join(star, star_avg, by = "schid")

ols_model <- lm(readscore ~ small + aide + tchexper + boy + white_asian + freelunch +
                  small_avg + aide_avg + tchexper_avg + boy_avg + white_asian_avg + freelunch_avg,
                data = star_m)

vcov_school <- vcovCL(ols_model, cluster = ~ schid)

linearHypothesis(ols_model,
                 c("small_avg = 0", "aide_avg = 0", "tchexper_avg = 0",
                   "boy_avg = 0", "white_asian_avg = 0", "freelunch_avg = 0"),
                 vcov. = vcov_school)
