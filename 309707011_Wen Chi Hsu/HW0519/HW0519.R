#15.17
#b
remotes::install_github("ccolonescu/POE5Rdata", force = TRUE)
library(POE5Rdata)
data("liquor5")
library(plm)       
library(lmtest)    
library(sandwich)  

pdata <- pdata.frame(liquor5, index = c("hh", "year"))
re_mod <- plm(liquor ~ income, data = pdata, model = "random")
summary(re_mod)

se <- sqrt(vcovHC(re_mod, type = "HC0")["income", "income"])
beta <- coef(re_mod)["income"]
ci <- beta + c(-1, 1) * 1.96 * se
cat("95% CI：", round(ci, 4), "\n")

#c
lm_test <- plmtest(liquor ~ income, data = pdata, effect = "individual", type = "bp")
print(lm_test)

#d
pdata$incomem <- ave(pdata$income, pdata$hh)
re_mod2 <- plm(liquor ~ income + incomem, data = pdata, model = "random")
summary(re_mod2)
coeftest(re_mod2, vcov. = vcovHC(re_mod2, type = "HC0"))

#15.20
#d
data("star")
star$sid <- star$id
newdata <- pdata.frame(star, index = c("schid", "sid"))

model_d <- plm(readscore ~ small + aide + tchexper + boy + white_asian + freelunch,
               data = newdata, model = "random")
summary(model_d)

plmtest(
  readscore ~ small + aide + tchexper + boy + white_asian + freelunch,
  data   = newdata,
  type   = "bp",
  effect = "individual"
)

#e
model_b <- plm(readscore ~ small + aide + tchexper + boy + white_asian + freelunch
               , data = newdata, model = "within")
hausman_test <- phtest(model_b, model_d)
hausman_test

fe_coef <- coef(summary(model_b))["boy", "Estimate"]
fe_se <- coef(summary(model_b))["boy", "Std. Error"]
re_coef <- coef(summary(model_d))["boy", "Estimate"]
re_se <- coef(summary(model_d))["boy", "Std. Error"]

t_stat <- (fe_coef - re_coef) / sqrt(fe_se^2 + re_se^2)
t_stat
fe_tab <- coef(summary(model_b))
re_tab <- coef(summary(model_d))
vars <- c("small", "aide", "tchexper", "white_asian", "freelunch")

for (v in vars) {
  diff    <- fe_tab[v, "Estimate"] - re_tab[v, "Estimate"]
  # Var(diff) = Var_FE - Var_RE  under RE exogeneity assumption
  se_diff <- sqrt(fe_tab[v, "Std. Error"]^2 - re_tab[v, "Std. Error"]^2)
  t_stat  <- diff / se_diff
  p_val   <- 2 * (1 - pnorm(abs(t_stat)))
  cat(sprintf("%-12s:  t = %5.2f,  p = %.3f\n", v, t_stat, p_val))
}
v <- "boy"
diff    <- fe_tab[v, "Estimate"] - re_tab[v, "Estimate"]
se_diff <- sqrt(fe_tab[v, "Std. Error"]^2 - re_tab[v, "Std. Error"]^2)
t_stat  <- diff / se_diff
p_val   <- 2 * (1 - pnorm(abs(t_stat)))
cat(sprintf("%-12s:  t = %5.2f,  p = %.3f  (boy)\n", v, t_stat, p_val))

#f
library(plm)
library(lmtest)
library(sandwich)

data("star", package="plm")
# schid = school id, id = student id
pdat <- pdata.frame(star, index = c("schid","id"))

pdat$small_m      <- ave(pdat$small,       pdat$schid)
pdat$aide_m       <- ave(pdat$aide,        pdat$schid)
pdat$tchexper_m   <- ave(pdat$tchexper,    pdat$schid)
pdat$boy_m        <- ave(pdat$boy,         pdat$schid)
pdat$white_asian_m<- ave(pdat$white_asian, pdat$schid)
pdat$freelunch_m  <- ave(pdat$freelunch,   pdat$schid)

re_mundlak <- plm(
  readscore ~ small + aide + tchexper + boy + white_asian + freelunch
  + small_m + aide_m + tchexper_m + boy_m + white_asian_m + freelunch_m,
  data  = pdat,
  model = "random"
)

summary(re_mundlak)