remotes::install_github("ccolonescu/POE5Rdata", force = TRUE)
library(POE5Rdata)
library(ggplot2)
install.packages(c("tidyverse","AER","systemfit","broom"))
library(AER)         
library(systemfit)  
library(tidyverse)
library(broom)
library(dplyr)
install.packages("plm") 
library(plm)  

#15.17.b
pdat <- pdata.frame(liquor5, index = c("hh", "year"))

re_mod <- plm(liquor ~ income, data = pdat, model = "random")
summary(re_mod)
beta   <- coef(re_mod)["income"]
se     <- sqrt(vcov(re_mod)["income","income"])
ci_low  <- beta - qnorm(0.975) * se
ci_high <- beta + qnorm(0.975) * se
c(`2.5 %` = ci_low, `97.5 %` = ci_high)

#15.17.c
plmtest(liquor ~ income, data = pdat,
        type = "bp",        # Breusch–Pagan
        effect = "individual")

#15.17.d
liquor5 <- liquor5 %>%
  group_by(hh) %>%
  mutate(INCOMEM = mean(income)) %>%
  ungroup()

pdat2 <- pdata.frame(liquor5, index = c("hh", "year"))

re_mundlak <- plm(liquor ~ income + INCOMEM,
                  data = pdat2,
                  model = "random")

summary(re_mundlak)

coef_table <- summary(re_mundlak)$coefficients
gamma_est   <- coef_table["INCOMEM","Estimate"]
gamma_se    <- coef_table["INCOMEM","Std. Error"]
gamma_zstat <- coef_table["INCOMEM", "z-value"]
gamma_pval  <- coef_table["INCOMEM", "Pr(>|z|)"]
c(Estimate = gamma_est,
  `Std. Error` = gamma_se,
  `z value`    = gamma_zstat,
  `p value`    = gamma_pval)

#15.20.d
re_d <- plm(
  readscore ~ small + aide + tchexper + boy + white_asian + freelunch,
  data  = pdata,
  model = "random"
)
summary(re_d)

plmtest(
  readscore ~ small + aide + tchexper + boy + white_asian + freelunch,
  data   = pdata,
  type   = "bp",
  effect = "individual"
)
#15.20.e
haus <- phtest(fe_b, re_d)
haus
fe_tab <- coef(summary(fe_b))
re_tab <- coef(summary(re_d))

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

#15.20.f
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
