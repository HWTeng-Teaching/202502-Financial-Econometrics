library(POE5Rdata)
library(dplyr)
library(plm)
data('liquor5')

# (b)
pdat <- pdata.frame(liquor5, index = c("hh", "year"))

re_mod <- plm(liquor ~ income, data = pdat, model = "random")
summary(re_mod)
beta   <- coef(re_mod)["income"]
se     <- sqrt(vcov(re_mod)["income","income"])
ci_low  <- beta - qnorm(0.975) * se
ci_high <- beta + qnorm(0.975) * se
c(`2.5 %` = ci_low, `97.5 %` = ci_high)

# (c)
plmtest(liquor ~ income, data = pdat,
        type = "bp",        
        effect = "individual")
qchisq(p=0.95,df=1)

# (d)
liquor5 <- liquor5 %>%
  group_by(hh) %>%
  mutate(INCOMEM = mean(income)) %>%
  ungroup()

pdat2 <- pdata.frame(liquor5, index = c("hh", "year"))

re_mundlak <- plm(liquor ~ income + INCOMEM,
                  data = pdat2,
                  model = "random")

summary(re_mundlak)