url <- "https://www.principlesofeconometrics.com/poe5/data/rdata/liquor5.rdata"
file_path <- "liquor5.rdata"
download.file(url, file_path, mode = "wb")
load(file_path)
names(liquor5)

library(dplyr)
install.packages("plm")  
library(plm)      

##15.17.1
liquor5_diff <- liquor5 %>%
  arrange(hh, year) %>%     
  group_by(hh) %>%
  mutate(
    liquord = liquor - lag(liquor),
    incomed = income - lag(income)
  ) %>%
  ungroup() %>%
  filter(!is.na(liquord), !is.na(incomed))

model <- lm(liquord ~ incomed - 1, data = liquor5_diff)
summary(model)
confint(model, level = 0.95)

##15.17.2
pdat <- pdata.frame(liquor5, index = c("hh", "year"))

re_mod <- plm(liquor ~ income, data = pdat, model = "random")
summary(re_mod)
beta   <- coef(re_mod)["income"]
se     <- sqrt(vcov(re_mod)["income","income"])
ci_low  <- beta - qnorm(0.975) * se
ci_high <- beta + qnorm(0.975) * se
c(`2.5 %` = ci_low, `97.5 %` = ci_high)

##15.17.3
plmtest(liquor ~ income, data = pdat,
        type = "bp",        # Breusch–Pagan
        effect = "individual")
qchisq(p=0.95,df=1)

##15.17.4
liquor5 <- liquor5 %>%
  group_by(hh) %>%
  mutate(INCOMEM = mean(income)) %>%
  ungroup()

pdat2 <- pdata.frame(liquor5, index = c("hh", "year"))

re_mundlak <- plm(liquor ~ income + INCOMEM,
                  data = pdat2,
                  model = "random")

summary(re_mundlak)

