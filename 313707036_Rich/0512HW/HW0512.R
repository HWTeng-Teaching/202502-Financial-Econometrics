#Q15.20.a
data <- POE5Rdata::star
modpooled <- lm(readscore ~ small + aide + tchexper + boy + white_asian + freelunch , data = data)
summary(modpooled)

#Q15.20.b
library(plm)
pdata <- pdata.frame(data, index = c("schid", "id")) 
modfe <- plm(readscore ~ small + aide + tchexper + boy + white_asian + freelunch , data = pdata, method = 'within')
summary(modfe)

#Q15.20.c

pFtest(modfe, modpooled)


#Q15.17.a
library(PoEdata)
data('liquor')

library(dplyr)

liquor <- liquor %>%
  mutate(
    liquor = l2 - l1,
    income = x2 - x1
  )

mod1 <- lm(hh ~ 0 + liquor + income , data = liquor)
summary(mod1)
conf <- confint(mod1, level = 0.05)
conf

