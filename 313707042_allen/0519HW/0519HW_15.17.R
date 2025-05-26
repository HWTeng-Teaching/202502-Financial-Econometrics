library(POE5Rdata)
data(liquor5)
?data
#b
library(plm)
pdata <- pdata.frame(liquor5, index = c("hh", "year"))
re_model <- plm(liquor ~ income, data = pdata, model = "random")

summary(re_model)
confint(re_model)
#c
plmtest(plm(liquor ~ income, data = pdata, model = "pooling"), type = "bp")
#d
liquor5$income_mean <- ave(liquor5$income, liquor5$hh, FUN = mean)
cre_model <- plm(liquor ~ income + income_mean, data = pdata, model = "random")
summary(cre_model)

