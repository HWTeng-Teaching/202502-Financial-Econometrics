install.packages("remotes")
library(remotes)
remotes::install_github("ccolonescu/POE5Rdata", force = TRUE)
library(POE5Rdata)
install.packages("plm")
library(plm)
library(dplyr)

#15.17
#a
data(liquor5)
liquor <- liquor5[order(liquor5$hh, liquor5$year), ]
liquor$LIQUORD <- ave(liquor$liquor, liquor$hh, FUN = function(x) c(NA, diff(x)))
liquor$INCOMED <- ave(liquor$income, liquor$hh, FUN = function(x) c(NA, diff(x)))
liquor_diff <- na.omit(liquor)
model <- lm(LIQUORD ~ INCOMED - 1, data = liquor_diff)
summary(model)
confint(model)

#15.20
#a
data("star")
model_a <- lm(readscore ~ small + aide + tchexper + boy + white_asian + freelunch, data = star)
summary(model_a)

#b
pdata <- pdata.frame(star, index = c("schid", "id"))
fe_model <- plm(readscore ~ small + aide + tchexper + boy + white_asian + freelunch,
                data = pdata, model = "within", effect = "individual")
summary(fe_model)

#c
pooled_model <- plm(readscore ~ small + aide + tchexper + boy + white_asian + freelunch,
                    data = pdata, model = "pooling")
pFtest(fe_model, pooled_model)

