#-----------------C15Q06-----------------
#(d)
alpha = 0.01
qf(1 - alpha, 715, 711)
#-----------------C15Q20-----------------
rm(list = ls())
library(POE5Rdata)
#install.packages('plm')
library(plm)
data("star")
#(a)
mod <- lm(readscore ~ small+aide+tchexper+boy+white_asian+freelunch , data = star)
sum = summary(mod)
sum$coefficients

#(b)
star$sid <- star$id
pdata <- pdata.frame(star, index = c("schid", "sid"))
pmod <- plm(readscore ~ small+aide+tchexper+boy+white_asian+freelunch,
                data = pdata, model = "within")
sum = summary(pmod)
sum$coefficients

#(c)
pFtest(pmod, mod)

S#-----------------C15Q17-----------------
rm(list = ls())
library(POE5Rdata)
#ls("package:POE5Rdata")
data("liquor5")

library(dplyr)
library(plm)

pdata <- pdata.frame(liquor5, index = c("hh", "year"))

pdata$LIQUORD <- diff(pdata$liquor)
pdata$INCOMED <- diff(pdata$income)
pdata

pdata_clean <- na.omit(pdata)
model <- lm(LIQUORD ~ INCOMED -1, data = pdata_clean)
summary(model)
confint(model, level = 0.95)