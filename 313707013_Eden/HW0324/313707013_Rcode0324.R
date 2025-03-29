#5.23
#b
library(POE5Rdata)
data("cocaine")
str(cocaine)
Regression <- lm(price~quant+qual+trend, data=cocaine)
summary(Regression)