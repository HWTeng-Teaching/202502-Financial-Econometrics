library(POE5Rdata)
data("cocaine", package = "POE5Rdata")

names(cocaine)

#b.

model <- lm(price ~ quant+qual+trend, data = cocaine)
summary(model)