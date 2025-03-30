library(POE5Rdata)
data("cocaine")

#b.
model <- lm(price ~ quant + qual + trend, data = cocaine)

summary(model)

