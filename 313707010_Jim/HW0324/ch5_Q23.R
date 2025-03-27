library(POE5Rdata)
data("cocaine")

model <- lm(price ~ quant + qual + trend, data = cocaine)

summary(model)