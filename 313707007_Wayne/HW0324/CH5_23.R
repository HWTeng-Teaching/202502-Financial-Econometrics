load("cocaine.RData")
summary(cocaine)         
model <- lm(price ~ quant + qual + trend, data = cocaine)
summary(model)
