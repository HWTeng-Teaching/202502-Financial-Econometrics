load("cocaine.RData")
summary(cocaine)         # 概要統計
model <- lm(price ~ quant + qual + trend, data = cocaine)
summary(model)
