library(POE5Rdata)
data("cocaine")

#5.23.b
model <- lm(price ~ quant + qual + trend, data = cocaine)
summary(model)

qt(0.05,52)
