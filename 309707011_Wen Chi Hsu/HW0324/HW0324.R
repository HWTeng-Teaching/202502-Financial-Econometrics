remotes::install_github("ccolonescu/POE5Rdata", force = TRUE)
library(POE5Rdata)
data("cocaine")

#b
model <- lm(price ~ quant + qual + trend, data = cocaine)
summary(model)

#d
qt(0.05, df = 54)

#e
qt(0.95, df = 54)
