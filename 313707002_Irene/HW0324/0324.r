remotes::install_github("ccolonescu/POE5Rdata", force = TRUE)
library(POE5Rdata)
data("cocaine")
library(ggplot2)

#5.23.b
model <- lm(price ~ quant + qual + trend, data = cocaine)
summary(model)
