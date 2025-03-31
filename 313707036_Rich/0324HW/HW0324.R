library(POE5Rdata)
data <- POE5Rdata::cocaine
mod1 <- lm(price ~ quant+ qual + trend, data = data)
summary(mod1)
summary(mod1)$r.squared

summary(mod1)$coefficients["quant", "Pr(>|t|)"] / 2

summary(mod1)$coefficients["qual", "Pr(>|t|)"]
