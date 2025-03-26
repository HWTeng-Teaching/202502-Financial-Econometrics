#5.23
#b
data(cocaine)
str(cocaine)
tab1 <- lm(price~quant+qual+trend, data=cocaine)
summary(tab1)
