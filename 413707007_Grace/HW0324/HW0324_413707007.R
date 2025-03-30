

#5.23
#a.
load("C:\\Users\\thaol\\Downloads\\cocaine.rdata")
print(cocaine)

mod <- lm(price ~ quant + qual + trend, data=cocaine)
smod <- summary(mod)
smod
