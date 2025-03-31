install.packages("POE5Rdata") 
library(POE5Rdata)  
data(cocaine)

#b
model = lm(price ~quant + qual + trend, data = cocaine)
summary(model)

qt(0.05, df = 54)
qt(0.95, df = 54)
