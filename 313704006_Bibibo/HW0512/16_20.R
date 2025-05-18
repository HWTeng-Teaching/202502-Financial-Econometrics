library(POE5Rdata)
data(star)
#a
ols = lm(readscore ~ small + aide + tchexper + boy+
         white_asian + freelunch, data = star)
summary(ols)

#b
#install.packages("plm")
library(plm)
wage_fixed = lm(readscore ~ small + aide + tchexper + boy+
                  white_asian + freelunch + factor(schid) - 1, data = star)
summary(wage_fixed)

#c
model_fe <- plm(readscore ~ small + aide + tchexper + boy + 
                white_asian + freelunch, data = star,
                model = "within",
                index = c("schid", "id"))
summary(model_fe)
ols_pooling <- plm(readscore ~ small + aide + tchexper + boy + 
                   white_asian + freelunch, data = star,
                   model = "pooling")
pFtest(model_fe, ols_pooling)
