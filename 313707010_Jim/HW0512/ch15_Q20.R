library(POE5Rdata)
library(dplyr)
library(plm)

data('star')

# (a)
model_a <- lm(readscore ~ small + aide + tchexper + boy + white_asian + freelunch, data = star)
summary(model_a)

# (b)
pdata <- pdata.frame(star, index = c("schid", "id"))
model_b <- plm(readscore ~ small + aide + tchexper + boy + white_asian + freelunch,
               data = pdata, model = "within", effect = "individual")  # school FE
summary(model_b)

# (c)

model_pooled <- plm(readscore ~ small + aide + tchexper + boy + white_asian + freelunch,
                    data = pdata, model = "pooling")
pFtest(model_b, model_pooled)