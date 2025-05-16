# 15.20
library(POE5Rdata)
data(star) 
install.packages("plm")
library(plm)

# (a)
model_a <- lm(readscore ~ small + aide + tchexper + boy + white_asian + freelunch, data = star)
summary(model_a)

# (b) fixed effects regression
pdata <- pdata.frame(star, index = c("schid", "id"))
model_b <- plm(readscore ~ small + aide + tchexper + boy + white_asian + freelunch, data = pdata,
               model = "within",  # fixed effects
               effect = "individual")  # fixed SCHID
summary(model_b)

# (c)
pFtest(model_b, model_a)