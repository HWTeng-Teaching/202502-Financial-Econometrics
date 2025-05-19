library(POE5Rdata)
data(star)

library(plm)
library(lme4)
library(dplyr)
library(wooldridge)

data("star", package = "wooldridge")

# (a)
model_ols <- lm(readscore ~ small + aide + tchexper + boy + white_asian + freelunch, data = star)
cat("\n(a) OLS 模型結果：\n")
print(summary(model_ols))

# (b) 
star$stid <- star$id
star$id <- NULL 
pdata <- pdata.frame(star, index = c("schid", "stid"))

fe_school <- plm(readscore ~ small + aide + tchexper + boy + white_asian + freelunch,
                 data = pdata,
                 model = "within",
                 effect = "individual")
cat("\n固定效果模型結果：\n")
print(summary(fe_school))


# (c) 
f_test <- pFtest(fe_model, model_ols)
cat("\n(c) 固定效果顯著性檢定：\n")
print(f_test)

