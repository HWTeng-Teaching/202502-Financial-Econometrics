#15.20


library(POE5Rdata)

library(dplyr)
library(lmtest)
library(sandwich)
library(broom)
library(plm)

data ("star")
summary(star)
head(star)


#a. 
readscore.pool <- lm(readscore ~ small + aide + tchexper + boy + white_asian + freelunch, data = star )
summary ( readscore.pool)


#b.

# 只用 SCHID 作為固定效果（學校固定效果）
pdata <- pdata.frame(star, index = "schid")

# 建立 school fixed effects 模型
fe_model <- plm(readscore ~ small + aide + tchexper + boy + white_asian + freelunch,
                data = pdata, model = "within", effect = "individual")

# 結果摘要
summary(fe_model)







#c.
# OLS 模型（無固定效果）
ols_model <- lm(readscore ~ small + aide + tchexper + boy + white_asian + freelunch, data = star)

# F 檢定：固定效果是否顯著
pFtest(fe_model, ols_model)





