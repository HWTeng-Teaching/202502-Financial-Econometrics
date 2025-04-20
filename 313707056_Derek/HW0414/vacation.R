library(POE5Rdata)
library("ggplot2")
library(car)
library(tidyverse)
library(lmtest)
library(sandwich)   # 計算 robust vcov



data("vacation")
model<-lm(miles~.,data=vacation)
summary(model)
confint(model, "kids", level = 0.95)

residualPlots(model,type="response",quadratic=F)

# 依收入排序
vacation_sorted <- vacation %>% arrange(income)

# 使用前90與後90筆資料建立模型
model_low <- lm(miles ~ income + age + kids, data = vacation_sorted[1:90,])
model_high <- lm(miles ~ income + age + kids, data = vacation_sorted[111:200,])  # 中間20筆捨去

# Goldfeld–Quandt test
gqtest(model, order.by = ~income, data = vacation, fraction = 0.2)



#d
coeftest(model, vcov = vcovHC(model, type = "HC1"))
# 抓係數與 robust 標準誤
coef_kids <- coef(model)["kids"]
se_kids <- sqrt(vcovHC(model, type = "HC1")["kids", "kids"])

# 計算 95% 信賴區間
ci_lower <- coef_kids - 1.96 * se_kids
ci_upper <- coef_kids + 1.96 * se_kids
c(ci_lower, ci_upper)

#e
# 使用GLS需先建立權重向量 w_i = 1/INCOME^2
vacation$w <- 1 / (vacation$income^2)

# GLS模型
gls_model <- lm(miles ~ income + age + kids, data = vacation, weights = w)

# 95%信賴區間（GLS常規）
confint(gls_model, "kids", level = 0.95)

# GLS + robust 標準誤
coeftest(gls_model, vcov = vcovHC(gls_model, type = "HC1"))

