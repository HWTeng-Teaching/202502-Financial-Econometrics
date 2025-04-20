#8.18
# 313707014 陳紀蓁

library(POE5Rdata)
library(tidyverse)
library(sandwich)
library(lmtest)
library(sandwich)
library(car)

data ("cps5")
summary(cps5)
head(cps5)

#a. 
mod1 <- lm(log(wage) ~ educ + exper + I(exper^2) + female + black + metro + south + midwest + west, data=cps5)
smod1 <- data.frame(xtable(summary(mod1)))
kable(smod1, 
      caption="The basic multiple regression model", 
      col.names=c("coefficient", "Std. Error", "t-value", "p-value"),
      align="c", digits=3)

s <- summary(mod1)
s


# Goldfeld–Quandt test（使用 car 套件）
gqtest(mod1, order.by = ~female, data = cps5)


#b.
#Breusch–Pagan Test
bptest(mod1, varformula = ~ metro + female + black, data = cps5)

bptest(mod1, varformula = ~ educ + exper + I(exper^2) + female + black +
         metro + south + midwest + west, data = cps5)


#c. 
#White test for heteroskedasticity

bptest(mod1, ~ (educ + exper + I(exper^2) + female + black +
         metro + south + midwest + west)^2, data = cps5)

#d.

ols <- summary(mod1)
ols


ols_R <- coeftest(mod1, vcov = vcovHC(mod1, type = "HC1"))
ols_R


#e. FGLS（可行廣義最小平方法）估計模型

# 取得殘差平方
resid_sq <- resid(mod1)^2
resid_sq

var_model <- lm(log(resid_sq) ~ metro + exper, data = cps5)

log_var_hat <- predict(var_model)
weights <- 1 / exp(log_var_hat)

# 第五步：進行 WLS（FGLS）

fgls_model <- lm(log(wage) ~ educ + exper + I(exper^2) + female + black + metro + south + midwest + west, data = cps5, weights = weights)

# 查看 FGLS 結果（含標準誤與信賴區間）
summary(fgls_model)

# 加上 95% 區間估計
confint(fgls_model)


# f
fgls_robust <- coeftest(fgls_model, vcov = vcovHC(fgls_model, type = "HC1"))

print(fgls_robust)









