library(POE5Rdata)
data("cps5")
library(lmtest)
#a
# 建立模型
model_a <- lm(log(wage) ~ educ + exper + I(exper^2) + female + black + metro + south + midwest + west, data = cps5)

# Goldfeld–Quandt test（按 FEMALE 排序）
gqtest(model_a, order.by = ~ female, data = cps5, fraction = 0.2, alternative = "two.sided")

#b
# Step 1: OLS 取殘差平方
cps5$resid_sq <- resid(model_a)^2

# Step 2: 使用 METRO, FEMALE, BLACK 做人工回歸
aux_model <- lm(resid_sq ~ metro + female + black, data = cps5)

# Step 3: 計算 NR²
n <- nrow(cps5)
R2 <- summary(aux_model)$r.squared
LM_stat <- n * R2
LM_stat

#c
library(lmtest)

# White test，會自動將所有 RHS 變數的平方與交乘項納入
bptest(model_a, ~ educ + exper + I(exper^2) + female + black + metro + south + midwest + west, data = cps5)

#d
library(sandwich)

# Robust 標準誤
coeftest(model_a, vcov = vcovHC(model_a, type = "HC1"))

#e
# 殘差平方回歸在 METRO 和 EXPER 上
var_model <- lm(resid(model_a)^2 ~ metro + exper, data = cps5)
sigma2_hat <- fitted(var_model)

# 權重為 1 / sigma2_hat
wts <- 1 / sigma2_hat

# FGLS
fgls_model <- lm(log(wage) ~ educ + exper + I(exper^2) + female + black + metro + south + midwest + west,
                 data = cps5, weights = wts)

summary(fgls_model)
confint(fgls_model)

#f
coeftest(fgls_model, vcov = vcovHC(fgls_model, type = "HC1"))

