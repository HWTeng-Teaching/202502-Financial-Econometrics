if (!requireNamespace("remotes", quietly = TRUE)) install.packages("remotes")
remotes::install_github("ccolonescu/POE5Rdata", force = TRUE)

library(POE5Rdata)
data('vacation')

# (a) OLS + CI
ols_model <- lm(miles ~ income + age + kids, data = vacation)

summary(ols_model)

confint(ols_model, "kids", level = 0.95)
# (b)殘差與income、age
ols_resid <- resid(ols_model)
# 殘差 vs income
plot(vacation$income, ols_resid, main = "Residuals vs Income",
     xlab = "Income", ylab = "Residuals")
abline(h = 0, col = "red")

# 殘差 vs age
plot(vacation$age, ols_resid, main = "Residuals vs Age",
     xlab = "Age", ylab = "Residuals")
abline(h = 0, col = "red")
# (c) Goldfeld–Quandt 檢定
install.packages("lmtest")
library(lmtest)

# 按 income 排序
vacation_sorted <- vacation[order(vacation$income), ]

# Goldfeld–Quandt 測試（排除中間20個觀測值）
gqtest(miles ~ income + age + kids, order.by = ~income, data = vacation_sorted, fraction = 0.2)

# (d) 使用異質變異穩健標準誤（Robust SE）重新估計模型
install.packages("sandwich")
install.packages("lmtest")
library(sandwich)
library(lmtest)

# robust 標準誤
coeftest(ols_model, vcov = vcovHC(ols_model, type = "HC1"))

beta_kids <- coef(ols_model)["kids"]
se_kids <- sqrt(vcovHC(ols_model, type = "HC1")["kids", "kids"])
ci_robust <- beta_kids + c(-1, 1) * 1.96 * se_kids
ci_robust

# (e) GLS變異數估計
install.packages("gls")
install.packages("nlme")
library(nlme)

# 使用 GLS 估計誤差變異與 income² 成正比
gls_model <- gls(miles ~ income + age + kids,
                 data = vacation,
                 weights = varFixed(~income^2))

summary(gls_model)
gls_ci <- intervals(gls_model)

# kids 的係數區間
gls_ci$coef["kids", ]



