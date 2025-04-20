Fc <- qf(0.025,3257,3256)
print(Fc)
qchisq(0.95,14)

url <- "https://www.principlesofeconometrics.com/poe5/data/ascii/vacation.dat"
download.file(url, destfile = "vacation.dat")
vacation <- read.table("vacation.dat", header = FALSE)
colnames(vacation) <- c("MILES","INCOME", "AGE", "KIDS")
head(vacation)  # 查看前幾行

##a.
# OLS estimation
model_ols <- lm(MILES ~ INCOME + AGE + KIDS, data = vacation)
summary(model_ols)
confint(model_ols, "KIDS", level = 0.95)

##b.
# 取得殘差
residuals_ols <- resid(model_ols)

plot(vacation$INCOME, residuals_ols,
     main = "Residuals vs INCOME",
     xlab = "INCOME", ylab = "Residuals")
abline(h = 3, col = "pink")

plot(vacation$AGE, residuals_ols,
     main = "Residuals vs AGE",
     xlab = "AGE", ylab = "Residuals")
abline(h = 2, col = "lightblue")


##c
library(lmtest)

# 按INCOME排序
vacation_sorted <- vacation[order(vacation$INCOME), ]

# Goldfeld–Quandt test (排除中間20筆，使用兩邊各90筆)
gq_test <- gqtest(MILES ~ INCOME + AGE + KIDS, order.by = ~INCOME, data = vacation_sorted, fraction = 0.1)

# 顯示結果
print(gq_test)

##d.
install.packages("sandwich")
install.packages("lmtest")
library(sandwich)
library(lmtest)

# 取得robust標準誤
robust_se <- coeftest(model_ols, vcov = vcovHC(model_ols, type = "HC1"))

# 顯示robust估計結果
print(robust_se)

# 自行計算信賴區間 for KIDS
kid_coef <- coef(model_ols)["KIDS"]
kid_se_robust <- sqrt(vcovHC(model_ols, type = "HC1")["KIDS", "KIDS"])
ci_lower <- kid_coef - 1.96 * kid_se_robust
ci_upper <- kid_coef + 1.96 * kid_se_robust
c(ci_lower, ci_upper)


##e.
# 加入權重 = 1 / INCOME^2
model_gls <- lm(MILES ~ INCOME + AGE + KIDS, data = vacation, weights = 1 / (INCOME^2))
summary(model_gls)

confint(model_gls, "KIDS", level = 0.95)

# 若要使用 robust GLS 標準誤
robust_gls_se <- coeftest(model_gls, vcov = vcovHC(model_gls, type = "HC1"))
print(robust_gls_se)
kid_coef <- coef(model_gls)["KIDS"]
robust_gls_se <- sqrt(vcovHC(model_gls, type = "HC1")["KIDS", "KIDS"])
ci_lower <- kid_coef - 1.96 * robust_gls_se
ci_upper <- kid_coef + 1.96 * robust_gls_se
c(ci_lower, ci_upper)
