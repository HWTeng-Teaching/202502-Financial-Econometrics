library(POE5Rdata)
data("mroz", package = "POE5Rdata")

names(mroz)

#a.

# 套件與資料
library(AER)
data("mroz", package = "POE5Rdata")

# 建立工作樣本
working_married <- subset(mroz, lfp == 1)
working_married$logwage <- log(working_married$wage)

# 執行 2SLS（IV）估計
iv_model <- ivreg(logwage ~ educ + exper + I(exper^2) | mothereduc + fathereduc + exper + I(exper^2), data = working_married)

# 取出殘差
working_married$resid_iv <- resid(iv_model)

# 繪圖殘差 vs. exper
plot(working_married$exper, working_married$resid_iv,
     xlab = "exper", ylab = "IV residuals",
     main = "Residuals vs. exper")
abline(h = 0, col = "red", lty = 2)

#a.ans
#The residual plot shows signs of heteroskedasticity, as the variance of residuals changes with exper. Therefore, the assumption of homoskedasticity may not hold.

#b.

iv_data <- model.frame(iv_model)

iv_data$resid_sq <- resid(iv_model)^2

bp_model <- lm(resid_sq ~ exper, data = iv_data)

summary(bp_model)

n <- nrow(iv_data)
NR_stat <- n * R2

df <- 1  
p_value <- 1 - pchisq(NR_stat, df)

cat("NR test stat =", NR_stat, "\n")
cat("p-value =", p_value, "\n")

#b.ans
#NR test stat = 7.187274,greater than critical value.we reject the null hypothesis of homoskedasticity. T

#c.

library(sandwich)
library(lmtest)

robust_se <- coeftest(iv_model, vcov. = vcovHC(iv_model, type = "HC1"))

print(robust_se)

educ_coef <- robust_se["educ", "Estimate"]
educ_se <- robust_se["educ", "Std. Error"]

lower <- educ_coef - 1.96 * educ_se
upper <- educ_coef + 1.96 * educ_se

cat("95% Confidence Interval for 'educ' with robust SE: [", round(lower, 4), ",", round(upper, 4), "]\n")

#c.ans
#[ -0.0039 , 0.1267 ] larger

#d.

library(boot)

boot_iv <- function(data, indices) {
  d <- data[indices, ]
  model <- ivreg(logwage ~ educ + exper + I(exper^2) |
                   mothereduc + fathereduc + exper + I(exper^2), data = d)
  return(coef(model)["educ"])
}

set.seed(123)  
boot_result <- boot(data = working_married, statistic = boot_iv, R = 200)

print(boot_result)

boot_se <- sd(boot_result$t)
educ_coef_boot <- coef(iv_model)["educ"]

ci_lower <- educ_coef_boot - 1.96 * boot_se
ci_upper <- educ_coef_boot + 1.96 * boot_se

cat("Bootstrap SE for 'educ':", round(boot_se, 4), "\n")
cat("95% Confidence Interval for 'educ' (Bootstrap): [",
    round(ci_lower, 4), ",", round(ci_upper, 4), "]\n")

#d.ans
#[ -0.002 , 0.1248 ]slightly smaller than (c),larger than usual iv.
