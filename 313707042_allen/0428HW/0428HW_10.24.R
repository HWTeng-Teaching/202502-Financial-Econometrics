library(POE5Rdata)
library(ggplot2)
data(mroz)
?data
married_data = mroz[mroz$lfp==1, ]
#a
iv_model <- ivreg(log(wage) ~ exper + I(exper^2) + educ | exper + I(exper^2) + mothereduc + fathereduc,
                  data=married_data  )
e_iv <- resid(iv_model)
married_data$e_iv <- e_iv

ggplot(married_data, aes(x = exper, y = e_iv)) +
  geom_point(alpha = 0.6) +
  geom_smooth(method = "loess", se = FALSE, color = "blue") +
  labs(x = "EXPER", y = "IV residuals") +
  theme_minimal()
#b
married_data$e_iv_sq <- married_data$e_iv^2
bp_model <- lm(e_iv_sq ~ exper, data = married_data)

R2 <- summary(bp_model)$r.squared
N <- nrow(married_data)

NR2 <- N * R2
p_value <- 1 - pchisq(NR2, df = 1)
cat("Breusch-Pagan test:\n")
cat("NR² =", round(NR2, 4), "\n")
cat("p-value =", round(p_value, 4), "\n")
#c
library(sandwich)  
library(lmtest)    

iv_model <- ivreg(log(wage) ~ exper + I(exper^2) + educ |
                    exper + I(exper^2) + mothereduc + fathereduc,
                  data = married_data)

robust_result <- coeftest(iv_model, vcov = vcovHC(iv_model, type = "HC1"))
print(robust_result)
b <- robust_result["educ", "Estimate"]
se <- robust_result["educ", "Std. Error"]

lower <- b - 1.96 * se
upper <- b + 1.96 * se

cat("95% confidence interval for EDUC:", round(lower, 4), "to", round(upper, 4), "\n")
#d
library(AER)
library(boot)

boot_iv <- function(data, indices) {
  d <- data[indices, ]
  fit <- ivreg(log(wage) ~ exper + I(exper^2) + educ |
                 exper + I(exper^2) + mothereduc + fathereduc,
               data = d)
  return(coef(fit)["educ"])
}


set.seed(123)  
boot_result <- boot(data = married_data, statistic = boot_iv, R = 200)

boot_se <- sd(boot_result$t)
boot_est <- mean(boot_result$t)
boot_ci <- boot.ci(boot_result, type = "norm")

cat("EDUC 係數 (bootstrap):", round(boot_est, 4), "\n")
cat("Bootstrap 標準誤:", round(boot_se, 4), "\n")
cat("95% 信賴區間 (normal method):", 
    round(boot_ci$normal[2], 4), "to", round(boot_ci$normal[3], 4), "\n")
