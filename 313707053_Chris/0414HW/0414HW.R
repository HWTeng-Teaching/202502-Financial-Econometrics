library(POE5Rdata)
POE5Rdata::vacation	

#(a)
model <- lm(miles ~ income + age + kids, data = vacation)
summary(model)
confint(model, level = 0.95)

#(b)
plot(vacation$income, residuals(model), main = "Residuals vs INCOME",
     xlab = "Income",ylab = "Vacation miles",pch = 16,col = "red")
plot(vacation$age, residuals(model), main = "Residuals vs AGE",
     xlab = "Age",ylab = "Vacation miles"
     ,pch = 16,col = "blue")

#(c)
vacation_data_sorted <- vacation[order(vacation$income),]

group1 <- vacation_data_sorted[1:90, ]
group2 <- vacation_data_sorted[101:200, ]

model_group1 <- lm(miles ~ income + age + kids, data = group1)
model_group2 <- lm(miles ~ income + age + kids, data = group2)

RSS1 <- sum(resid(model_group1)^2)
RSS2 <- sum(resid(model_group2)^2)


n1 <- nrow(group1)
n2 <- nrow(group2)
k <- length(coef(model_group1))  

F_stat <- (RSS1 / (n1 - k)) / (RSS2 / (n2 - k))

df1 <- n1 - k
df2 <- n2 - k
alpha <- 0.05
F_critical <- qf(1 - alpha/2, df1, df2)

if (F_stat > F_critical) {
  cat("Reject H0: Heteroskedasticity is present.")
} else {
  cat("Fail to reject H0: No evidence of heteroskedasticity.")
}


#(d)
install.packages("sandwich") 
install.packages("lmtest")
library(lmtest)
library(sandwich) 
model <- lm(miles ~ income + age + kids, data = vacation)
robust_se <- sqrt(diag(vcovHC(model, type = "HC1")))
summary(model, robust = robust_se)
beta_kids <- coef(model)["kids"]
ci_lower <- beta_kids - 1.96 * robust_se["kids"]
ci_upper <- beta_kids + 1.96 * robust_se["kids"]

c(ci_lower, ci_upper)


#(e)
install.packages("nlme")
library(nlme)
gls_model_conventional <- gls(miles ~ income + age + kids, data = vacation,
                              weights = varPower(form = ~ income^2))  
summary(gls_model_conventional)
gls_model_robust <- gls(miles ~ income + age + kids, data = vacation,
                        weights = varPower(form = ~ income^2), method = "ML")  # Maximum likelihood estimation

summary(gls_model_robust)
beta_kids_conventional <- coef(gls_model_conventional)["kids"]
se_kids_conventional <- sqrt(diag(vcov(gls_model_conventional))["kids"])

ci_lower_conventional <- beta_kids_conventional - 1.96 * se_kids_conventional
ci_upper_conventional <- beta_kids_conventional + 1.96 * se_kids_conventional


beta_kids_robust <- coef(gls_model_robust)["kids"]
se_kids_robust <- sqrt(diag(vcov(gls_model_robust))["kids"])

ci_lower_robust <- beta_kids_robust - 1.96 * se_kids_robust
ci_upper_robust <- beta_kids_robust + 1.96 * se_kids_robust

c(conventional_CI = c(ci_lower_conventional, ci_upper_conventional),
  robust_CI = c(ci_lower_robust, ci_upper_robust))




#8.18
#(a)
POE5Rdata::cps5
group_male <- cps5[cps5$female == 0, ]
group_female <- cps5[cps5$female == 1, ]


model_male <- lm(log(wage) ~ educ + exper + I(exper^2) + female + black + metro + south + midwest + west, data = group_male)
model_female <- lm(log(wage) ~ educ + exper + I(exper^2) + female + black + metro + south + midwest + west, data = group_female)

RSS_male <- sum(resid(model_male)^2)
RSS_female <- sum(resid(model_female)^2)


n_male <- nrow(group_male)
n_female <- nrow(group_female)
k <- length(coef(model_male))  

F_stat <- (RSS_male / (n_male - k)) / (RSS_female / (n_female - k))


F_critical <- qf(0.95, df1 = n_male - k, df2 = n_female - k)


F_stat
F_critical
if (F_stat > F_critical) {
  print("Reject H0: Heteroskedasticity is present.")
} else {
  print("Fail to reject H0: No evidence of heteroskedasticity.")
}

#(b)

model_ols_b <- lm(log(wage) ~ educ + exper + I(exper^2) + female + black + metro + south + midwest + west, data = cps5)
bptest(model_ols_b, ~ metro + female + black, data = cps5)

#(c)

white_test <- bptest(model_ols_b, studentize = TRUE) 


df <- length(coef(model_ols_b)) - 1  
critical_value <- qchisq(0.95, df)

white_test_stat <- white_test$statistic
white_test_stat
critical_value

if (white_test_stat > critical_value) {
  cat("Reject H0: Heteroskedasticity is present.")
} else {
  cat("Fail to reject H0: No evidence of heteroskedasticity.")
}

#(d)

model_ols_white <- lm(log(wage) ~ educ + exper + I(exper^2) + female + black + metro + south + midwest + west, data = cps5)

robust_se_white <- sqrt(diag(vcovHC(model_ols_white, type = "HC1")))

summary(model_ols_white)
confint(model_ols_white, level = 0.95)

#傳統
model_ols <- lm(log(wage) ~ educ + exper + I(exper^2) + female + black + metro + south + midwest + west, data = cps5)
summary(model_ols)
ols_se <- summary(model_ols)$coefficients[, "Std. Error"]
ci_lower <- coef(model_ols) - 1.96 * ols_se
ci_upper <- coef(model_ols) + 1.96 * ols_se
data.frame(Coefficients = coef(model_ols), Lower_CI = ci_lower, Upper_CI = ci_upper)


#(e)

gls_model_fgls <- gls(log(wage) ~ educ + exper + I(exper^2) + female + black + metro + south + midwest + west, data = cps5, weights = varPower(form = ~ wage^2))

summary(gls_model_fgls)
confint(gls_model_fgls, level = 0.95)

#(f)
gls_model_fgls_robust <- gls(log(wage) ~ educ + exper + I(exper^2) + female + black + metro + south + midwest + west, data = cps5, weights = varPower(form = ~ wage^2), method = "ML")
summary(gls_model_fgls_robust)
confint(gls_model_fgls_robust, level = 0.95)
