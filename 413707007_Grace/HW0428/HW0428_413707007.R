#10.18
load("C:\\Users\\thaol\\Downloads\\mroz.rdata")
view(mroz)

mroz$MOTHERCOLL <- ifelse(mroz$mothereduc > 12 & mroz$lfp == 1, 1, 0)
mroz$FATHERCOLL <- ifelse(mroz$fathereduc > 12 & mroz$lfp == 1, 1, 0)

numbers_mothercoll <- sum(mroz$MOTHERCOLL)
numbers_fathercoll <- sum(mroz$FATHERCOLL)

percentage_mothercoll <- (numbers_mothercoll / 428) * 100
percentage_fathercoll <- (numbers_fathercoll / 428) * 100
print(paste("Percentage of mothers with some college education:", percentage_mothercoll, "%"))
print(paste("Percentage of fathers with some college education:", percentage_fathercoll, "%"))

correlation_matrix <- cor(cbind(mroz$educ,mroz$MOTHERCOL,mroz$FATHERCOLL))
print(correlation_matrix)
install.packages("AER")
# 2) Load it
library(AER)
# restricts sample to married women
mroz1 <- mroz[mroz$lfp==1,] 
mroz1$MOTHERCOLL <- ifelse(mroz1$mothereduc > 12, 1, 0)
wage_iv_model <- ivreg(log(wage) ~ educ + exper + I(exper^2) | MOTHERCOLL + exper + I(exper^2), data = mroz1)
educ_95percent_interval <- confint(wage_iv_model, level = 0.95)["educ",]
cat("The 95% interval estimate for the coefficient of EDUC is [",round(educ_95percent_interval,4),"]")

wage_ols <- lm(educ ~ MOTHERCOLL, data = mroz1)

f_test <- summary(wage_ols)$fstatistic[1]
cat("F-test statistic for the hypothesis that MOTHERCOLL has no effect on EDUC:", f_test, "\n")

wage_iv_model2 <- ivreg(log(wage) ~ educ + exper + I(exper^2) | MOTHERCOLL + FATHERCOLL + exper + I(exper^2), data
                  = mroz1)
conf_int_e <- confint(wage_iv_model2, level = 0.95)["educ",]
print(conf_int_e)

wage_ols2 <- lm(educ ~ MOTHERCOLL + FATHERCOLL, data = mroz1)
f_test_2 <- summary(wage_ols2)$fstatistic[1]
cat("F-test statistic for the joint significance of MOTHERCOLL and FATHERCOLL:", f_test_2, "\n")

summary_wage_iv_model2 <- summary(wage_iv_model2, diagnostics=TRUE)
# Extract the Sargan over–identification test statistic
sargan_stat <- summary_wage_iv_model2$diagnostics["Sargan", "statistic"]
quantile_value <- qchisq(0.95, df=1) 
print(quantile_value)
cat("Sargan-Hansen statistic for the validity of the surplus instrument:", sargan_stat, "\n")


#10.20
load("C:\\Users\\thaol\\Downloads\\capm5.rdata")
capm5$msftrf = (capm5$msft-capm5$riskfree)
capm5$rmrf = (capm5$mkt-capm5$riskfree)
msftrf=lm(msftrf~rmrf, data=capm5)
summary(msftrf)

capm5$rank = rank(capm5$rmrf)
rmrf_ols=lm(rmrf~rank , data=capm5)
summary(rmrf_ols)


vhat = residuals(rmrf_ols) # obtain first stage residuals
modc = lm(msftrf ~ vhat + rmrf, data = capm5)
summary(modc)

iv1 = ivreg(msftrf ~ rmrf | rank, data = capm5)
coeftest(iv1, vcov = vcovHC, type = "HC1")
confint(iv1, level = 0.95) 
summary(iv1)


capm5$POS <- ifelse(capm5$rmrf > 0, 1, 0)
mode <- lm(rmrf ~ rank + POS, data = capm5) 
summary(mode)

vhat2 <- residuals(mode)
modf <- lm(msftrf ~ rmrf + vhat2, data = capm5) 
summary(modf)


#10.24a
install.packages("dplyr")
library(dplyr)
mroz2 <- mroz %>%
  filter(!is.na(wage), wage > 0)
iv1024 <- ivreg (log(wage) ~ educ + exper + I(exper^2)| mothereduc + fathereduc + exper + I(exper^2) , data = mroz2)
ehat = resid(iv1024)
plot(mroz2$exper , ehat , xlab = "EXPER", ylab = 'IV ehat', main = 'Residual plot')


#10.24b
ehat_2 <- ehat^2
aux_reg <- lm(ehat_2 ~ mroz2$exper)
n <- length(ehat_2) 
R2 <- summary(aux_reg)$r.squared
NR2_stat <- n * R2
pchisq(NR2_stat, df = 1, lower.tail = FALSE)
p_value <- pchisq(NR2_stat, df = 1, lower.tail = FALSE) 
cat("NR2 test statistic =", NR2_stat, "\n")
cat("p-value =", p_value, "\n")

#10.24c
robust_se <- vcovHC(iv1024, type = "HC3")

beta_educ <- coef(iv1024)["educ"]
se_educ_rob <- sqrt(robust_se["educ", "educ"])
se_educ <- sqrt(vcov(iv1024)["educ", "educ"])

df <- nrow(mroz2) - length(coef(iv1024))
t_critical <- qt(0.975, df)

lower_bound_rob <- beta_educ - t_critical * se_educ_rob
upper_bound_rob <- beta_educ + t_critical * se_educ_rob
lower_bound <- beta_educ - t_critical * se_educ
upper_bound <- beta_educ + t_critical * se_educ

cat("95% CI for the coefficient of 'educ': [", lower_bound, ",", upper_bound, "]\n","95% CI for the coefficient of 'educ' with robust SE: [", lower_bound_rob, ",", upper_bound_rob, "]\n")

#10.24.d

install.packages(c("boot", "AER"))
library(boot)
library(AER)
bootstrap_function <- function(mroz2, indices) {
  data_bootstrap <- mroz2[indices, ]
  mod_bootstrap <- ivreg(log(wage) ~ exper + I(exper^2) + educ | exper + I(exper^2) + mothereduc + fathereduc, data = data_bootstrap)
  return(coef(mod_bootstrap))
}

set.seed(42)
bootstrap_results <- boot(data = mroz2, statistic = bootstrap_function, R = 200)
bootstrap_se <- apply(bootstrap_results$t, 2, sd)
