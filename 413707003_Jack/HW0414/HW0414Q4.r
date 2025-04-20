# 8.16
# a

load(url("https://www.principlesofeconometrics.com/poe5/data/rdata/vacation.rdata"))

model <- lm(miles ~ income + age + kids, data=vacation)
summary(model)

confint(model, level = 0.95)["kids",]
#        2.5 %     97.5 % 
#   -135.32981  -28.32302


# b

residuals <- resid(model)

plot(vacation$income, residuals, main="residuals vs INCOME", xlab="INCOME", ylab="residuals")
abline(h=0, col="red")

plot(vacation$age, residuals, main="residuals vs AGE", xlab="AGE", ylab="residuals")
abline(h=0, col="red")

# It seems that as income increases, the residuals also become larger.


# c

vacation_sorted <- vacation[order(vacation$income), ]

low_income <- vacation_sorted[1:90, ]
high_income <- vacation_sorted[111:200, ]

model_low <- lm(miles ~ income + age + kids, data=low_income)
model_high <- lm(miles ~ income + age + kids, data=high_income)

rss_low <- sum(resid(model_low)^2)
rss_high <- sum(resid(model_high)^2)

df_low <- nrow(low_income) - length(coef(model_low))
df_high <- nrow(high_income) - length(coef(model_high))

F_stat <- (rss_high / df_high) / (rss_low / df_low)
# 3.104061

alpha <- 0.05
F_crit <- qf(1 - alpha, df_high, df_low)
# 1.428617

# Because 3.104061 > 1.428617, we can reject the null hypothesis, there is heteroscedasticity across different income levels.


