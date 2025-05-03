# 8.18
# a

load(url("https://www.principlesofeconometrics.com/poe5/data/rdata/cps5.rdata"))

cps5_male <- cps5 %>% filter(female == 0)
cps5_female <- cps5 %>% filter(female == 1)
model_male <- lm(log(wage) ~ educ + exper + I(exper^2) + metro, data = cps5_male)
model_female <- lm(log(wage) ~ educ + exper + I(exper^2) + metro, data = cps5_female)
rss_male <- sum(resid(model_male)^2)
rss_female <- sum(resid(model_female)^2)
df_male <- model_male$df.residual
df_female <- model_female$df.residual
F_stat <- (rss_female / df_female) / (rss_male / df_male)
# 0.9489479
qf(0.05/2, df_male, df_female)
# 0.9452788
qf(1-0.05/2, df_male, df_female)
# 1.058071

# Because 1.058071 > 0.9489479 > 0.9452788, we fail to reject the null hypothesis, the wage variance is the same for males and females.


# b

model <- lm(log(wage) ~ educ + exper + I(exper^2) + female + black + metro + south + midwest + west, data = cps5)
residuals_squared <- resid(model)^2
aux_model <- lm(residuals_squared ~ metro + female + black, data = cps5)
n <- nrow(cps5)
R_squared <- summary(aux_model)$r.squared
NR2 <- n * R_squared
# 23.55681
df <- length(coef(aux_model)) - 1
critical <- qchisq(0.99, df)
# 11.34487

# Because 23.55681 > 11.34487, we reject the null hypothesis, the model exist heteroskedasticity.


# c

white_test <- bptest(model, ~ educ + exper + I(exper^2) + female + black + metro + south + midwest + west + I(educ^2) + I(exper^2) + I(female^2) + I(black^2) + I(metro^2) + I(south^2) + I(midwest^2) + I(west^2), data = cps5)
print(white_test)


# Because p_value <0.05, we reject the null hypothesis, the model exist heteroskedasticity.

