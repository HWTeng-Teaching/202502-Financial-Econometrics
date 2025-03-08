# c 

residuals <- resid(linear_model)

plot(cps5_small$educ, residuals, main="Residuals vs EDUC", xlab="EDUC", ylab="Residuals")
abline(h=0, col="red")

# It seems that when EDUC is smaller, the distribution of residuals is predominantly positive.


# d

male_model <- lm(wage ~ educ, data=cps5_small, subset=(female == 0))
female_model <- lm(wage ~ educ, data=cps5_small, subset=(female == 1))


black_model <- lm(wage ~ educ, data=cps5_small, subset=(black == 1))
white_model <- lm(wage ~ educ, data=cps5_small, subset=(black == 0))

summary(male_model)
summary(female_model)
summary(black_model)
summary(white_model)

coeff_table <- data.frame(
  Group = c("Male", "Female", "Black", "White"),
  Beta1 = sapply(list(male_model, female_model, black_model, white_model), function(model) coef(model)[1]),
  Beta2 = sapply(list(male_model, female_model, black_model, white_model), function(model) coef(model)[2])
)

print(coeff_table)


# Group      Beta1    Beta2
# 1   Male  -8.284936 2.378471
# 2 Female -16.602785 2.659487
# 3  Black  -6.254114 1.923330
# 4  White -10.474710 2.417769

#Compared to males, females have a lower wage when EDUC = 0. Similarly, compared to blacks, whites also have a lower wage at EDUC = 0. However, both groups have a higher beta_2, meaning that for each additional unit of EDUC, their wage increases at a higher rate.

