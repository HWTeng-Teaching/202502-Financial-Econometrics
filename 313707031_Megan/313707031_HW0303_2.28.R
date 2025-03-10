#2.28(a)
if (!requireNamespace("remotes", quietly = TRUE)) install.packages("remotes")
remotes::install_github("ccolonescu/POE5Rdata", force = TRUE)

library(POE5Rdata)
data("cps5_small")

summary(cps5_small$wage)
summary(cps5_small$educ)

hist(cps5_small$wage,
     main = "Histogram of WAGE",
     xlab = "WAGE (Hourly Rate)",
     col = "purple",
     border = "black")

hist(cps5_small$educ,
     main = "Histogram of EDUC",
     xlab = "EDUC (Years of Education)",
     col = "purple",
     border = "black")

#2.28(b)
linear_modle <- lm(cps5_small$wage~cps5_small$educ)
summary(linear_modle)

#2.28(c)
resiudals_EDUC <- resid(linear_modle)

plot(cps5_small$educ, resiudals_EDUC,
     main = "Residuals vs EDUC",
     xlab = "EDUC (Years of Education)",
     ylab = "Residuals",
     pch = 19, col = "purple")

abline(h = 0, col = "orange",lwd=5, lty = 2)

#2.28(d)
female_data <- cps5_small[cps5_small$female == 1,]
lm_female <- lm(wage~educ,data = female_data)
summary(lm_female)

male_data <- cps5_small[cps5_small$female == 0,]
lm_male <- lm(wage~educ,data = male_data)
summary(lm_male)

black_data <- cps5_small[cps5_small$black == 1,]
lm_black <- lm(wage~educ,data = black_data)
summary(lm_black)

white_data <- cps5_small[cps5_small$black == 0,]
lm_white <- lm(wage~educ,data = white_data)
summary(lm_white)

#2.28(e)
x2 <- cps5_small$educ^2
quadratic_modle <- lm(cps5_small$wage~x2)
summary(quadratic_modle)

educ_12 <- 12
educ_16 <- 16
marginal_effect_edcu12 <- 2*coef(quadratic_modle)[2]*educ_12
marginal_effect_educ16 <- 2*coef(quadratic_modle)[2]*educ_16
marginal_effect_edcu12
marginal_effect_educ16

#2.28(f)
plot(cps5_small$educ, cps5_small$wage,
     main = "Fitted Linear vs Quadratic Regression Models",
     xlab = "EDUC (Years of Education)",
     ylab = "WAGE (Hourly Wage)",
     pch = 19,
     col = "purple")

abline(linear_modle,col='pink',lwd=2.5)
curve(coef(quadratic_modle)[1] + coef(quadratic_modle)[2]*x^2,
      add = TRUE,col='orange',lwd=2.5)
