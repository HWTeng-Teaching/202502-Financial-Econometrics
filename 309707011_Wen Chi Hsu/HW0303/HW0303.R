#2.17
remotes::install_github("ccolonescu/POE5Rdata", force = TRUE)
library(POE5Rdata)
data("collegetown")

#a #b 
mod1 <- lm(price~sqft, data = collegetown)
summary(mod1)
plot(collegetown$sqft, collegetown$price, xlab = "SQFT", ylab = "Price", pch = 18, col = "blue", main = "Linear Regression Model")
abline(mod1, col = "red")

#c 
mod2 <- lm(price~I(sqft^2), data = collegetown)
summary(mod2)
b2 <- coef(mod2)[2]
sqft_2000 = 20
marginal_effect <- 2*b2*sqft_2000
marginal_effect

#d
plot(collegetown$sqft, collegetown$price, xlab = "SQFT", 
     ylab = "Price", col = "blue", 
     main = "Quadratic Regression Model", pch = 18)

quad_fun <- function(x){
  y=coef(mod2)[1]+coef(mod2)[2]*x^2
  return(y)
}
curve(quad_fun, col = "black",add = TRUE,lwd=1.5)

#tangent line
price_2000 <- coef(mod2)[1] + coef(mod2)[2] * sqft_2000^2
slope_2000 <- 2 * b2 * sqft_2000
sqft_tangent <- seq(sqft_2000 - 15, sqft_2000 + 30, length.out = 100)
price_tangent <- price_2000 + slope_2000 * (sqft_tangent - sqft_2000)
lines(sqft_tangent, price_tangent, col = "green", lwd = 2, lty = 2)
points(sqft_2000, price_2000, col = "red", pch = 4, cex = 1.5)

#e 
elasticity <- (marginal_effect * sqft_2000) / price_2000
elasticity

#f #LSE
residuals_lin <- resid(mod1)
residuals_quad <- resid(mod2)

plot(collegetown$sqft, residuals_lin, 
     xlab = "SQFT", 
     ylab = "Residual", 
     main = "Linear Residual",
     pch = 18, col = "blue")

plot(collegetown$sqft, residuals_quad, 
     xlab = "SQFT", 
     ylab = "Residual", 
     main = "Quadratic Residual", 
     pch = 18, col = "lightblue")

#g
SSE_lin <- sum(residuals_lin^2)
SSE_quad <- sum(residuals_quad^2)

SSE_lin
SSE_quad

#2.25
#a
data("cex5_small")
install.packages("ggplot2")
install.packages("dplyr")

ggplot(cex5_small, aes(x = foodaway, y = after_stat(count) / sum(after_stat(count)) * 100)) +
  geom_histogram(binwidth = 50, fill = "lightblue", color = "black", alpha = 0.7) +
  labs(title = "FOODAWAY Expenditure",
       x = "FOODAWAY (Dollars per Month per Person)",
       y = "Percentage",) +  
  scale_y_continuous(labels = scales::percent_format(scale = 1.5)) +  
  theme_minimal()

summary(foodaway)

#b
num_adv <- sum(cex5_small$advanced == 1, na.rm = TRUE)
mean_adv <- mean(cex5_small$foodaway[cex5_small$advanced == 1], na.rm = TRUE)
median_adv <- median(cex5_small$foodaway[cex5_small$advanced == 1], na.rm = TRUE)

num_col <- sum(cex5_small$college == 1, na.rm = TRUE)
mean_col <- mean(cex5_small$foodaway[cex5_small$college == 1], na.rm = TRUE)
median_col <- median(cex5_small$foodaway[cex5_small$college == 1], na.rm = TRUE)

num_none <- sum(cex5_small$advanced == 0 & cex5_small$college == 0, na.rm = TRUE)
mean_none <- mean(cex5_small$foodaway[cex5_small$advanced == 0 & cex5_small$college == 0], na.rm = TRUE)
median_none <- median(cex5_small$foodaway[cex5_small$advanced == 0 & cex5_small$college == 0], na.rm = TRUE)

cat("With advanced degree mean:", mean_adv, "Median:", median_adv, "N:" , num_adv, "\n")
cat("With college degree mean:", mean_col, "Median:", median_col, "N:" , num_col, "\n")
cat("No advanced or college degree mean:", mean_none, "Median:", median_none, "N:" , num_none, "\n")

#c
clean <- cex5_small[cex5_small$foodaway > 0, ]
clean$log_foodaway <- log(clean$foodaway)

hist(clean$log_foodaway, xlab = "ln(FOODAWAY)", col = "lightgreen", border = "black", 
     breaks = 30)

summary(clean$log_foodaway)

total_obs <- length(cex5_small$foodaway)
clean_obs <- sum(cex5_small$foodaway > 0, na.rm = TRUE)
cat(" Observations of FOODAWAY:", total_obs, "\n",
    "Observations of ln(FOODAWAY): ", clean_obs, "\n")

#d
mod3 <- lm(log_foodaway ~ income, data = clean)
summary(mod3)

#e
plot(clean$income, clean$log_foodaway, 
     xlab = "INCOME", ylab = "ln(FOODAWAY)", 
     pch = 18, col = "blue")
abline(mod3, col = "red")

#f
residuals <- resid(mod3)
plot(clean$income, residuals, 
     xlab = "INCOME", ylab = "Residuals", 
     pch = 18, col = "blue")

#2.28
#a
data("cps5_small")
summary(cps5_small$wage)
summary(cps5_small$educ)

hist(cps5_small$wage, col = "lightblue", border = "black", main = "Wage",
     xlab = "Hourly Wage", ylab = "Frequency", breaks = 30)

hist(cps5_small$educ, col = "lightgreen", border = "black", main = "Education",
     xlab = "Years of Education", ylab = "Frequency", breaks = 15)

#b
mod4 <- lm(wage~educ, data = cps5_small)
summary(mod4)

#c
resid4 <- resid(mod4)
plot(cps5_small$educ, resid4, 
     xlab = "Education", 
     ylab = "Residual", 
     pch = 18, col = "blue")

#d
model_male <- lm(wage ~ educ, data = cps5_small, subset = (female == 0))
summary(model_male)

model_female <- lm(wage ~ educ, data = cps5_small, subset = (female == 1))
summary(model_female)

model_black <- lm(wage ~ educ, data = cps5_small, subset = (black == 1))
summary(model_black)

model_white <- lm(wage ~ educ, data = cps5_small, subset = (black == 0))
summary(model_white)

#e
cps5_small$educ2 <- cps5_small$educ^2 
mod5 <- lm(wage ~ educ2, data = cps5_small)
summary(mod5)

alp2 <- coef(mod5)["educ2"]

marginal_effect_12 <- 2 * alp2 * 12
marginal_effect_16 <- 2 * alp2 * 16
linear_effect <- coef(mod4)[2]

cat("Marginal effect of EDUC = 12 :", marginal_effect_12, "\n")
cat("Marginal effect of EDUC = 16 :", marginal_effect_16, "\n")
cat("Marginal effect of linear regression :", linear_effect, "\n")

#f
plot(cps5_small$educ, cps5_small$wage, col = "blue", pch = 20, 
     main = "Wage vs. Education", xlab = "Years of Education", ylab = "Hourly Wage")
abline(mod4, col = "red", lwd = 2, lty = 2)

quad_mod5 <- function(x){
  y=coef(mod5)[1]+coef(mod5)[2]*x^2
  return(y)
}
curve(quad_mod5, col = "green",add = TRUE,lwd=1.5)
