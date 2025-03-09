# Download and load the dataset
url <- "http://www.principlesofeconometrics.com/poe5/data/rdata/cps5_small.rdata"
file_path <- "cps5_small.rdata"
download.file(url, file_path, mode = "wb")
load(file_path)
ls()
head(cps5_small)

## 2.28.1 Histogram
summary_wage <- summary(cps5_small$wage)
summary_educ <- summary(cps5_small$educ)
cat("Summary statistics for WAGE:\n")
print(summary_wage)
cat("\nSummary statistics for EDUC:\n")
print(summary_educ)

hist(cps5_small$wage, 
     main = "Histogram of WAGE", 
     xlab = "Wage", 
     col = "lightblue", 
     border = "black", 
     breaks = 100)

hist(cps5_small$educ, 
     main = "Histogram of EDUC", 
     xlab = "Years of Education", 
     col = "lightblue", 
     border = "black", 
     breaks = 100)

## 2.28.2 Regression
wage_model <- lm(wage ~ educ, data = cps5_small)
summary(wage_model)
slope_estimate <- coef(wage_model)["educ"]
cat("Estimated slope (β2):", slope_estimate, "\n")

## 2.28.3 residuals
cps5_small$residuals <- resid(wage_model)

plot(cps5_small$educ, cps5_small$residuals,
     main = "Residuals vs EDUC",
     xlab = "Years of Education",
     ylab = "Residuals",
     col = "blue",
     pch = 16,
     cex = 0.7)
abline(h = 0, col = "red", lwd = 2)

summary(cps5_small$residuals)

## 2.28.4 Compare the regression on 4 sections
male_model <- lm(wage ~ educ, data = cps5_small[cps5_small$female == 0, ])
summary_male <- summary(male_model)
female_model <- lm(wage ~ educ, data = cps5_small[cps5_small$female == 1, ])
summary_female <- summary(female_model)
black_model <- lm(wage ~ educ, data = cps5_small[cps5_small$black == 1, ])
summary_black <- summary(black_model)
white_model <- lm(wage ~ educ, data = cps5_small[cps5_small$black == 0, ])
summary_white <- summary(white_model)

cat("\nRegression for Males:\n")
print(summary_male)
cat("\nRegression for Females:\n")
print(summary_female)
cat("\nRegression for Blacks:\n")
print(summary_black)
cat("\nRegression for Whites:\n")
print(summary_white)

## 2.28.5 EDUC^2
cps5_small$educ2 <- cps5_small$educ^2
quad_model <- lm(wage ~ educ2, data = cps5_small)
summary(quad_model)
alpha2 <- coef(quad_model)["educ2"]
marginal_effect_12 <- 2 * alpha2 * 12
marginal_effect_16 <- 2 * alpha2 * 16
cat("Marginal effect at EDUC = 12:", marginal_effect_12, "\n")
cat("Marginal effect at EDUC = 16:", marginal_effect_16, "\n")

## 2.28.6
cps5_small$educ2 <- cps5_small$educ^2
linear_model <- lm(wage ~ educ, data = cps5_small)
quad_model <- lm(wage ~ educ + educ2, data = cps5_small)
educ_range <- seq(min(cps5_small$educ), max(cps5_small$educ), length.out = 100)
fitted_linear <- predict(linear_model, newdata = data.frame(educ = educ_range))
fitted_quad <- predict(quad_model, newdata = data.frame(educ = educ_range, educ2 = educ_range^2))

plot(cps5_small$educ, cps5_small$wage, 
     main = "Comparison of Linear and Quadratic Models",
     xlab = "Years of Education",
     ylab = "Wage",
     col = "blue",
     pch = 16,
     cex = 0.7)

lines(educ_range, fitted_linear, col = "red", lwd = 2, lty = 2) 
lines(educ_range, fitted_quad, col = "green", lwd = 2)  
legend("topleft", legend = c("Linear Fit", "Quadratic Fit"), 
       col = c("red", "green"), lwd = 2, lty = c(2, 1))

