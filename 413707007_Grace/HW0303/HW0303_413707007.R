##2.17
load("C:\\Users\\thaol\\Downloads\\collegetown.rdata")
ls()
print(collegetown)
#a. Plot house price against house size in a scatter diagram
library(ggplot2)
ggplot(collegetown, aes(x = sqft, y = price)) +
  geom_point(color = "blue") +   # Plot the data points
  labs(
    title = "Scatter Plot of House Price vs. House Size",
    x = "House Size",
    y = "House Price"
  ) +
  theme_minimal()

#b.
a_regression <- lm(price ~ sqft, data = collegetown)
summary(a_regression)

# Create the scatter plot with regression line
ggplot(collegetown, aes(x = sqft, y = price)) +
  geom_point(color = "blue") +  # Plot data points
  geom_smooth(method = "lm", se = FALSE, color = "red") +  # Add regression line
  labs(
    title = "Scatter Plot of House Price vs. House Size",
    x = "House Size",
    y = "House Price"
  ) +
  theme_minimal()

#c.
model_quad_217 <- lm(price ~  I(sqft^2), data = collegetown)
summary(model_quad_217)


#d.
ggplot(collegetown, aes(x = sqft, y = price)) +
  geom_point(color = "blue") +
  # Linear fit (red line)
  geom_smooth(method = "lm", formula = y ~ x, color = "red", se = FALSE) +
  # Quadratic fit (green line)
  geom_smooth(method = "lm", formula = y ~ x + I(x^2), color = "green", se = FALSE) +
  labs(title = "Linear vs. Quadratic Fit",
       x = "Square Feet (sqft)",
       y = "Price")

#e.
beta1 <- coef(model_quad_217)[1] # Extract coefficients
beta2 <- coef(model_quad_217)[2]

x0 <- 20 #Define the point of interest: 2,000 sq ft => SQFT = 20

P_x0 <- beta1 + beta2 * (x0^2) # Compute the predicted PRICE at x0

dP_dx <- 2 * beta2 * x0 # Derivative at x0

elasticity_x0 <- dP_dx * (x0 / P_x0) # Elasticity = (dP/dx) * (x0 / P_x0)
elasticity_x0

#f. 
res_linear <- residuals(model)
res_quad   <- residuals(model_quad_217)


ggplot(collegetown, aes(x = sqft, y = res_linear)) +
  geom_point(color = "blue") +   # Plot the data points
  labs(
    title = "Scatter Plot of Residual linear vs. House Size",
    x = "House Size",
    y = "Residual linear"
  ) +
  theme_minimal()


ggplot(collegetown, aes(x = sqft, y = res_quad)) +
  geom_point(color = "blue") +   # Plot the data points
  labs(
    title = "Scatter Plot of Residual Non-linear vs. House Size",
    x = "House Size",
    y = "Residual Non-linear"
  ) +
  theme_minimal()

#e.


SSE_linear <- sum(residuals(model)^2)
SSE_quad <- sum(residuals(model_quad_217)^2)
# Display the results
SSE_linear
SSE_quad




## 2.25
#a
load("C:\\Users\\thaol\\Downloads\\cex5_small.rdata")
ls()
print(cex5_small)
foodaway <- cex5_small$foodaway
# Generate histogram
hist(foodaway, 
     main = "Histogram of Foodaway", 
     xlab = "Foodaway", 
     col = "lightgreen")  # Correct function name
summary(foodaway)

#b
summary(subset(cex5_small,advanced == 1)$foodaway)
summary(subset(cex5_small,college == 1)$foodaway)
summary(subset(cex5_small,(advanced == 0 & college == 0))$foodaway)


#c

cex5_small$lnfoodaway <- log(cex5_small$foodaway)
cex5_small <- cex5_small[is.finite(cex5_small$lnfoodaway), ]
hist(lnfoodaway, 
     main = "Histogram of LnFoodaway", 
     xlab = "LnFoodaway", 
     col = "blue")  # Correct function name
summary(cex5_small$lnfoodaway)
length(cex5_small$lnfoodaway)


#d
d_regression <- lm(lnfoodaway ~ income, data = cex5_small)
b2 <- coef(d_regression) [[2]]
print(b2)

#e
library(ggplot2)
# Create the scatter plot with regression line
ggplot(cex5_small, aes(x = income, y = lnfoodaway)) +
  geom_point(color = "blue") +  # Plot data points
  geom_smooth(method = "lm", se = FALSE, color = "red") +  # Add regression line
  labs(
    title = "Scatter Plot of ln(foodaway) vs. Income",
    x = "Income",
    y = "ln(foodaway)"
  ) +
  theme_minimal()

#f
# Compute residuals
cex5_small$residuals <- residuals(d_regression)
cex5_small <- cex5_small[is.finite(cex5_small$residuals), ]
# Scatter plot of residuals vs. income
ggplot(cex5_small, aes(x = income, y = residuals)) +
  geom_point(color = "blue") +  # Plot data points
   labs(
    title = "Scatter Plot of Residuals vs. Income",
    x = "Income",
    y = "residuals"
  ) +
  theme_minimal()


##2.28
#a.
load("C:\\Users\\thaol\\Downloads\\cps5_small.rdata")
ls()
Wage = cps5_small$wage
Educ = cps5_small$educ
summary(Wage)
summary(Educ)
hist(Wage, 
     main = "Histogram of Wage", 
     xlab = "Wage", 
     col = "blue")  # Correct function name

hist(Educ, 
     main = "Histogram of Educ", 
     xlab = "Educ", 
     col = "blue")  # Correct function name

#b. Estimate the linear regression
b_regression <- lm(Wage ~ Educ, data = cps5_small)
summary(b_regression)

#c.
# Compute residuals
residuals_2 <- residuals(b_regression)  # Extract residuals
print(residuals_2)
# Scatter plot of residuals vs. Educ
ggplot(cps5_small, aes(x = Educ, y = residuals_2)) +
  geom_point(color = "blue") +  # Plot data points
  labs(
    title = "Scatter Plot of Residuals vs. Educ",
    x = "Educ",
    y = "residuals"
  ) +
  theme_minimal()

#d. Estimate the regression
#Filter sub-sample
female = cps5_small %>% filter(female == 1)
male = cps5_small %>% filter(female == 0)
black = cps5_small %>% filter(black == 1)
white = cps5_small %>% filter(black == 0)
#model
model_female <- lm(wage ~ educ, data = female)
model_male   <- lm(wage ~ educ, data = male)
model_black  <- lm(wage ~ educ, data = black)
model_white  <- lm(wage ~ educ, data = white)

install.packages("stargazer")
library(stargazer)
stargazer(model_female, model_male, model_black, model_white,
          type = "text",
          title = "Regression Results by Group",
          dep.var.labels = c("Wage"),        # <--- Labels the DV as "Wage"
          column.labels = c("Females", "Males", "Blacks", "Whites"))

#e. Non-linear regression
model_quad_228 <- lm(wage ~ I(educ^2), data = cps5_small)


ggplot(cps5_small, aes(x = educ, y = wage)) +
  geom_point(color = "blue") +
  # Linear fit (red line)
  geom_smooth(method = "lm", formula = y ~ x, color = "red", se = FALSE) +
  # Quadratic fit (green line)
  geom_smooth(method = "lm", formula = y ~ x + I(x^2), color = "green", se = FALSE) +
  labs(title = "Linear (Red) vs. Quadratic (Green) Fit",
       x = "Education",
       y = "Wage")
