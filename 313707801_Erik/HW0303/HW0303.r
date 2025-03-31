#For the first question, I couldn't load the data via the GitHub link, so I downloaded the data and loaded it from my local directory
setwd("/Users/erikparkhomenko/Library/Mobile Documents/com~apple~CloudDocs/Studies/NYCU/Financial Econometrics/Data/poe5rdata")
load("collegetown.rdata")

head(collegetown)

# Scatter Diagram of House Price vs House Size
plot(collegetown$sqft, collegetown$price,
    xlab = "House Size (hundreds of sq. ft.)",
    ylab = "House Price (thousands of dollars)",
    main = "Scatter Diagram of House Price vs House Size",
    pch = 19, col = "blue"
)

# Fit a linear regression model
model1 <- lm(formula = price ~ sqft, data = collegetown)

#summarise the regression
summary(model1)

# Add the regression line to the scatter diagram
abline(model1, col = "red", lwd = 2)

#Estimating a quadratic model
model2 <- lm(price ~ I(sqft^2), data = collegetown)
summary(model2)

#Compute how much the price of a house is expected to change when the size of the house increases by 100 sqft
alpha2 <- coef(model2)[2]
sqft_base <- 2000 #this value was adjusted for hundreds of sqft as that's how the data is presented in the data file
sqft_increment <- 100 #same as above
slope_2000 <- 2 * alpha2 * sqft_base
change_approx <- slope_2000 * sqft_increment
change_approx

# Add the quadratic regression line to the scatter diagram
sqft_seq <- seq(min(collegetown$sqft), max(collegetown$sqft), length.out = 200)
price_seq <- predict(model2, newdata = data.frame(sqft = sqft_seq))
lines(sqft_seq, price_seq, col = "green", lwd = 2)

# Add the tangent line at x = 20
x0 <- 20
price_x0 <- predict(model2, newdata = data.frame(sqft = x0))
alpha2 <- coef(model2)[2]
slope_x0 <- 2 * alpha2_tangent * x0
x_seq <- seq(min(collegetown$sqft), max(collegetown$sqft), length.out = 200)
tangent_line <- price_x0 + slope_x0 * (x_seq - x0)
lines(x_seq, tangent_line, col = "orange", lwd = 2, lty = 2)

# Extract the coefficients of the quadratic model
alpha1 <- coef(model2)[1]
alpha2 <- coef(model2)[2]
x0 <- 20

# Compute the elasticity of price with respect to house size at x = 20
price_x0 <- alpha1 + alpha2 * x0^2
slope_x0 <- 2 * alpha2 * x0
elasticity_x0 <- slope_x0 * (x0 / price_x0)

# Plotting the residuals
resid1 <- residuals(model1)
resid2 <- residuals(model2)
par(mfrow = c(1, 2))
plot(collegetown$sqft, resid1,
    main = "Linear Model Residuals",
    xlab = "SQFT",
    ylab = "Residuals",
    pch = 19, col = "blue"
)
plot(collegetown$sqft, resid2,
    main = "Quadratic Model Residuals",
    xlab = "SQFT",
    ylab = "Residuals",
    pch = 19, col = "blue"
)

#Question 25 
# Load the data
setwd("/Users/erikparkhomenko/Library/Mobile Documents/com~apple~CloudDocs/Studies/NYCU/Financial Econometrics/Data/poe5rdata")
load("cex5.rdata")

# Plot the histogram of the variable "foodaway"
hist(cex5_small$foodaway,
    main = "Histogram of Food Away from Home",
    xlab = "Food Away from Home ($)",
    ylab = "Frequency",
    col = "lightblue",
    border = "black")

# Compute the mean and median of the variable "foodaway"
mean(cex5$foodaway, na.rm = TRUE)
median(cex5$foodaway, na.rm = TRUE)
quantile(cex5$foodaway, probs = c(0.25, 0.75), na.rm = TRUE)

# Compute the mean and median of the variable "foodaway" for advanced students
adv <- subset(cex5, advanced == 1)
mean(adv$foodaway, na.rm = TRUE)
median(adv$foodaway, na.rm = TRUE)

# Compute the mean and median of the variable "foodaway" for college students
college <- subset(cex5, college == 1)
mean(college$foodaway, na.rm = TRUE)
median(college$foodaway, na.rm = TRUE)

# Compute the mean and median of the variable "foodaway" with no advanced degree or college
no_adv_college <- subset(cex5, advanced == 0 & college == 0)
mean(no_adv_college$foodaway, na.rm = TRUE)
median(no_adv_college$foodaway, na.rm = TRUE)

#Remove NA and non-positive values (log of 0 or negative values is undefined)
cex5_log <- log(cex5$foodaway[cex5$foodaway > 0])

#Construct the histogram of ln(foodaway)
hist(cex5_log,
    main = "Histogram of ln(Food Away from Home)",
    xlab = "ln(Food Away from Home)",
    ylab = "Frequency",
    col = "lightblue",
    border = "black")

#Summarise the statistics of ln(foodaway)
summary(foodaway_log)

# Estimate the regression model for ln(foodaway) on income
cex5_pos <- subset(cex5, foodaway > 0 & !is.na(income))
model_ln_foodaway <- lm(log(foodaway) ~ income, data = cex5_pos)
summary(model_ln_foodaway)

#Plot ln(foodaway) against income
plot(cex5_pos$income, log(cex5_pos$foodaway),
    xlab = "Income",
    ylab = "ln(Food Away from Home)",
    main = "ln(Food Away from Home) vs Income",
    pch = 19, col = "blue"
)

# Add the fitted regression curve
income_seq <- seq(min(cex5_pos$income, na.rm = TRUE), max(cex5_pos$income, na.rm = TRUE), length.out = 200)
predicted_log_foodaway <- predict(model_ln_foodaway, newdata = data.frame(income = income_seq))
lines(income_seq, predicted_log_foodaway, col = "red", lwd = 2)

# Calculate and plot residuals from ln(foodaway) ~ income
residuals_ln_foodaway <- resid(model_ln_foodaway)

plot(cex5_pos$income, residuals_ln_foodaway,
       main = "Residuals vs Income",
       xlab = "Income",
       ylab = "Residuals",
       pch = 19, col = "darkred")
abline(h = 0, lty = 2, col = "gray")

# Load the data for question 28
setwd("/Users/erikparkhomenko/Library/Mobile Documents/com~apple~CloudDocs/Studies/NYCU/Financial Econometrics/Data/poe5rdata")
load("cps5.rdata")

# Summarise the data for wage and education
summary(cps5$wage)
summary(cps5$educ)

hist(cps5$wage, 
    main = "Histogram of Wage", 
    xlab = "Wage", 
    col = "lightblue",
    border = "black")

hist(cps5$educ,
    main = "Histogram of Education (Years)",
    xlab = "Years of education",
    col = "orange",
    border = "black"
)

# Fit a linear regression model for wage on education
model_wage_educ <- lm(wage ~ educ, data = cps5)
summary(model_wage_educ)

#Calculate residuals
residuals_wage_educ <- resid(model_wage_educ)
#Plot residuals
plot(cps5$educ, residuals_wage_educ,
    main = "Residuals of Wage on Education",
    xlab = "Years of Education",
    ylab = "Residuals",
    pch = 19, col = "darkred"
)
abline(h = 0, lty = 2, col = "gray")

#Estimate regression for males, females, blacks and whites
model_male <- lm(wage ~ educ, data = subset(cps5, female == 0))
model_female <- lm(wage ~ educ, data = subset(cps5, female == 1))
model_black <- lm(wage ~ educ, data = subset(cps5, black == 1))
model_white <- lm(wage ~ educ, data = subset(cps5, white == 1))

summary(model_male)
summary(model_female)
summary(model_black)
summary(model_white)

#Building a quadratic regression model for wage to educ
cps5$educ2 <- cps5$educ^2
model_quad <- lm(wage ~ educ2, data = cps5)
summary(model_quad)

alpha2 <- coef(model_quad)["educ2"]
ME_12 <- 2 * alpha2 * 12
ME_16 <- 2 * alpha2 * 16

ME_12
ME_16

#Estimating for the linear model
model_wage_educ <- lm(wage ~ educ, data = cps5) #was done earlier
model_line <- coef(model_wage_educ)["educ"]
