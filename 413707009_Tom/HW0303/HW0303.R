#02.17

library(readxl)

collegetown <- read_excel("/Users/tom/Documents/NYCU/Financial econometrics/poe5xlsx/collegetown.xlsx")

# a. Create a scatter plot of house size (SQFT) versus sale price (PRICE)
plot(collegetown$sqft, collegetown$price,
     xlab = "House Size (hundreds of sq ft)",
     ylab = "Sale Price (thousands of dollars)",
     main = "Scatterplot of House Price vs. House Size",
     pch = 19,        # Use solid circles for points
     col = "blue")    # Color the points blue


# b. Estimate the linear regression model PRICE = β1 + β2SQFT + e. Interpret the estimates. Draw a sketch of the fitted line.
model <- lm(price ~ sqft, data = collegetown)

# Display the summary of the regression model to see the estimates and other statistics
summary(model)
# Plot the data
plot(collegetown$sqft, collegetown$price,
     xlab = "House Size (hundreds of square feet)",
     ylab = "Sale Price (thousands of dollars)",
     main = "House Price vs. House Size with Fitted Regression Line",
     pch = 19,       # solid circle for points
     col = "blue")   # points colored blue

# Add the fitted regression line to the scatter plot
abline(model, col = "red", lwd = 2)


# c.
model_quad <- lm(price ~ I(sqft^2), data = collegetown)

# Display the summary of the model
summary(model_quad)

# Extract alpha2 from the fitted model
alpha2 <- coef(model_quad)["I(sqft^2)"]  # or coef(model_quad)[2]

# SQFT value for a 2,000 sq ft house
SQFT_2000 <- 20

# Marginal effect at 2,000 sq ft (20 in the 'SQFT' variable)
marginal_effect_2000 <- 2 * alpha2 * SQFT_2000

# Print the marginal effect (in thousands of dollars)
marginal_effect_2000


#d.
# Scatterplot of the original data
model_quad <- lm(price ~ I(sqft^2), data = collegetown)

# Display summary for estimates
summary(model_quad)
plot(collegetown$sqft, collegetown$price,
     xlab = "House Size (hundreds of sq ft)",
     ylab = "Sale Price (thousands of dollars)",
     main = "Quadratic Fit: PRICE vs. SQFT",
     pch = 19, col = "blue")

x_grid <- seq(min(collegetown$sqft), max(collegetown$sqft), length.out = 200) # Create a sequence of SQFT values spanning the observed range

y_pred <- predict(model_quad, newdata = data.frame(sqft = x_grid)) # Predict PRICE using the quadratic model for each x in x_grid

lines(x_grid, y_pred, col = "red", lwd = 2) # Draw the fitted quadratic curve

alpha2 <- coef(model_quad)["I(sqft^2)"] # Extract alpha2 from the fitted model

x0 <- 20 # Point of tangency: 2,000 sq ft = 20 in 'SQFT' units

y0 <- predict(model_quad, newdata = data.frame(sqft = x0)) # Predicted price at x0

slope <- 2 * alpha2 * x0  # derivative = 2*alpha2*x

intercept_tangent <- y0 - slope * x0

abline(a = intercept_tangent, b = slope, col = "green", lwd = 2, lty = 2) # Draw the tangent line

points(x0, y0, pch = 19, col = "green", cex = 1.3) # Optionally, add a point marking the location of tangency


# e.
alpha1 <- coef(model_quad)[1] # Extract coefficients
alpha2 <- coef(model_quad)[2]

x0 <- 20 #Define the point of interest: 2,000 sq ft => SQFT = 20

P_x0 <- alpha1 + alpha2 * (x0^2) # Compute the predicted PRICE at x0

dP_dx <- 2 * alpha2 * x0 # Derivative at x0

elasticity_x0 <- dP_dx * (x0 / P_x0) # Elasticity = (dP/dx) * (x0 / P_x0)
elasticity_x0


#f.
res_linear <- residuals(model)
res_quad   <- residuals(model_quad)
# Plot for the linear model
plot(collegetown$sqft, res_linear,
     xlab = "House Size (hundreds of sq ft)",
     ylab = "Residual (PRICE in thousands of dollars)",
     main = "Linear Model Residuals vs. SQFT",
     pch = 19, col = "blue")
abline(h = 0, col = "red", lwd = 2)

# Plot for the quadratic model
plot(collegetown$sqft, res_quad,
     xlab = "House Size (hundreds of sq ft)",
     ylab = "Residual (PRICE in thousands of dollars)",
     main = "Quadratic Model Residuals vs. SQFT",
     pch = 19, col = "blue")
abline(h = 0, col = "red", lwd = 2)

# g.

# SSE for linear model
SSE_linear <- sum(residuals(model)^2)

# SSE for quadratic model
SSE_quad <- sum(residuals(model_quad)^2)

# Display the results
SSE_linear
SSE_quad
#============================================================================================================
# 02.25
#============================================================================================================

#a ==========================================================================================================
cex5_small <- read_excel("/Users/tom/Documents/NYCU/Financial econometrics/poe5xlsx/cex5_small.xlsx")

hist(cex5_small$foodaway,
     main = "Histogram of Food Away From Home Expenditure",
     xlab = "foodaway ($ per month per person)",
     col = "lightblue",  # Fill color for the bars
     border = "white")   # Border color for the bars

summary(cex5_small$foodaway)

mean_foodaway   <- mean(cex5_small$foodaway)
median_foodaway <- median(cex5_small$foodaway)

quantiles_25_75 <- quantile(cex5_small$foodaway, probs = c(0.25, 0.75))

cat("Mean of foodaway:   ", mean_foodaway, "\n")
cat("Median of foodaway: ", median_foodaway, "\n")
cat("25th percentile:     ", quantiles_25_75[1], "\n")
cat("75th percentile:     ", quantiles_25_75[2], "\n")

#b ==========================================================================================================
# (1) Households with an advanced degree
mean_adv  <- mean(cex5_small$foodaway[cex5_small$advanced == 1])
median_adv <- median(cex5_small$foodaway[cex5_small$advanced == 1])

# (2) Households with a college degree (and no advanced degree, if you want them separate)
mean_college  <- mean(cex5_small$foodaway[cex5_small$college == 1])
median_college <- median(cex5_small$foodaway[cex5_small$college == 1])

# (3) Households with no advanced degree or no college degree
mean_none <- mean(cex5_small$foodaway[!(cex5_small$advanced == 0 | cex5_small$college == 0)])
median_none <- median(cex5_small$foodaway[!(cex5_small$advanced == 0 | cex5_small$college == 0)])

cat("Advanced degree - Mean:", mean_adv, " Median:", median_adv, "\n")
cat("College degree  - Mean:", mean_college, " Median:", median_college, "\n")
cat("No degree       - Mean:", mean_none, " Median:", median_none, "\n")

#c ==========================================================================================================

cex5_small$ln_foodaway <- log(cex5_small$foodaway)
hist(cex5_small$ln_foodaway,
     main = "Histogram of ln(foodaway)",
     xlab = "ln(foodaway)",
     col = "lightblue",
     border = "white")

summary(cex5_small$ln_foodaway)
length(cex5_small$foodaway)
length(cex5_small$ln_foodaway)

#d ==========================================================================================================
cex5_small_clean <- subset(cex5_small, foodaway > 0 & !is.na(foodaway))

# 2. Create ln(foodaway)

# 3. Fit the regression
model_log_lin <- lm(ln_foodaway ~ income, data = cex5_small_clean)

summary(model_log_lin)

#e ==========================================================================================================

plot(cex5_small_clean$income, cex5_small_clean$ln_foodaway,
     xlab = "Household Monthly Income (in $100s)",
     ylab = "ln(foodaway)",
     main = "ln(foodaway) vs. income with Fitted Line",
     pch = 19,       # Solid circles
     col = "blue")   # Points in blue

# Overlay the fitted regression line
# For a simple linear model with one predictor, abline() works nicely.
abline(model_log_lin, col = "red", lwd = 2)

#f ==========================================================================================================
resid_log_lin <- residuals(model_log_lin)
plot(cex5_small_clean$income, resid_log_lin,
     xlab = "Household Monthly Income (in $100s)",
     ylab = "Residuals from Log-Linear Model",
     main = "Residuals vs. INCOME",
     pch = 19, col = "blue")
abline(h = 0, col = "red", lwd = 2)  # reference line at 0

# ==========================================================================================================
#02.28
# ==========================================================================================================
cps5_small <- read_excel("/Users/tom/Documents/NYCU/Financial econometrics/poe5xlsx/cps5_small.xlsx")
# Summary statistics for wage
summary(cps5_small$wage)

# Summary statistics for educ
summary(cps5_small$educ)

hist(cps5_small$wage,
     main = "Histogram of Hourly Wages",
     xlab = "Hourly Wage (dollars per hour)",
     col = "lightblue",
     border = "white")

# Histogram of educ
hist(cps5_small$educ,
     main = "Histogram of Years of Education",
     xlab = "Years of Education",
     col = "lightgreen",
     border = "white")

# b ==========================================================================================================

model_lin <- lm(wage ~ educ, data = cps5_small)

# View the summary of the regression
summary(model_lin)

# c ==========================================================================================================

# Compute the residuals
resid_lin <- residuals(model_lin)

# (Optional) Store them in the data frame for convenience
cps5_small$resid_lin <- resid_lin

# Plot the residuals against educ
plot(cps5_small$educ, resid_lin,
     xlab = "Years of Education",
     ylab = "Residuals",
     main = "Residuals vs. educ",
     pch = 19, col = "blue")
abline(h = 0, col = "red", lwd = 2)

# d ==========================================================================================================

library(stargazer)

# Subset the data for each group and run regressions
model_male   <- lm(wage ~ educ, data = subset(cps5_small, female == 0))
model_female <- lm(wage ~ educ, data = subset(cps5_small, female == 1))
model_black  <- lm(wage ~ educ, data = subset(cps5_small, black == 1))
model_white  <- lm(wage ~ educ, data = subset(cps5_small, black == 0))

stargazer(model_male, model_female, model_black, model_white,
          type = "text",  # or "html" or "latex"
          column.labels = c("Males", "Females", "Blacks", "Whites"),
          dep.var.labels = "Hourly Wage",
          covariate.labels = "Education (Years)",
          title = "Separate Regressions by Gender and Race",
          digits = 3)

# e ==========================================================================================================

model_quad <- lm(wage ~ I(educ^2), data = cps5_small)

# (3) View the summary
summary(model_quad)

alpha2 <- coef(model_quad)[2]  # The coefficient on I(EDUC^2)

# Marginal effect at 12 years
ME_12 <- 2 * alpha2 * 12

# Marginal effect at 16 years
ME_16 <- 2 * alpha2 * 16
model_lin_alpha2 <- coef(model_lin)['educ']
model_lin_alpha2
cat("Marginal Effect at 12 years of education:", ME_12, "\n")
cat("Marginal Effect at 16 years of education:", ME_16, "\n")

# f ==========================================================================================================

# 1) Basic scatterplot of WAGE vs. EDUC
plot(cps5_small$educ, cps5_small$wage,
     main = "WAGE vs. EDUC with Linear and Quadratic Fits",
     xlab = "Years of Education",
     ylab = "Hourly Wage",
     pch = 19,      # solid circle
     col = "blue")  # points in blue

# 2) Add the linear fit (straight line)
abline(model_lin, col = "red", lwd = 2)

# 3) Add the quadratic fit
#    First, create a sequence of EDUC values spanning the data range
x_vals <- seq(min(cps5_small$educ, na.rm = TRUE),
              max(cps5_small$educ, na.rm = TRUE),
              length.out = 200)

#    Predict wages using the quadratic model at each x_vals point
y_quad <- predict(model_quad, newdata = data.frame(educ = x_vals))

#    Draw the quadratic curve
lines(x_vals, y_quad, col = "green", lwd = 2)

# Optional: Add a legend to distinguish the lines
legend("topleft",
       legend = c("Data", "Linear Fit", "Quadratic Fit"),
       col = c("blue", "red", "green"),
       pch = c(19, NA, NA),
       lty = c(NA, 1, 1),
       bty = "n",
       lwd = c(NA, 2, 2))

