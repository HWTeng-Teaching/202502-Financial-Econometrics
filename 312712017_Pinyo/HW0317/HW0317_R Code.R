##################################### C04Q28_a################################################

# Load necessary libraries
library(ggplot2)
library(lmtest)

# Read the CSV file (adjust the path to your file)
data <- read.csv("C:/Users/PINYKEWD/Documents/wa_wheat.csv")

# Check the structure of the data
head(data)

# Focus on the Northampton column and create a new dataframe
northampton_data <- data.frame(TIME = data$time, northampton = data$northampton)

# 1. Estimate the first equation: northampton = β0 + β1TIME + et
model1 <- lm(northampton ~ TIME, data=northampton_data)
summary(model1)

# 2. Estimate the second equation: northampton = α0 + α1 * ln(TIME) + et
model2 <- lm(northampton ~ log(TIME), data=northampton_data)
summary(model2)

# 3. Estimate the third equation: northampton = γ0 + γ1TIME^2 + et
model3 <- lm(northampton ~ I(TIME^2), data=northampton_data)
summary(model3)

# 4. Estimate the fourth equation: ln(northampton) = ϕ0 + ϕ1TIME + et
model4 <- lm(log(northampton) ~ TIME, data=northampton_data)
summary(model4)

# 5. Plot the fitted values for each model
ggplot(northampton_data, aes(x=TIME, y=northampton)) +
  geom_point() +
  geom_smooth(method="lm", formula=y ~ x, se=FALSE, col="blue") +
  ggtitle("Fitted values for model 1: northampton = β0 + β1TIME")

ggplot(northampton_data, aes(x=TIME, y=northampton)) +
  geom_point() +
  geom_smooth(method="lm", formula=y ~ log(x), se=FALSE, col="blue") +
  ggtitle("Fitted values for model 2: northampton = α0 + α1ln(TIME)")

ggplot(northampton_data, aes(x=TIME, y=northampton)) +
  geom_point() +
  geom_smooth(method="lm", formula=y ~ I(x^2), se=FALSE, col="blue") +
  ggtitle("Fitted values for model 3: northampton = γ0 + γ1TIME^2")

ggplot(northampton_data, aes(x=TIME, y=log(northampton))) +
  geom_point() +
  geom_smooth(method="lm", formula=y ~ x, se=FALSE, col="blue") +
  ggtitle("Fitted values for model 4: ln(northampton) = ϕ0 + ϕ1TIME")

# 6. Plot the residuals for each model
par(mfrow=c(2,2))  # Create a 2x2 plot layout

plot(model1$residuals, main="Residuals for model 1", ylab="Residuals")
plot(model2$residuals, main="Residuals for model 2", ylab="Residuals")
plot(model3$residuals, main="Residuals for model 3", ylab="Residuals")
plot(model4$residuals, main="Residuals for model 4", ylab="Residuals")

# 7. Perform normality tests on the residuals
shapiro.test(model1$residuals)  # Shapiro-Wilk test for normality
shapiro.test(model2$residuals)
shapiro.test(model3$residuals)
shapiro.test(model4$residuals)

# 8. Evaluate R-squared for each model
cat("R-squared for model 1:", summary(model1)$r.squared, "\n")
cat("R-squared for model 2:", summary(model2)$r.squared, "\n")
cat("R-squared for model 3:", summary(model3)$r.squared, "\n")
cat("R-squared for model 4:", summary(model4)$r.squared, "\n")

# Optional: Check for heteroscedasticity using Breusch-Pagan test
bptest(model1)  
bptest(model2)
bptest(model3)
bptest(model4)

##################################### C04Q28_b################################################
# Load necessary libraries
library(ggplot2)
library(car) # For influence measures

# Assuming 'model3' is the chosen model
chosen_model <- model3

# 1. Calculate studentized residuals
studentized_residuals <- rstudent(chosen_model)

# 2. Calculate leverage values (hat values)
leverage <- hatvalues(chosen_model)

# 3. Calculate DFBETAS
dfbetas_values <- dfbetas(chosen_model)

# 4. Calculate DFFITS
dffits_values <- dffits(chosen_model)

# 5. Combine all diagnostics into a single dataframe for review
diagnostics <- data.frame(
  Observation = 1:nrow(northampton_data),
  Studentized_Residuals = studentized_residuals,
  Leverage = leverage,
  DFBETAS_Intercept = dfbetas_values[,1],
  DFBETAS_TIME2 = dfbetas_values[,2], # Adjust column based on predictors
  DFFITS = dffits_values
)

# View the diagnostics
print(diagnostics)

# Identify unusual observations based on thresholds:
# - Studentized residuals: |value| > 2 indicates potential outliers.
# - Leverage: Compare with 2 * (p + 1) / n, where p is the number of predictors and n is the number of observations.
# - DFBETAS: |value| > 2 / sqrt(n) indicates influential observations.
# - DFFITS: |value| > 2 * sqrt(p / n) indicates influential observations.

n <- nrow(northampton_data) # Number of observations
p <- length(coef(chosen_model)) # Number of predictors including intercept

leverage_threshold <- 2 * (p + 1) / n
dfbetas_threshold <- 2 / sqrt(n)
dffits_threshold <- 2 * sqrt(p / n)

unusual_observations <- diagnostics[
  abs(diagnostics$Studentized_Residuals) > 2 |
    diagnostics$Leverage > leverage_threshold |
    abs(diagnostics$DFBETAS_Intercept) > dfbetas_threshold |
    abs(diagnostics$DFBETAS_TIME2) > dfbetas_threshold |
    abs(diagnostics$DFFITS) > dffits_threshold, ]

print("Unusual Observations:")
print(unusual_observations)

##################################### C04Q28_d################################################
# Subset the data to include only observations up to 1996
training_data <- subset(northampton_data, TIME <= 47)  # TIME = 47 corresponds to 1996 (1950 + 47 - 1)

# Refit the chosen quadratic model using the training data
model3_training <- lm(northampton ~ I(TIME^2), data=training_data)

# Display the summary of the refitted model
summary(model3_training)

# Predict the yield for 1997 (TIME = 48) and construct a 95% prediction interval
new_data <- data.frame(TIME = 48)  # TIME = 48 corresponds to 1997
new_data$TIME2 <- new_data$TIME^2

prediction <- predict(model3_training, newdata=new_data, interval="prediction", level=0.95)
print("Prediction and 95% Prediction Interval for Yield in 1997:")
print(prediction)

# Compare the prediction interval with the actual observed value in 1997
actual_yield_1997 <- subset(northampton_data, TIME == 48)$northampton
cat("Actual Yield in 1997:", actual_yield_1997, "\n")

# Check if the actual yield falls within the prediction interval
if (actual_yield_1997 >= prediction[2] && actual_yield_1997 <= prediction[3]) {
  cat("The actual yield in 1997 falls within the prediction interval.\n")
} else {
  cat("The actual yield in 1997 does NOT fall within the prediction interval.\n")
}

##################################### C04Q29_a################################################

install.packages("tseries")

# Load necessary libraries
library(dplyr)
library(ggplot2)
library(tseries)  # For Jarque-Bera test

# Load the dataset (adjust the path to your file)
data <- read.csv("C:/Users/PINYKEWD/Documents/cex5_small.csv")

# Calculate summary statistics for food and income
summary_stats <- data %>%
  summarise(
    food_mean = mean(food, na.rm = TRUE),
    food_median = median(food, na.rm = TRUE),
    food_min = min(food, na.rm = TRUE),
    food_max = max(food, na.rm = TRUE),
    food_sd = sd(food, na.rm = TRUE),
    income_mean = mean(income, na.rm = TRUE),
    income_median = median(income, na.rm = TRUE),
    income_min = min(income, na.rm = TRUE),
    income_max = max(income, na.rm = TRUE),
    income_sd = sd(income, na.rm = TRUE)
  )

# Print summary statistics
print("Summary Statistics:")
print(summary_stats)

# Create histograms with mean and median marked
# Histogram for food
ggplot(data, aes(x = food)) +
  geom_histogram(fill = "skyblue", color = "black", bins = 30) +
  geom_vline(aes(xintercept = summary_stats$food_mean), color = "red", linetype = "dashed", linewidth = 1) +
  geom_vline(aes(xintercept = summary_stats$food_median), color = "blue", linetype = "dashed", linewidth = 1) +
  labs(title = "Histogram of Food Expenditures",
       subtitle = "Red dashed line = Mean, Blue dashed line = Median")

# Histogram for income
ggplot(data, aes(x = income)) +
  geom_histogram(fill = "lightgreen", color = "black", bins = 30) +
  geom_vline(aes(xintercept = summary_stats$income_mean), color = "red", linetype = "dashed", linewidth = 1) +
  geom_vline(aes(xintercept = summary_stats$income_median), color = "blue", linetype = "dashed", linewidth = 1) +
  labs(title = "Histogram of Income",
       subtitle = "Red dashed line = Mean, Blue dashed line = Median")

# Jarque-Bera test for normality
jb_test_food <- jarque.bera.test(data$food)
jb_test_income <- jarque.bera.test(data$income)

# Print test results
cat("\nJarque-Bera Test Results:\n")
cat("food:   Test Statistic =", jb_test_food$statistic, "  p-value =", jb_test_food$p.value, "\n")
cat("income: Test Statistic =", jb_test_income$statistic, "  p-value =", jb_test_income$p.value, "\n")

# Interpret results
if (summary_stats$food_mean > summary_stats$food_median) {
  cat("\nfood: Mean > Median → Right-skewed distribution\n")
} else {
  cat("\nfood: Mean ≤ Median → Left-skewed or symmetric distribution\n")
}

if (summary_stats$income_mean > summary_stats$income_median) {
  cat("income: Mean > Median → Right-skewed distribution\n")
} else {
  cat("income: Mean ≤ Median → Left-skewed or symmetric distribution\n")
}

##################################### C04Q29_b################################################

# Load necessary libraries
library(ggplot2)

# Load the dataset (adjust the path to your file)
data <- read.csv("C:/Users/PINYKEWD/Documents/cex5_small.csv")

# Estimate the linear relationship: FOOD = β1 + β2INCOME + e
linear_model <- lm(food ~ income, data = data)

# Display the summary of the model
summary(linear_model)

# Extract the coefficient for β2 (income) and construct a 95% confidence interval
beta2_confidence_interval <- confint(linear_model, level = 0.95)["income", ]
cat("95% Confidence Interval for β2 (income):\n")
print(beta2_confidence_interval)

# Check if β2 is estimated precisely
if (abs(beta2_confidence_interval[1]) < abs(beta2_confidence_interval[2])) {
  cat("The effect of income on average food expenditure is estimated relatively precisely.\n")
} else {
  cat("The effect of income on average food expenditure is not estimated precisely.\n")
}

# Create a scatter plot FOOD vs INCOME with fitted least squares line
ggplot(data, aes(x = income, y = food)) +
  geom_point(color = "blue") +
  geom_smooth(method = "lm", se = FALSE, color = "red") +
  labs(title = "Scatter Plot of FOOD vs INCOME",
       subtitle = "Red line: Fitted Least Squares Line",
       x = "Income",
       y = "Food Expenditure") +
  theme_minimal()

##################################### C04Q29_c################################################

# Load necessary libraries
library(ggplot2)
library(tseries)  # For Jarque-Bera test

# Load the dataset (adjust the path to your file)
data <- read.csv("C:/Users/PINYKEWD/Documents/cex5_small.csv")

# Fit the linear model (repeating part b for completeness)
linear_model <- lm(food ~ income, data = data)

# 1. Obtain least squares residuals
residuals <- residuals(linear_model)

# 2. Plot residuals against INCOME
ggplot(data, aes(x = income, y = residuals)) +
  geom_point(color = "red") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "blue") +
  labs(title = "Residuals vs INCOME",
       subtitle = "Dashed line at residual = 0",
       x = "Income",
       y = "Residuals") +
  theme_minimal()

# 3. Construct residual histogram with mean/median lines
residual_stats <- data.frame(
  residual_mean = mean(residuals),
  residual_median = median(residuals)
)

ggplot(data.frame(residuals), aes(x = residuals)) +
  geom_histogram(fill = "orange", color = "black", bins = 30) +
  geom_vline(aes(xintercept = residual_stats$residual_mean), color = "red", linetype = "dashed", linewidth = 1) +
  geom_vline(aes(xintercept = residual_stats$residual_median), color = "blue", linetype = "dashed", linewidth = 1) +
  labs(title = "Histogram of Residuals",
       subtitle = "Red dashed line = Mean, Blue dashed line = Median")

# 4. Jarque-Bera test for normality of residuals
jb_test_residuals <- jarque.bera.test(residuals)
cat("\nJarque-Bera Test for Residuals:\n")
cat("Test Statistic =", jb_test_residuals$statistic, "  p-value =", jb_test_residuals$p.value, "\n")

# 5. Interpretation of normality importance
cat("\nNormality Importance Explanation:\n")
cat("In regression analysis, it is more important for the random error term (e) to be normally distributed than the variables FOOD and INCOME themselves.\n")
cat("The Central Limit Theorem ensures that coefficient estimates are asymptotically normal even if variables are not.\n")
cat("However, normality of residuals is critical for valid hypothesis tests (e.g., t-tests for coefficients) and confidence intervals.\n")
cat("If residuals are non-normal, statistical inference may be unreliable, even if FOOD/INCOME are non-normal.\n")

##################################### C04Q29_d################################################

# Load necessary libraries
library(ggplot2)

# Load the dataset (adjust the path to your file)
data <- read.csv("C:/Users/PINYKEWD/Documents/cex5_small.csv")

# Fit the linear model: FOOD = β1 + β2INCOME + e
linear_model <- lm(food ~ income, data = data)

# Display model summary
summary(linear_model)

# Extract coefficients from the model
beta1 <- coef(linear_model)[1]  # Intercept
beta2 <- coef(linear_model)[2]  # Slope (income coefficient)

# Define income levels for elasticity calculation
income_levels <- c(19, 65, 160)

# Calculate point estimates of food expenditure at each income level (fitted values)
fitted_food <- beta1 + beta2 * income_levels

# Calculate point estimates of elasticity at each income level
elasticity_point_estimates <- beta2 * income_levels / fitted_food

# Construct 95% confidence interval for β2
beta2_confidence_interval <- confint(linear_model, level = 0.95)["income", ]

# Calculate lower and upper bounds of elasticity at each income level using CI of β2
elasticity_lower_bound <- beta2_confidence_interval[1] * income_levels / fitted_food
elasticity_upper_bound <- beta2_confidence_interval[2] * income_levels / fitted_food

# Combine results into a dataframe for easy interpretation
results <- data.frame(
  Income = income_levels,
  Fitted_Food = fitted_food,
  Elasticity_Point_Estimate = elasticity_point_estimates,
  Elasticity_Lower_Bound = elasticity_lower_bound,
  Elasticity_Upper_Bound = elasticity_upper_bound
)

# Print results
print("Elasticity Estimates:")
print(results)

# Interpretation of Elasticities:
cat("\nInterpretation:\n")
cat("Elasticities measure the percentage change in food expenditure for a percentage change in income.\n")
cat("As income increases, economic theory suggests that the elasticity of food expenditure may decrease,\n")
cat("because food is a necessity and additional income is less likely to be spent proportionally on food.\n")
cat("Observe whether elasticities are similar or dissimilar and whether interval estimates overlap.\n")

##################################### C04Q29_e################################################

# Load necessary libraries
library(ggplot2)

# Load the dataset (adjust the path to your file)
data <- read.csv("C:/Users/PINYKEWD/Documents/cex5_small.csv")

# Estimate the log-log relationship
log_log_model <- lm(log(food) ~ log(income), data = data)

# Display model summary
summary(log_log_model)

# Create scatter plot for ln(FOOD) vs ln(INCOME) with fitted line
ggplot(data, aes(x = log(income), y = log(food))) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "red") +
  labs(title = "Log-Log Model: ln(FOOD) vs ln(INCOME)",
       x = "ln(INCOME)", y = "ln(FOOD)") +
  theme_minimal()

# Calculate generalized R-squared for log-log model
gen_r_squared_log <- 1 - exp(-2/nrow(data) * (logLik(log_log_model) - logLik(lm(log(food) ~ 1, data = data))))

# Calculate R-squared for linear model (from part b)
linear_model <- lm(food ~ income, data = data)
r_squared_linear <- summary(linear_model)$r.squared

# Print R-squared values for comparison
cat("Generalized R-squared (Log-Log model):", gen_r_squared_log, "\n")
cat("R-squared (Linear model):", r_squared_linear, "\n")

# Compare model fit
if (gen_r_squared_log > r_squared_linear) {
  cat("The log-log model appears to fit the data better based on R-squared values.\n")
} else {
  cat("The linear model appears to fit the data better based on R-squared values.\n")
}

##################################### C04Q29_f################################################

# Load necessary libraries
library(ggplot2)

# Load the dataset (adjust the path to your file)
data <- read.csv("C:/Users/PINYKEWD/Documents/cex5_small.csv")

# Estimate the log-log relationship
log_log_model <- lm(log(food) ~ log(income), data = data)

# Display model summary
summary(log_log_model)

# Extract the coefficient for log(income) and construct a 95% confidence interval
elasticity_point_estimate <- coef(log_log_model)["log(income)"]
elasticity_confidence_interval <- confint(log_log_model, level = 0.95)["log(income)", ]

# Print the results
cat("Point Estimate of Elasticity (Log-Log Model):", elasticity_point_estimate, "\n")
cat("95% Confidence Interval for Elasticity (Log-Log Model):\n")
print(elasticity_confidence_interval)

#Compare it with d
# Extract the  confidence interval from part d
linear_elasticity_ci_19<-c(0.1,0.2) #enter the confident interval from part d, income = 19
linear_elasticity_ci_65<-c(0.2,0.3) #enter the confident interval from part d, income = 65
linear_elasticity_ci_160<-c(0.3,0.4) #enter the confident interval from part d, income = 160

#Check if  d confident intervals contain  the log log point estimate
check_overlap <- function(interval, point) {
  return(point >= interval[1] && point <= interval[2])
}

overlap_19<-check_overlap(linear_elasticity_ci_19,elasticity_point_estimate)
overlap_65<-check_overlap(linear_elasticity_ci_65,elasticity_point_estimate)
overlap_160<-check_overlap(linear_elasticity_ci_160,elasticity_point_estimate)

cat(paste("Does CI d with income 19 contain point estimate loglog? ",overlap_19),"\n")
cat(paste("Does CI d with income 65 contain point estimate loglog? ",overlap_65),"\n")
cat(paste("Does CI d with income 160 contain point estimate loglog? ",overlap_160),"\n")



# Provide a comparison
cat("\nComparison with Part (d):\n")
cat("The coefficient of log(income) in the log-log model directly represents the elasticity of food expenditure\n")
cat("with respect to income. Compare the point estimate and confidence interval here to the estimates in part (d),\n")
cat("considering the income levels at which the elasticities were calculated in part (d) (19, 65, 160).\n")

if( (elasticity_point_estimate>linear_elasticity_ci_19[1]) & (elasticity_point_estimate<linear_elasticity_ci_19[2])){
  cat("Based on the values you have inputed, the eslaticity on income 19 and the loglog model are similar","\n")
}

##################################### C04Q29_g################################################

# Load necessary libraries
library(ggplot2)
library(tseries)  # For Jarque-Bera test

# Load the dataset (adjust the path to your file)
data <- read.csv("C:/Users/PINYKEWD/Documents/cex5_small.csv")

# Estimate the log-log relationship
log_log_model <- lm(log(food) ~ log(income), data = data)

# Obtain least squares residuals from the log-log model
residuals_log_log <- residuals(log_log_model)

# Plot residuals against ln(INCOME)
ggplot(data, aes(x = log(income), y = residuals_log_log)) +
  geom_point() +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  labs(title = "Residuals vs ln(INCOME) (Log-Log Model)",
       x = "ln(INCOME)",
       y = "Residuals") +
  theme_minimal()

# Construct a residual histogram
ggplot(data.frame(residuals_log_log), aes(x = residuals_log_log)) +
  geom_histogram(bins = 30, fill = "skyblue", color = "black") +
  labs(title = "Histogram of Residuals (Log-Log Model)",
       x = "Residuals",
       y = "Frequency") +
  theme_minimal()

# Carry out the Jarque-Bera test for normality
jb_test_log_log <- jarque.bera.test(residuals_log_log)
cat("\nJarque-Bera Test for Log-Log Model Residuals:\n")
print(jb_test_log_log)

# Print test results and interpret the conclusion
cat("\nConclusion about Normality of Regression Errors:\n")
if (jb_test_log_log$p.value > 0.05) {
  cat("Based on the Jarque-Bera test, we fail to reject the null hypothesis of normality.\n")
  cat("The residuals appear to be normally distributed in the log-log model.\n")
} else {
  cat("Based on the Jarque-Bera test, we reject the null hypothesis of normality.\n")
  cat("The residuals do not appear to be normally distributed in the log-log model.\n")
}

##################################### C04Q29_h################################################

# Load necessary libraries
library(ggplot2)

# Load your data (adjust the path if necessary)
data <- read.csv("C:/Users/PINYKEWD/Documents/cex5_small.csv")

# Ensure column names are in lowercase (if not already)
colnames(data) <- tolower(colnames(data))

# Compute the log of INCOME (lowercase column name assumed: 'income')
data$log_income <- log(data$income)

# Estimate the linear-log model (lowercase column names assumed: 'food' and 'income')
model_linear_log <- lm(food ~ log_income, data = data)

# Summary of the model
summary(model_linear_log)

# Scatter plot for FOOD vs ln(INCOME) with the fitted least squares line
ggplot(data, aes(x = log_income, y = food)) +
  geom_point() + 
  geom_smooth(method = "lm", col = "blue", se = FALSE) +
  labs(title = "FOOD vs ln(INCOME)",
       x = "ln(INCOME)",
       y = "FOOD") +
  theme_minimal()

# Calculate the R-squared value for the linear-log model
r_squared_linear_log <- summary(model_linear_log)$r.squared
print(paste("R-squared for the linear-log model:", r_squared_linear_log))

##################################### C04Q29_i################################################
# Load necessary libraries
library(ggplot2)

# Load your data (adjust the path if necessary)
data <- read.csv("C:/Users/PINYKEWD/Documents/cex5_small.csv")

# Ensure column names are in lowercase (if not already)
colnames(data) <- tolower(colnames(data))

# Compute the log of INCOME (lowercase column name assumed: 'income')
data$log_income <- log(data$income)

# Estimate the linear-log model (lowercase column names assumed: 'food' and 'income')
model_linear_log <- lm(food ~ log_income, data = data)

# Get the coefficient (beta_2) and standard error of the log_income variable
beta_2 <- coef(model_linear_log)["log_income"]
se_beta_2 <- summary(model_linear_log)$coefficients["log_income", "Std. Error"]

# Calculate the elasticity at INCOME = 19, 65, and 160
income_values <- c(19, 65, 160)

# Calculate the point estimates of elasticity
elasticity_point <- beta_2 / income_values

# Calculate the 95% confidence intervals for elasticity
z_value <- qnorm(0.975)  # 1.96 for 95% CI

# Compute the 95% confidence intervals for the elasticity at the given income values
elasticity_lower <- elasticity_point - z_value * (se_beta_2 / income_values)
elasticity_upper <- elasticity_point + z_value * (se_beta_2 / income_values)

# Output the results
data.frame(
  INCOME = income_values,
  Elasticity = elasticity_point,
  Lower_CI = elasticity_lower,
  Upper_CI = elasticity_upper
)

##################################### C04Q29_j################################################

# Load necessary libraries
library(ggplot2)
library(tseries)  # For Jarque-Bera test

# Load your data (adjust the path if necessary)
data <- read.csv("C:/Users/PINYKEWD/Documents/cex5_small.csv")

# Ensure column names are in lowercase (if not already)
colnames(data) <- tolower(colnames(data))

# Compute the log of INCOME (lowercase column name assumed: 'income')
data$log_income <- log(data$income)

# Estimate the linear-log model (lowercase column names assumed: 'food' and 'income')
model_linear_log <- lm(food ~ log_income, data = data)

# Obtain the residuals
data$residuals <- residuals(model_linear_log)

# Scatter plot of residuals against ln(INCOME)
ggplot(data, aes(x = log_income, y = residuals)) +
  geom_point() +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  labs(title = "Residuals vs ln(INCOME)",
       x = "ln(INCOME)",
       y = "Residuals") +
  theme_minimal()

# Histogram of residuals
ggplot(data, aes(x = residuals)) +
  geom_histogram(binwidth = 5, fill = "blue", color = "black", alpha = 0.7) +
  geom_vline(aes(xintercept = mean(residuals)), color = "red", linetype = "dashed") +
  geom_vline(aes(xintercept = median(residuals)), color = "green", linetype = "dashed") +
  labs(title = "Histogram of Residuals",
       x = "Residuals",
       y = "Frequency") +
  theme_minimal()

# Perform the Jarque-Bera test for normality
jb_test <- jarque.bera.test(data$residuals)
print(jb_test)

