##################################### C02Q17_a################################################

# Load the necessary library for plotting
library(ggplot2)

# Replace 'path/to/collegetown.csv' with the actual path to your dataset
collegetown <- read.csv("C:/Users/PINYKEWD/Documents/collegetown.csv")

# Check the first few rows to confirm the data is loaded correctly
head(collegetown)

# Plot house price (PRICE) against house size (SQFT) using ggplot2 with dark blue color
ggplot(collegetown, aes(x = sqft, y = price)) +
  geom_point(color = "darkblue") +                  # Set color to dark blue
  labs(title = "House Price vs House Size",
       x = "House Size (hundreds of square feet)",
       y = "House Price (in thousands of dollars)") +
  theme_minimal()                                  # Use a clean theme

##################################### C02Q17_b################################################

# Load the necessary library for plotting
library(ggplot2)

# Replace with the actual path to your dataset
collegetown <- read.csv("C:/Users/PINYKEWD/Documents/collegetown.csv")

# Fit the linear regression model
model <- lm(price ~ sqft, data = collegetown)

# View the summary of the model to get the estimates of the coefficients
summary(model)

# Plot house price (PRICE) against house size (SQFT) and add the fitted regression line
ggplot(collegetown, aes(x = sqft, y = price)) +
  geom_point(color = "darkblue") +                  # Plot points
  geom_smooth(method = "lm", color = "red") +       # Add the fitted line (linear regression)
  labs(title = "House Price vs House Size with Fitted Line",
       x = "House Size (hundreds of square feet)",
       y = "House Price (in thousands of dollars)") +
  theme_minimal()                                  # Clean theme

##################################### C02Q17_c################################################

# Load necessary library
library(ggplot2)

# Load dataset (replace with actual file path if needed)
collegetown <- read.csv("C:/Users/PINYKEWD/Documents/collegetown.csv")

# Fit the quadratic regression model (PRICE = α1 + α2 * SQFT^2)
quadratic_model <- lm(price ~ I(sqft^2), data = collegetown)

# View the summary of the model
summary(quadratic_model)

# Compute the marginal effect: dPRICE/dSQFT = 2 * α2 * SQFT
sqft_value <- 2000  # Given house size in square feet
alpha2 <- coef(quadratic_model)[2]  # Extract α2 coefficient
marginal_effect <- 2 * alpha2 * sqft_value

# Print the marginal effect
cat("Marginal Effect at 2000 sqft:", marginal_effect, "thousands of dollars per 100 sqft\n")

# Plot the quadratic regression line with scatter points
ggplot(collegetown, aes(x = sqft, y = price)) +
  geom_point(color = "darkblue") +  # Scatter plot
  stat_smooth(method = "lm", formula = y ~ I(x^2), color = "red") +  # Quadratic fit
  labs(title = "Quadratic Regression: House Price vs. House Size",
       x = "House Size (hundreds of square feet)",
       y = "House Price (in thousands of dollars)") +
  theme_minimal()

##################################### C02Q17_d################################################

  # Load necessary library
  library(ggplot2)
  
  # Load dataset
  collegetown <- read.csv("C:/Users/PINYKEWD/Documents/collegetown.csv")
  
  # Ensure column names are lowercase
  colnames(collegetown) <- tolower(colnames(collegetown))
  
  # Fit the quadratic regression model
  quadratic_model <- lm(price ~ sqft + I(sqft^2), data = collegetown)
  
  # Extract coefficients
  alpha1 <- coef(quadratic_model)[1]  # Intercept
  alpha2 <- coef(quadratic_model)[2]  # Coefficient for SQFT
  alpha3 <- coef(quadratic_model)[3]  # Coefficient for SQFT^2
  
  # 2000 sqft in dataset units
  sqft_value <- 20  
  
  # Compute the fitted price at 2000 sqft
  price_value <- alpha1 + alpha2 * sqft_value + alpha3 * sqft_value^2  
  
  # Compute the slope of the tangent at 2000 sqft
  slope_at_point <- alpha2 + 2 * alpha3 * sqft_value  
  
  # Generate fitted quadratic curve data
  sqft_seq <- seq(min(collegetown$sqft), max(collegetown$sqft), length.out = 100)
  curve_data <- data.frame(sqft = sqft_seq, price = alpha1 + alpha2 * sqft_seq + alpha3 * sqft_seq^2)
  
  # Generate tangent line data centered around 2000 sqft (x=20)
  tangent_seq <- seq(sqft_value - 5, sqft_value + 5, length.out = 50)  # Focus near 20
  tangent_data <- data.frame(
    sqft = tangent_seq,
    price = price_value + slope_at_point * (tangent_seq - sqft_value)  # Tangent equation
  )
  
  # Plot with 2000 sqft point included in the scatter plot
  ggplot(collegetown, aes(x = sqft, y = price)) +
    geom_point(color = "darkblue") +  # Scatter plot
    geom_line(data = curve_data, aes(x = sqft, y = price), color = "red", size = 1.2) +  # Quadratic curve
    geom_line(data = tangent_data, aes(x = sqft, y = price), color = "green", linetype = "dashed", size = 1.2) +  # Tangent line
    annotate("point", x = sqft_value, y = price_value, color = "black", size = 4) +  # Correct tangent point
    annotate("text", x = sqft_value, y = price_value, label = paste("2000 sqft\nPrice: ", round(price_value, 2)), 
             vjust = -1.5, hjust = 0.5, color = "black") +  # Label the 2000 sqft point
    labs(title = "Quadratic Regression: House Price vs. House Size",
         subtitle = "Red: Fitted Quadratic Curve, Green Dashed: Tangent Line at 2000 sqft",
         x = "House Size (hundreds of square feet)",
         y = "House Price (in thousands of dollars)") +
    theme_minimal()

##################################### C02Q17_e################################################
  
  # Load necessary library
  library(ggplot2)
  
  # Load dataset
  collegetown <- read.csv("C:/Users/PINYKEWD/Documents/collegetown.csv")
  
  # Ensure column names are lowercase
  colnames(collegetown) <- tolower(colnames(collegetown))
  
  # Fit the quadratic regression model (as specified in part c)
  quadratic_model <- lm(price ~ I(sqft^2), data = collegetown)
  
  # Extract coefficients
  alpha1 <- coef(quadratic_model)[1]  # Intercept
  alpha2 <- coef(quadratic_model)[2]  # Coefficient for SQFT^2
  
  # 2000 sqft in dataset units
  sqft_value <- 20  # 2000 sqft is represented as 20 in the dataset
  
  # Compute the price at 2000 sqft using the quadratic model
  price_value <- alpha1 + alpha2 * sqft_value^2
  
  # Compute the derivative (slope) at 2000 sqft for the quadratic model
  derivative_at_point <- 2 * alpha2 * sqft_value
  
  # Compute the elasticity at 2000 sqft
  elasticity <- derivative_at_point * (sqft_value / price_value)
  
  # Output the elasticity value
  elasticity
  
  ##################################### C02Q17_e################################################

  # Load necessary library
  library(ggplot2)
  
  # Load dataset
  collegetown <- read.csv("C:/Users/PINYKEWD/Documents/collegetown.csv")
  
  # Ensure column names are lowercase
  colnames(collegetown) <- tolower(colnames(collegetown))
  
  # (b) Fit the linear regression model
  linear_model <- lm(price ~ sqft, data = collegetown)
  
  # Compute the residuals for the linear model
  linear_residuals <- residuals(linear_model)
  
  # (c) Fit the quadratic regression model
  quadratic_model <- lm(price ~ I(sqft^2), data = collegetown)
  
  # Compute the residuals for the quadratic model
  quadratic_residuals <- residuals(quadratic_model)
  
  # Plot residuals for linear model against SQFT
  ggplot(collegetown, aes(x = sqft, y = linear_residuals)) +
    geom_point(color = "blue") +
    labs(title = "Linear Model: Residuals vs. SQFT",
         x = "SQFT (Hundreds of Square Feet)",
         y = "Residuals") +
    theme_minimal()
  
  # Plot residuals for quadratic model against SQFT
  ggplot(collegetown, aes(x = sqft, y = quadratic_residuals)) +
    geom_point(color = "red") +
    labs(title = "Quadratic Model: Residuals vs. SQFT",
         x = "SQFT (Hundreds of Square Feet)",
         y = "Residuals") +
    theme_minimal()
  
  ##################################### C02Q17_e################################################
  
  # Load necessary library
  library(ggplot2)
  
  # Load dataset
  collegetown <- read.csv("C:/Users/PINYKEWD/Documents/collegetown.csv")
  
  # Ensure column names are lowercase
  colnames(collegetown) <- tolower(colnames(collegetown))
  
  # (b) Fit the linear regression model
  linear_model <- lm(price ~ sqft, data = collegetown)
  
  # Compute the residuals for the linear model
  linear_residuals <- residuals(linear_model)
  
  # Calculate SSE for the linear model
  SSE_linear <- sum(linear_residuals^2)
  
  # (c) Fit the quadratic regression model
  quadratic_model <- lm(price ~ I(sqft^2), data = collegetown)
  
  # Compute the residuals for the quadratic model
  quadratic_residuals <- residuals(quadratic_model)
  
  # Calculate SSE for the quadratic model
  SSE_quadratic <- sum(quadratic_residuals^2)
  
  # Print SSE values for both models
  cat("SSE for the linear model: ", SSE_linear, "\n")
  cat("SSE for the quadratic model: ", SSE_quadratic, "\n")
  
  # Compare the SSE values
  if(SSE_linear < SSE_quadratic) {
    cat("The linear model has a lower SSE and fits the data better.")
  } else if(SSE_quadratic < SSE_linear) {
    cat("The quadratic model has a lower SSE and fits the data better.")
  } else {
    cat("Both models have the same SSE.")
  }
  

  ##################################### C02Q25_a################################################
  
  # Load necessary library (if you don't have it installed, you can install it using install.packages("ggplot2"))
  library(ggplot2)
  
  # Load the data (replace the file path with the actual location of cex5_small)
  data <- read.csv("C:/Users/PINYKEWD/Documents/cex5_small.csv")
  
  # Create histogram of FOODAWAY
  ggplot(data, aes(x = foodaway)) +
    geom_histogram(binwidth = 5, fill = "blue", color = "black", alpha = 0.7) +
    labs(title = "Histogram of FOODAWAY Expenditure", x = "FOODAWAY ($ per person per month)", y = "Frequency")
  
  # Calculate summary statistics
  summary_stats <- summary(data$foodaway)
  
  # Calculate mean and median
  mean_foodaway <- mean(data$foodaway)
  median_foodaway <- median(data$foodaway)
  
  # Calculate 25th and 75th percentiles
  percentiles <- quantile(data$foodaway, probs = c(0.25, 0.75))
  
  # Output summary statistics
  print(summary_stats)
  cat("\nMean of FOODAWAY:", mean_foodaway, "\n")
  cat("Median of FOODAWAY:", median_foodaway, "\n")
  cat("25th percentile of FOODAWAY:", percentiles[1], "\n")
  cat("75th percentile of FOODAWAY:", percentiles[2], "\n")
  
  ##################################### C02Q25_b################################################
  
  # Load the data
  data <- read.csv("C:/Users/PINYKEWD/Documents/cex5_small.csv")
  
  # Check for missing values in foodaway
  sum(is.na(data$foodaway))  # How many missing values in foodaway?
  sum(is.na(data$advanced))  # How many missing values in 'advanced' column?
  sum(is.na(data$college))   # How many missing values in 'college' column?
  
  # Clean the data by removing rows where foodaway, advanced, or college have NA values
  data_clean <- data[!is.na(data$foodaway) & !is.na(data$advanced) & !is.na(data$college), ]
  
  # Check unique values in 'advanced' and 'college' columns to ensure correct subsetting
  cat("Unique values in 'advanced':\n")
  print(unique(data_clean$advanced))
  
  cat("Unique values in 'college':\n")
  print(unique(data_clean$college))
  
  # Subset the data based on 'advanced' and 'college' columns
  # Advanced Degree (assuming 'advanced' is 1 for advanced degree)
  advanced_degree_data <- subset(data_clean, advanced == 1)
  
  # College Degree (assuming 'college' is 1 for college degree)
  college_degree_data <- subset(data_clean, college == 1)
  
  # No Degree (assuming these households have no advanced or college degree member)
  no_degree_data <- subset(data_clean, advanced == 0 & college == 0)
  
  # Check the number of rows in each subset
  cat("Rows with advanced degree:", nrow(advanced_degree_data), "\n")
  cat("Rows with college degree:", nrow(college_degree_data), "\n")
  cat("Rows with no degree:", nrow(no_degree_data), "\n")
  
  # Calculate mean and median for each subset, ignoring NA values
  mean_advanced <- mean(advanced_degree_data$foodaway, na.rm = TRUE)
  median_advanced <- median(advanced_degree_data$foodaway, na.rm = TRUE)
  
  mean_college <- mean(college_degree_data$foodaway, na.rm = TRUE)
  median_college <- median(college_degree_data$foodaway, na.rm = TRUE)
  
  mean_no_degree <- mean(no_degree_data$foodaway, na.rm = TRUE)
  median_no_degree <- median(no_degree_data$foodaway, na.rm = TRUE)
  
  # Print the results
  cat("\nMean and Median of FOODAWAY for households with an Advanced Degree member:\n")
  cat("Mean:", mean_advanced, "\n")
  cat("Median:", median_advanced, "\n\n")
  
  cat("Mean and Median of FOODAWAY for households with a College Degree member:\n")
  cat("Mean:", mean_college, "\n")
  cat("Median:", median_college, "\n\n")
  
  cat("Mean and Median of FOODAWAY for households with no advanced or college degree member:\n")
  cat("Mean:", mean_no_degree, "\n")
  cat("Median:", median_no_degree, "\n")
  
  ##################################### C02Q25_c################################################
  
  # Load the data
  data <- read.csv("C:/Users/PINYKEWD/Documents/cex5_small.csv")
  
  # Ensure the 'foodaway' column exists
  if (!"foodaway" %in% names(data)) {
    stop("Column 'foodaway' not found in dataset")
  }
  
  # Remove rows where foodaway is zero or negative (log transformation is not defined for non-positive values)
  data_log <- subset(data, foodaway > 0)
  
  # Apply natural log transformation
  data_log$ln_foodaway <- log(data_log$foodaway)
  
  # Plot histogram of ln(FOODAWAY)
  hist(data_log$ln_foodaway, main="Histogram of ln(FOODAWAY)", xlab="ln(FOODAWAY)", col="skyblue", border="black")
  
  # Compute summary statistics
  mean_ln_foodaway <- mean(data_log$ln_foodaway, na.rm = TRUE)
  median_ln_foodaway <- median(data_log$ln_foodaway, na.rm = TRUE)
  percentiles_ln_foodaway <- quantile(data_log$ln_foodaway, probs = c(0.25, 0.75), na.rm = TRUE)
  
  # Print summary statistics
  cat("Summary Statistics for ln(FOODAWAY):\n")
  cat("Mean:", mean_ln_foodaway, "\n")
  cat("Median:", median_ln_foodaway, "\n")
  cat("25th Percentile:", percentiles_ln_foodaway[1], "\n")
  cat("75th Percentile:", percentiles_ln_foodaway[2], "\n")
  
  # Explanation of different number of observations
  cat("\nNumber of observations in original FOODAWAY:", nrow(data), "\n")
  cat("Number of observations in ln(FOODAWAY):", nrow(data_log), "\n")
  cat("\nDifference in observations occurs because log transformation is not defined for FOODAWAY values that are zero or negative.\n")
  
  ##################################### C02Q25_d################################################
  
  # Load the dataset
  data <- read.csv("C:/Users/PINYKEWD/Documents/cex5_small.csv")
  
  # Check if the required columns exist
  if (!("foodaway" %in% names(data)) | !("income" %in% names(data))) {
    stop("Error: Required columns 'foodaway' and 'income' not found in dataset.")
  }
  
  # Remove rows with NA values in 'foodaway' and 'income'
  data_clean <- na.omit(data[, c("foodaway", "income")])
  
  # Remove rows where 'foodaway' is zero or negative (log transformation requires positive values)
  data_clean <- subset(data_clean, foodaway > 0)
  
  # Apply log transformation
  data_clean$ln_foodaway <- log(data_clean$foodaway)
  
  # Check for any remaining NaN or Inf values after transformation
  data_clean <- subset(data_clean, is.finite(ln_foodaway))
  
  # Ensure there is still data left after filtering
  if (nrow(data_clean) == 0) {
    stop("Error: No valid data left after cleaning. Check 'foodaway' values.")
  }
  
  # Estimate the regression model
  model <- lm(ln_foodaway ~ income, data = data_clean)
  
  # Display the summary of the regression model
  summary(model)
  

  ##################################### C02Q25_e################################################
  
  # Remove rows where 'foodaway' is <= 0 or contains NA/Inf/NaN
  clean_data <- data[!is.na(data$foodaway) & !is.infinite(data$foodaway) & 
                       data$foodaway > 0 & 
                       !is.na(data$income) & !is.infinite(data$income), ]
  
  # Compute ln(FOODAWAY) after filtering out non-positive values
  clean_data$ln_foodaway <- log(clean_data$foodaway)
  
  # Fit the linear model on the cleaned data
  model <- lm(ln_foodaway ~ income, data = clean_data)
  
  # Print the model summary to check the results
  summary(model)
  
  # Create the scatter plot with regression line
  library(ggplot2)
  ggplot(clean_data, aes(x = income, y = ln_foodaway)) +
    geom_point(alpha = 0.5, color = "blue") +  # Scatter plot
    geom_smooth(method = "lm", color = "red", se = FALSE) +  # Regression line
    labs(title = "Scatter Plot of ln(FOODAWAY) vs INCOME",
         x = "Household Income ($100 units)",
         y = "Log of Food Away Spending (ln(FOODAWAY))") +
    theme_minimal()
  
  
  ##################################### C02Q25_f################################################
  
  # Calculate the residuals (difference between observed and predicted values)
  clean_data$residuals <- residuals(model)
  
  # Plot residuals vs. INCOME
  library(ggplot2)
  ggplot(clean_data, aes(x = income, y = residuals)) +
    geom_point(alpha = 0.5, color = "blue") +  # Scatter plot of residuals
    geom_hline(yintercept = 0, linetype = "dashed", color = "red") +  # Line at y = 0
    labs(title = "Residuals vs INCOME",
         x = "Household Income ($100 units)",
         y = "Residuals (ln(FOODAWAY) - Predicted)") +
    theme_minimal()
  
  ##################################### C02Q28_a################################################
  
  # Load the necessary library
  library(ggplot2)
  
  # Read the dataset (replace with the correct path to your file)
  cps5_small <- read.csv("C:/Users/PINYKEWD/Documents/cps5_small.csv")
  
  # 1. Check column names
  colnames(cps5_small)
  
  # 2. Check the structure of the dataset (data types and sample data)
  str(cps5_small)
  
  # 3. Summary statistics for wage and educ
  summary(cps5_small$wage)
  summary(cps5_small$educ)
  
  # 4. If the column names are different, adjust them accordingly and generate histograms
  
  # Histogram for wage (adjust the column name if needed)
  ggplot(cps5_small, aes(x = wage)) +
    geom_histogram(binwidth = 1, fill = "blue", color = "black", alpha = 0.7) +
    labs(title = "Histogram of WAGE", x = "Hourly Wage", y = "Frequency")
  
  # Histogram for educ (adjust the column name if needed)
  ggplot(cps5_small, aes(x = educ)) +
    geom_histogram(binwidth = 1, fill = "green", color = "black", alpha = 0.7) +
    labs(title = "Histogram of EDUC", x = "Years of Education", y = "Frequency")
  
  ##################################### C02Q28_b################################################
  
  # Estimate the linear regression model
  wage_model <- lm(wage ~ educ, data = cps5_small)
  
  # Display the regression summary
  summary(wage_model)
  
  ##################################### C02Q28_c################################################
  
  # Calculate residuals
  cps5_small$residuals <- residuals(wage_model)
  
  # Scatter plot of residuals vs. education
  ggplot(cps5_small, aes(x = educ, y = residuals)) +
    geom_point(alpha = 0.5, color = "blue") +  # Scatter plot points
    geom_hline(yintercept = 0, linetype = "dashed", color = "red") +  # Reference line at zero
    labs(title = "Residuals vs. Education", x = "Years of Education", y = "Residuals") +
    theme_minimal()
  
  ##################################### C02Q28_d################################################
  
  # Estimate separate regressions for males and females
  male_model <- lm(wage ~ educ, data = cps5_small, subset = female == 0)
  female_model <- lm(wage ~ educ, data = cps5_small, subset = female == 1)
  
  # Estimate separate regressions for blacks and whites
  black_model <- lm(wage ~ educ, data = cps5_small, subset = black == 1)
  white_model <- lm(wage ~ educ, data = cps5_small, subset = black == 0)
  
  # Display summaries of the models
  summary(male_model)
  summary(female_model)
  summary(black_model)
  summary(white_model)
  
  ##################################### C02Q28_e################################################
  
  # Estimate the quadratic regression WAGE = α1 + α2EDUC^2 + e
  cps5_small$educ2 <- cps5_small$educ^2  # Create squared education variable
  quad_model <- lm(wage ~ educ + educ2, data = cps5_small)
  
  # Display summary of quadratic regression
  summary(quad_model)
  
  # Extract coefficients
  alpha1 <- coef(quad_model)[1]  # Intercept
  alpha2 <- coef(quad_model)[2]  # Coefficient for EDUC
  alpha3 <- coef(quad_model)[3]  # Coefficient for EDUC^2
  
  # Compute the marginal effects: d(WAGE)/d(EDUC) = α2 + 2 * α3 * EDUC
  marginal_12 <- alpha2 + 2 * alpha3 * 12  # Marginal effect at EDUC = 12
  marginal_16 <- alpha2 + 2 * alpha3 * 16  # Marginal effect at EDUC = 16
  
  # Compare with linear regression marginal effect
  linear_effect <- coef(wage_model)[2]  # From part (b)
  
  # Print results
  cat("Marginal Effect at EDUC = 12:", marginal_12, "\n")
  cat("Marginal Effect at EDUC = 16:", marginal_16, "\n")
  cat("Linear Regression Marginal Effect:", linear_effect, "\n")
  
  ##################################### C02Q28_f################################################
  
  # Load necessary library
  library(ggplot2)
  
  # Fit the linear model
  linear_model <- lm(wage ~ educ, data = cps5_small)
  
  # Fit the quadratic model
  quadratic_model <- lm(wage ~ educ + I(educ^2), data = cps5_small)
  
  # Create a data frame for predictions
  educ_range <- data.frame(educ = seq(min(cps5_small$educ), max(cps5_small$educ), length.out = 100))
  
  # Get predicted values from both models
  educ_range$linear_fit <- predict(linear_model, newdata = educ_range)
  educ_range$quadratic_fit <- predict(quadratic_model, newdata = educ_range)
  
  # Plot actual data points and fitted values
  ggplot(cps5_small, aes(x = educ, y = wage)) +
    geom_point(alpha = 0.5, color = "gray") +  # Scatter plot of actual data
    geom_line(data = educ_range, aes(x = educ, y = linear_fit), color = "blue", size = 1, linetype = "dashed") + # Linear fit
    geom_line(data = educ_range, aes(x = educ, y = quadratic_fit), color = "red", size = 1) + # Quadratic fit
    labs(title = "Comparison of Linear and Quadratic Regression Models",
         x = "Years of Education (EDUC)",
         y = "Hourly Wage (WAGE)",
         caption = "Blue Dashed Line: Linear Model | Red Line: Quadratic Model") +
    theme_minimal()
  
  
  