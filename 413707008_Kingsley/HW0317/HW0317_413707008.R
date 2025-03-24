# Question 4
library(ggplot2)
library(dplyr)

# Define model coefficients
model1_intercept <- 64.289
model1_slope <- 0.990

model2_intercept <- 39.464
model2_slope <- 15.312

# PART A & B: Create data for both models
exper_values1 <- 0:30  # Model 1: 0 to 30 years
ratings_model1 <- model1_intercept + model1_slope * exper_values1

exper_values2 <- 1:30  # Model 2: 1 to 30 years (since ln(0) is undefined)
ratings_model2 <- model2_intercept + model2_slope * log(exper_values2)

# Plot Model 1 (Part a)
p1 <- ggplot(data.frame(EXPER = exper_values1, RATING = ratings_model1), aes(x = EXPER, y = RATING)) +
  geom_line(color = "blue", size = 1) +
  labs(title = "Model 1: RATING = 64.289 + 0.990*EXPER",
       x = "Experience (years)", y = "Performance Rating") +
  theme_minimal() +
  ylim(60, 100)
print(p1)

# Plot Model 2 (Part b)
p2 <- ggplot(data.frame(EXPER = exper_values2, RATING = ratings_model2), aes(x = EXPER, y = RATING)) +
  geom_line(color = "red", size = 1) +
  labs(title = "Model 2: RATING = 39.464 + 15.312*ln(EXPER)",
       x = "Experience (years)", y = "Performance Rating") +
  theme_minimal() +
  ylim(60, 100)
print(p2)

# Comparison plot for both models
plot_data <- data.frame(
  Experience = c(exper_values2, exper_values2),
  Rating = c(model1_intercept + model1_slope * exper_values2, ratings_model2),
  Model = factor(rep(c("Linear (Model 1)", "Logarithmic (Model 2)"), each = length(exper_values2)))
)

combined_plot <- ggplot(plot_data, aes(x = Experience, y = Rating, color = Model)) +
  geom_line(size = 1) +
  labs(title = "Comparison of Model 1 and Model 2",
       x = "Experience (years)", 
       y = "Performance Rating") +
  theme_minimal() +
  ylim(60, 100)
print(combined_plot)

# Explanation for why artists with no experience are excluded from Model 2
cat("The four artists with no experience (EXPER = 0) are not used in Model 2 because\n")
cat("the natural logarithm of zero (ln(0)) is undefined. The logarithmic function is\n")
cat("only defined for positive values, so Model 2 can only include artists with EXPER > 0.\n\n")

# PART C: Marginal effects for Model 1
cat("PART C: Marginal Effects for Model 1\n")
cat("---------------------------------\n")
cat("In Model 1 (linear model), the marginal effect is constant at", model1_slope, "for all experience levels.\n")
cat("(i) For an artist with 10 years of experience:", model1_slope, "\n")
cat("(ii) For an artist with 20 years of experience:", model1_slope, "\n\n")

# PART D: Marginal effects for Model 2
cat("PART D: Marginal Effects for Model 2\n")
cat("---------------------------------\n")
me_10 <- model2_slope / 10
me_20 <- model2_slope / 20

cat("For Model 2 (logarithmic model), the marginal effect formula is:", model2_slope, "/ EXPER\n")
cat("(i) For an artist with 10 years of experience:", me_10, "\n")
cat("(ii) For an artist with 20 years of experience:", me_20, "\n\n")

# Plot marginal effects for Model 2
exper_seq <- 1:30
me_values <- model2_slope / exper_seq

me_plot <- ggplot(data.frame(EXPER = exper_seq, MarginalEffect = me_values), aes(x = EXPER, y = MarginalEffect)) +
  geom_line(color = "red", size = 1) +
  labs(title = "Marginal Effect of Experience on Rating in Model 2",
       x = "Experience (years)", 
       y = "Marginal Effect") +
  theme_minimal()
print(me_plot)

# PART E: Model fit comparison
cat("PART E: Model Fit Comparison\n")
cat("---------------------------------\n")
r2_model1_all <- 0.3793
r2_model1_nonzero <- 0.4858
r2_model2 <- 0.6414

# Create a comparison table
r2_comparison <- data.frame(
  Model = c("Model 1 (all data)", "Model 1 (excluding zero experience)", "Model 2"),
  R_squared = c(r2_model1_all, r2_model1_nonzero, r2_model2)
)

# Print the table
print(r2_comparison)

cat("\nModel 2 fits the data better with an R² of 0.6414 compared to Model 1's R² values.\n")
cat("Even when considering only artists with experience, Model 1's R² of 0.4858\n")
cat("is still lower than Model 2's R² of 0.6414.\n\n")

# PART F: Economic plausibility discussion
# Create a plot showing marginal effects by experience level
marginal_data <- data.frame(
  Experience = seq(1, 30, by = 1),
  Model1_Marginal = rep(model1_slope, 30),
  Model2_Marginal = model2_slope / seq(1, 30, by = 1)
)

marginal_plot <- ggplot(marginal_data, aes(x = Experience)) +
  geom_line(aes(y = Model1_Marginal, color = "Linear (Model 1)"), size = 1) +
  geom_line(aes(y = Model2_Marginal, color = "Logarithmic (Model 2)"), size = 1) +
  scale_color_manual(values = c("Linear (Model 1)" = "blue", "Logarithmic (Model 2)" = "red")) +
  labs(title = "Marginal Effect of One Additional Year of Experience",
       x = "Years of Experience",
       y = "Increase in Rating per Additional Year",
       color = "Model Type") +
  theme_minimal()

print(marginal_plot)
cat("PART F: Economic Plausibility\n")
cat("---------------------------------\n")
cat("Based on economic reasoning, Model 2 (logarithmic model) is more plausible because:\n\n")
cat("1. Diminishing returns: In most professions, the benefit of each additional year\n")
cat("   of experience tends to diminish over time. Model 2 captures this economic principle\n")
cat("   with its logarithmic form, showing larger gains in early years that taper off.\n\n")
cat("2. Learning curve: Professionals typically learn rapidly in early career stages\n")
cat("   and then their growth plateaus. Model 2 reflects this pattern, while Model 1\n")
cat("   unrealistically suggests constant improvement at the same rate throughout a career.\n\n")
cat("3. Statistical evidence: The higher R² for Model 2 (0.6414 vs. 0.3793) supports the\n")
cat("   notion that the relationship between experience and performance is non-linear.\n\n")
cat("4. Practical implications: Model 2 suggests that investing in early career development\n")
cat("   yields higher returns, which aligns with common training and development practices.\n\n")
cat("While Model 1 is simpler, Model 2 better represents the economic reality of how\n")
cat("experience translates to performance in technical professional fields.")

# Question 28
# Define the URL
url <- "http://www.principlesofeconometrics.com/poe5/data/rdata/wa_wheat.rdata"
# Open a connection to the URL
con <- url(url, "rb")  # "rb" = read binary mode
# Load the RData file directly from the web
load(con)
# Close the connection
close(con)

# 1) YIELD = β0 + β1 * TIME
model1 <- lm(northampton ~ time, data = wa_wheat)

# 2) YIELD = α0 + α1 * ln(TIME)
model2 <- lm(northampton ~ log(time), data = wa_wheat)

# 3) YIELD = γ0 + γ1 * TIME^2
model3 <- lm(northampton ~ I(time^2), data = wa_wheat)

# 4) ln(YIELD) = φ0 + φ1 * TIME
model4 <- lm(log(northampton) ~ time, data = wa_wheat)

# Summary of each model
summary(model1)  # For Model 1
summary(model2)  # For Model 2
summary(model3)  # For Model 3
summary(model4)  # For Model 4

# a. and b.

# Fitted values
wa_wheat$fit1 <- predict(model1)
wa_wheat$fit2 <- predict(model2)
wa_wheat$fit3 <- predict(model3)
wa_wheat$fit4 <- exp(predict(model4))  # Exponentiate for Model 4 to compare with YIELD

df = wa_wheat

# Load gridExtra for arranging multiple plots
library(gridExtra)
library(ggplot2)

# Create individual plots for each model using ggplot2
# Model 1: Linear
p1 <- ggplot(df, aes(x = time, y = northampton)) +
  geom_point(color = "black", size = 2) +
  geom_line(aes(y = fit1), color = "blue", linewidth = 1) +
  labs(title = "Model 1: Linear", x = "Time", y = "Yield") +
  theme_minimal()

# Model 2: ln(TIME)
p2 <- ggplot(df, aes(x = time, y = northampton)) +
  geom_point(color = "black", size = 2) +
  geom_line(aes(y = fit2), color = "red", linewidth = 1) +
  labs(title = "Model 2: ln(TIME)", x = "Time", y = "Yield") +
  theme_minimal()

# Model 3: TIME^2
p3 <- ggplot(df, aes(x = time, y = northampton)) +
  geom_point(color = "black", size = 2) +
  geom_line(aes(y = fit3), color = "green", linewidth = 1) +
  labs(title = "Model 3: TIME^2", x = "Time", y = "Yield") +
  theme_minimal()

# Model 4: ln(YIELD)
p4 <- ggplot(df, aes(x = time, y = northampton)) +
  geom_point(color = "black", size = 2) +
  geom_line(aes(y = fit4), color = "purple", linewidth = 1) +
  labs(title = "Model 4: ln(YIELD)", x = "Time", y = "Yield") +
  theme_minimal()

# Arrange the plots in a 2x2 grid
grid.arrange(p1, p2, p3, p4, nrow = 2, ncol = 2)

# Calculate residuals
df$resid1 <- resid(model1)
df$resid2 <- resid(model2)
df$resid3 <- resid(model3)
df$resid4 <- resid(model4)

# Create residual plots
# Model 1
r1 <- ggplot(df, aes(x = time, y = resid1)) +
  geom_point(color = "black", size = 2) +
  geom_hline(yintercept = 0, color = "red", linetype = "dashed") +
  labs(title = "Residuals Model 1", x = "Time", y = "Residuals") +
  theme_minimal()

# Model 2
r2 <- ggplot(df, aes(x = time, y = resid2)) +
  geom_point(color = "black", size = 2) +
  geom_hline(yintercept = 0, color = "red", linetype = "dashed") +
  labs(title = "Residuals Model 2", x = "Time", y = "Residuals") +
  theme_minimal()

# Model 3
r3 <- ggplot(df, aes(x = time, y = resid3)) +
  geom_point(color = "black", size = 2) +
  geom_hline(yintercept = 0, color = "red", linetype = "dashed") +
  labs(title = "Residuals Model 3", x = "Time", y = "Residuals") +
  theme_minimal()

# Model 4
r4 <- ggplot(df, aes(x = time, y = resid4)) +
  geom_point(color = "black", size = 2) +
  geom_hline(yintercept = 0, color = "red", linetype = "dashed") +
  labs(title = "Residuals Model 4", x = "Time", y = "Residuals") +
  theme_minimal()

# Arrange residual plots in a 2x2 grid
grid.arrange(r1, r2, r3, r4, nrow = 2, ncol = 2)

# Normality test for residuals
shapiro1 <- shapiro.test(resid(model1))
shapiro2 <- shapiro.test(resid(model2))
shapiro3 <- shapiro.test(resid(model3))
shapiro4 <- shapiro.test(resid(model4))

# Adjusted R-squared
adj_r2 <- c(summary(model1)$adj.r.squared,
            summary(model2)$adj.r.squared,
            summary(model3)$adj.r.squared,
            summary(model4)$adj.r.squared)

# Print results
cat("Shapiro-Wilk p-values:\n")
cat("Model 1:", shapiro1$p.value, "\n")
cat("Model 2:", shapiro2$p.value, "\n")
cat("Model 3:", shapiro3$p.value, "\n")
cat("Model 4:", shapiro4$p.value, "\n")

cat("\nAdjusted R-squared:\n")
cat("Model 1:", adj_r2[1], "\n")
cat("Model 2:", adj_r2[2], "\n")
cat("Model 3:", adj_r2[3], "\n")
cat("Model 4:", adj_r2[4], "\n")

# c. and d.
# Studentized residuals
stud_res <- rstudent(model3)

# Leverage
leverage <- hatvalues(model3)
p <- 2  # Number of parameters (intercept + TIME^2)
leverage_threshold <- 2 * (2 / nrow(df))  # Rule of thumb: 2*(p/n), where p = number of parameters

# DFBETAS and DFFITS
dfbetas_vals <- dfbetas(model3)
dffits_vals <- dffits(model3)

# Thresholds
stud_res_threshold <- 2
dffits_threshold <- 2 / sqrt(nrow(df))
dfbetas_threshold <- 2 / sqrt(nrow(df))

# DFBETAS condition: any value in a row exceeds the threshold
dfbetas_condition <- apply(abs(dfbetas_vals), 1, function(x) any(x > dfbetas_threshold))

# Identify unusual observations based on all criteria
outliers <- which(
  abs(stud_res) > stud_res_threshold |
    leverage > leverage_threshold |
    abs(dffits_vals) > dffits_threshold |
    dfbetas_condition
)

# Output results
cat("Unusual observations (indices):", outliers, "\n")
cat("Corresponding years:", 1950 + outliers - 1, "\n")

# Subset data up to 1996 (TIME <= 47)
df_1996 <- df[df$time <= 47, ]

# Fit Model 3: YIELD = γ0 + γ1 * TIME^2 using only data up to 1996
model3_1996 <- lm(northampton ~ I(time^2), data = df_1996)

# Predict YIELD for 1997 (TIME = 48)
new_data <- data.frame(time = 48)
pred <- predict(model3_1996, newdata = new_data, interval = "prediction", level = 0.95)

# True yield for 1997 (TIME = 48)
true_yield_1997 <- df$northampton[df$time == 48]

# Print prediction interval and check if true value is within it
cat("95% Prediction Interval for YIELD in 1997:\n")
cat("  Lower bound:", pred[1, "lwr"], "\n")
cat("  Upper bound:", pred[1, "upr"], "\n")
cat("  Predicted YIELD:", pred[1, "fit"], "\n")
cat("  True YIELD in 1997:", true_yield_1997, "\n")
cat("Does the interval contain the true value? ",
    true_yield_1997 >= pred[1, "lwr"] & true_yield_1997 <= pred[1, "upr"],
    "\n")

# Question 29
# Define the URL
url <- "http://www.principlesofeconometrics.com/poe5/data/rdata/cex5_small.rdata"
# Open a connection to the URL
con <- url(url, "rb")  # "rb" = read binary mode
# Load the RData file directly from the web
load(con)
# Close the connection
close(con)

# Load required libraries
library(dplyr)
library(ggplot2)
library(tseries)  # For Jarque-Bera test

# We'll use the cex5_small dataframe as provided in the question

# a. Summary statistics and histograms
# Calculate summary statistics for FOOD and INCOME
food_summary <- summary(cex5_small$food)
income_summary <- summary(cex5_small$income)
food_sd <- sd(cex5_small$food)
income_sd <- sd(cex5_small$income)

cat("Summary statistics for FOOD:\n")
print(food_summary)
cat("Standard deviation:", food_sd, "\n\n")

cat("Summary statistics for INCOME:\n")
print(income_summary)
cat("Standard deviation:", income_sd, "\n\n")

# Create histograms
hist_food <- ggplot(cex5_small, aes(x = food)) +
  geom_histogram(bins = 10, fill = "skyblue", color = "black") +
  geom_vline(xintercept = mean(cex5_small$food), color = "red", linetype = "dashed", size = 1) +
  geom_vline(xintercept = median(cex5_small$food), color = "blue", linetype = "dashed", size = 1) +
  labs(title = "Histogram of FOOD Expenditure",
       x = "FOOD Expenditure",
       y = "Frequency") +
  annotate("text", x = mean(cex5_small$food) + 10, y = 3, label = "Mean", color = "red") +
  annotate("text", x = median(cex5_small$food) - 10, y = 3, label = "Median", color = "blue")

hist_income <- ggplot(cex5_small, aes(x = income)) +
  geom_histogram(bins = 10, fill = "lightgreen", color = "black") +
  geom_vline(xintercept = mean(cex5_small$income), color = "red", linetype = "dashed", size = 1) +
  geom_vline(xintercept = median(cex5_small$income), color = "blue", linetype = "dashed", size = 1) +
  labs(title = "Histogram of INCOME",
       x = "INCOME",
       y = "Frequency") +
  annotate("text", x = mean(cex5_small$income) + 5, y = 3, label = "Mean", color = "red") +
  annotate("text", x = median(cex5_small$income) - 5, y = 3, label = "Median", color = "blue")

print(hist_food)
print(hist_income)

# Jarque-Bera test for normality
jb_food <- jarque.bera.test(cex5_small$food)
jb_income <- jarque.bera.test(cex5_small$income)

cat("Jarque-Bera test for FOOD:\n")
print(jb_food)
cat("\nJarque-Bera test for INCOME:\n")
print(jb_income)

# Based on the histograms, determine if they are symmetrical and bell-shaped
# Compare mean and median to assess skewness

# b. Linear relationship
linear_model <- lm(food ~ income, data = cex5_small)
summary_linear <- summary(linear_model)

cat("\nLinear Model: FOOD = β₁ + β₂*INCOME + e\n")
print(summary_linear)

# 95% confidence interval for β₂
conf_int_beta2 <- confint(linear_model, "income", level = 0.95)
cat("\n95% Confidence Interval for β₂:", conf_int_beta2[1], "to", conf_int_beta2[2], "\n")

# Scatter plot with fitted line
scatter_linear <- ggplot(cex5_small, aes(x = income, y = food)) +
  geom_point() +
  geom_smooth(method = "lm", se = TRUE, color = "red") +
  labs(title = "Linear Model: FOOD vs INCOME",
       x = "INCOME",
       y = "FOOD Expenditure")

print(scatter_linear)

# Assess precision of the estimate
# Check width of confidence interval and significance

# c. Residual analysis for linear model
residuals_linear <- residuals(linear_model)
fitted_linear <- fitted(linear_model)

# Plot residuals against INCOME
residual_plot_linear <- ggplot(data.frame(income = cex5_small$income, residuals = residuals_linear), 
                               aes(x = income, y = residuals)) +
  geom_point() +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  labs(title = "Residuals vs INCOME (Linear Model)",
       x = "INCOME",
       y = "Residuals")

print(residual_plot_linear)

# Histogram of residuals
hist_residuals_linear <- ggplot(data.frame(residuals = residuals_linear), aes(x = residuals)) +
  geom_histogram(bins = 10, fill = "orange", color = "black") +
  labs(title = "Histogram of Residuals (Linear Model)",
       x = "Residuals",
       y = "Frequency")

print(hist_residuals_linear)

# Jarque-Bera test for residuals
jb_residuals_linear <- jarque.bera.test(residuals_linear)
cat("\nJarque-Bera test for residuals (Linear Model):\n")
print(jb_residuals_linear)

# Explain importance of residual normality vs. variable normality
cat("\nIt is more important for the random error e to be normally distributed than for the variables FOOD and INCOME to be normally distributed. This is because the statistical inference (hypothesis tests, confidence intervals) in regression analysis relies on the assumption that the error term follows a normal distribution, not the variables themselves.\n")

# d. Elasticity calculation for linear model
# Calculate elasticities at INCOME = 19, 65, and 160
income_points <- c(19, 65, 160)
food_predicted_linear <- predict(linear_model, newdata = data.frame(income = income_points))

# Point estimate of elasticity for linear model
elasticity_linear <- income_points * coef(linear_model)["income"] / food_predicted_linear

# Standard error for elasticity calculation
elasticity_se_linear <- function(model, income, predicted_food) {
  beta2 <- coef(model)["income"]
  var_beta2 <- vcov(model)["income", "income"]
  se <- sqrt(var_beta2) * income / predicted_food
  return(se)
}

elasticity_se_values <- sapply(1:length(income_points), function(i) {
  elasticity_se_linear(linear_model, income_points[i], food_predicted_linear[i])
})

# 95% confidence intervals
t_critical <- qt(0.975, df = linear_model$df.residual)
elasticity_ci_lower <- elasticity_linear - t_critical * elasticity_se_values
elasticity_ci_upper <- elasticity_linear + t_critical * elasticity_se_values

elasticity_results_linear <- data.frame(
  INCOME = income_points,
  FOOD_Predicted = food_predicted_linear,
  Elasticity = elasticity_linear,
  CI_Lower = elasticity_ci_lower,
  CI_Upper = elasticity_ci_upper
)

cat("\nElasticity estimates for Linear Model:\n")
print(elasticity_results_linear)

# Economic interpretation
cat("\nBased on economic principles, the income elasticity for food should decrease as income increases, as food is a necessity. Higher income households spend a smaller proportion of additional income on food compared to lower income households.\n")

# e. Log-log model
# Create log variables (handling potential zeros)
cex5_small$ln_food <- log(cex5_small$food)
cex5_small$ln_income <- log(cex5_small$income)

# Remove any rows with infinite or NA values due to log transformation
cex5_log <- cex5_small[is.finite(cex5_small$ln_food) & is.finite(cex5_small$ln_income), ]

log_log_model <- lm(ln_food ~ ln_income, data = cex5_log)
summary_log_log <- summary(log_log_model)

cat("\nLog-Log Model: ln(FOOD) = γ₁ + γ₂*ln(INCOME) + e\n")
print(summary_log_log)

# Scatter plot with fitted line for log-log model
scatter_log_log <- ggplot(cex5_log, aes(x = ln_income, y = ln_food)) +
  geom_point() +
  geom_smooth(method = "lm", se = TRUE, color = "blue") +
  labs(title = "Log-Log Model: ln(FOOD) vs ln(INCOME)",
       x = "ln(INCOME)",
       y = "ln(FOOD)")

print(scatter_log_log)

# Calculate generalized R² for log-log model
# Step 1: Generate predictions on the log scale
log_predictions <- predict(log_log_model)

# Step 2: Calculate predicted values on original scale
# For log-log models, we need to apply a smearing estimator (Duan's method)
# to correctly account for retransformation bias
residuals <- residuals(log_log_model)
smearing_factor <- mean(exp(residuals))
predictions_original_scale <- exp(log_predictions) * smearing_factor

# Step 3: Calculate actual food values
actual_food <- cex5_log$food

# Step 4: Calculate the squared correlation coefficient between 
# actual and predicted values on the original scale
generalized_r2_log_log <- cor(actual_food, predictions_original_scale)^2

cat("Generalized R² for log-log model:", generalized_r2_log_log, "\n")

# Compare models
cat("\nComparing the scatter plots, the log-log model appears to", 
    ifelse(generalized_r2_log_log > summary_linear$r.squared, 
           "fit the data better than", 
           "not fit the data as well as"), 
    "the linear model based on R² values.\n")

# f. Elasticity for log-log model
# In log-log model, the coefficient of ln_income directly represents the elasticity
elasticity_log_log <- coef(log_log_model)["ln_income"]
conf_int_elasticity_log_log <- confint(log_log_model, "ln_income", level = 0.95)

cat("\nElasticity estimate for Log-Log Model:", elasticity_log_log, "\n")
cat("95% Confidence Interval for elasticity:", conf_int_elasticity_log_log[1], "to", conf_int_elasticity_log_log[2], "\n")

# Compare with elasticities from linear model
cat("\nComparing elasticities from Linear and Log-Log models:\n")
cat("Log-Log model elasticity (constant):", elasticity_log_log, "\n")
cat("Linear model elasticity at INCOME = 19:", elasticity_linear[1], "\n")
cat("Linear model elasticity at INCOME = 65:", elasticity_linear[2], "\n")
cat("Linear model elasticity at INCOME = 160:", elasticity_linear[3], "\n")

# Statistical evidence
cat("\nStatistical evidence for comparison:\n")
cat("The elasticity from the log-log model is", 
    ifelse(elasticity_log_log >= min(elasticity_linear) & elasticity_log_log <= max(elasticity_linear),
           "within the range of", 
           "outside the range of"),
    "elasticities calculated from the linear model.\n")

# g. Residual analysis for log-log model
residuals_log_log <- residuals(log_log_model)

# Plot residuals against ln(INCOME)
residual_plot_log_log <- ggplot(data.frame(ln_income = cex5_log$ln_income, residuals = residuals_log_log), 
                                aes(x = ln_income, y = residuals)) +
  geom_point() +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  labs(title = "Residuals vs ln(INCOME) (Log-Log Model)",
       x = "ln(INCOME)",
       y = "Residuals")

print(residual_plot_log_log)

# Histogram of residuals for log-log model
hist_residuals_log_log <- ggplot(data.frame(residuals = residuals_log_log), aes(x = residuals)) +
  geom_histogram(bins = 10, fill = "lightblue", color = "black") +
  labs(title = "Histogram of Residuals (Log-Log Model)",
       x = "Residuals",
       y = "Frequency")

print(hist_residuals_log_log)

# Install and load the moments package
if (!require(moments)) install.packages("moments")
# This line checks if the psych package is installed, and installs it only if it's not already available.
library(moments)

# Calculate skewness and kurtosis
moments_skew <- moments::skewness(residuals_log_log)
moments_kurt <- moments::kurtosis(residuals_log_log)

cat("Moments package - Skewness:", moments_skew, "\n")
cat("Moments package - Kurtosis:", moments_kurt, "\n")

# Jarque-Bera test for residuals of log-log model
jb_residuals_log_log <- jarque.bera.test(residuals_log_log)
cat("\nJarque-Bera test for residuals (Log-Log Model):\n")
print(jb_residuals_log_log)

# h. Linear-log model
linear_log_model <- lm(food ~ ln_income, data = cex5_log)
summary_linear_log <- summary(linear_log_model)

cat("\nLinear-Log Model: FOOD = α₁ + α₂*ln(INCOME) + e\n")
print(summary_linear_log)

# Scatter plot with fitted line for linear-log model
scatter_linear_log <- ggplot(cex5_log, aes(x = ln_income, y = food)) +
  geom_point() +
  geom_smooth(method = "lm", se = TRUE, color = "green") +
  labs(title = "Linear-Log Model: FOOD vs ln(INCOME)",
       x = "ln(INCOME)",
       y = "FOOD Expenditure")

print(scatter_linear_log)

# Compare R² values
cat("\nR² comparison:\n")
cat("Linear Model R²:", summary_linear$r.squared, "\n")
cat("Log-Log Model Generalized R²:", generalized_r2_log_log, "\n")
cat("Linear-Log Model R²:", summary_linear_log$r.squared, "\n")

# i. Elasticity for linear-log model
# For linear-log model: elasticity = (α₂/FOOD)
# Calculate at specific income points
ln_income_points <- log(income_points)
food_predicted_linear_log <- predict(linear_log_model, newdata = data.frame(ln_income = ln_income_points))

# Point estimate of elasticity
elasticity_linear_log <- coef(linear_log_model)["ln_income"] / food_predicted_linear_log

# Standard error calculation
elasticity_se_linear_log <- function(model, predicted_food) {
  alpha2 <- coef(model)["ln_income"]
  var_alpha2 <- vcov(model)["ln_income", "ln_income"]
  se <- sqrt(var_alpha2) / predicted_food
  return(se)
}

elasticity_se_values_linear_log <- sapply(food_predicted_linear_log, function(pred_food) {
  elasticity_se_linear_log(linear_log_model, pred_food)
})

# 95% confidence intervals
t_critical <- qt(0.975, df = linear_log_model$df.residual)
elasticity_ci_lower_linear_log <- elasticity_linear_log - t_critical * elasticity_se_values_linear_log
elasticity_ci_upper_linear_log <- elasticity_linear_log + t_critical * elasticity_se_values_linear_log

elasticity_results_linear_log <- data.frame(
  INCOME = income_points,
  FOOD_Predicted = food_predicted_linear_log,
  Elasticity = elasticity_linear_log,
  CI_Lower = elasticity_ci_lower_linear_log,
  CI_Upper = elasticity_ci_upper_linear_log
)

cat("\nElasticity estimates for Linear-Log Model:\n")
print(elasticity_results_linear_log)

# Compare elasticities across models
cat("\nComparing elasticities across all models:\n")
cat("Log-Log model elasticity (constant):", elasticity_log_log, "\n")
cat("Linear model elasticity at INCOME = 19:", elasticity_linear[1], "\n")
cat("Linear-Log model elasticity at INCOME = 19:", elasticity_linear_log[1], "\n\n")
cat("Linear model elasticity at INCOME = 65:", elasticity_linear[2], "\n")
cat("Linear-Log model elasticity at INCOME = 65:", elasticity_linear_log[2], "\n\n")
cat("Linear model elasticity at INCOME = 160:", elasticity_linear[3], "\n")
cat("Linear-Log model elasticity at INCOME = 160:", elasticity_linear_log[3], "\n")

# j. Residual analysis for linear-log model
residuals_linear_log <- residuals(linear_log_model)

# Plot residuals against ln(INCOME)
residual_plot_linear_log <- ggplot(data.frame(ln_income = cex5_log$ln_income, residuals = residuals_linear_log), 
                                   aes(x = ln_income, y = residuals)) +
  geom_point() +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  labs(title = "Residuals vs ln(INCOME) (Linear-Log Model)",
       x = "ln(INCOME)",
       y = "Residuals")

print(residual_plot_linear_log)

# Histogram of residuals for linear-log model
hist_residuals_linear_log <- ggplot(data.frame(residuals = residuals_linear_log), aes(x = residuals)) +
  geom_histogram(bins = 10, fill = "lightgreen", color = "black") +
  labs(title = "Histogram of Residuals (Linear-Log Model)",
       x = "Residuals",
       y = "Frequency")

print(hist_residuals_linear_log)

# Jarque-Bera test for residuals of linear-log model
jb_residuals_linear_log <- jarque.bera.test(residuals_linear_log)
cat("\nJarque-Bera test for residuals (Linear-Log Model):\n")
print(jb_residuals_linear_log)

# k. Model preference
print(residual_plot_linear)
print(hist_residuals_linear)
moments::skewness(residuals_linear)
moments::kurtosis(residuals_linear)

print(residual_plot_log_log)
print(hist_residuals_log_log)
moments::skewness(residuals_log_log)
moments::kurtosis(residuals_log_log)

print(residual_plot_linear_log)
print(hist_residuals_linear_log)
moments::skewness(residuals_linear_log)
moments::kurtosis(residuals_linear_log)

cat("Model Comparison and Interpretation:\n\n",
    
    "• The linear model is counter-intuitive, as it implies increasing income elasticity,\n",
    "  which goes against economic reasoning (Engel's Law).\n\n",
    
    "• The linear-log model is more consistent with theory, showing decreasing elasticity\n",
    "  with income, but the residuals show a non-random 'spray' pattern indicating\n",
    "  heteroskedasticity.\n\n",
    
    "• The log-log model assumes constant elasticity, which is not unreasonable.\n",
    "  It also shows the most random residual scatter and the residuals are the closest\n",
    "  to normal, based on skewness and kurtosis.\n\n",
    
    "⇒ Based on these criteria, the log-log model appears to be the best choice.\n"
)
