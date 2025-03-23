#Question 29
#a.
install.packages("moments")
library(moments)
install.packages("tseries")
library(tseries)
library(readxl)
cex5_small <- read_excel("/Users/tom/Documents/NYCU/Financial econometrics/poe5xlsx/cex5_small.xlsx")

# If needed, subset to three-person households (uncomment and adjust variable name if necessary)
# cex5_small <- subset(cex5_small, hhsize == 3)

# Calculate summary statistics for food and income using summary() and sd()
food_summary <- summary(cex5_small$food)
income_summary <- summary(cex5_small$income)
food_sd <- sd(cex5_small$food, na.rm = TRUE)
income_sd <- sd(cex5_small$income, na.rm = TRUE)

# Print the summary statistics and standard deviations
cat("Summary statistics for food:\n")
print(food_summary)
cat("Standard Deviation for food:", food_sd, "\n\n")

cat("Summary statistics for income:\n")
print(income_summary)
cat("Standard Deviation for income:", income_sd, "\n\n")

# Construct histograms for both variables with mean and median marked
par(mfrow = c(1, 2))  # Set up side-by-side plotting

# For food expenditure
food_mean <- mean(cex5_small$food, na.rm = TRUE)
food_median <- median(cex5_small$food, na.rm = TRUE)
hist(cex5_small$food,
     main = "Histogram of Food Expenditure",
     xlab = "Food",
     col = "lightblue",
     border = "black")
abline(v = food_mean, col = "darkgreen", lwd = 2, lty = 2)     # mean line
abline(v = food_median, col = "red", lwd = 2, lty = 2) # median line

# For income
income_mean <- mean(cex5_small$income, na.rm = TRUE)
income_median <- median(cex5_small$income, na.rm = TRUE)
hist(cex5_small$income,
     main = "Histogram of Income",
     xlab = "Income",
     col = "lightblue",
     border = "black")
abline(v = income_mean, col = "darkgreen", lwd = 2, lty = 2)     # mean line
abline(v = income_median, col = "red", lwd = 2, lty = 2) # median line

# Reset plotting layout to default
par(mfrow = c(1, 1))

# Carry out the Jarque-Bera tests for normality
jb_food <- jarque.bera.test(cex5_small$food)
jb_income <- jarque.bera.test(cex5_small$income)

cat("Jarque-Bera test for food:\n")
print(jb_food)
cat("\nJarque-Bera test for income:\n")
print(jb_income)

#b.
lm_food <- lm(food ~ income, data = cex5_small)

# Print the model summary to see the estimated coefficients and diagnostics
summary(lm_food)

# Create a scatter plot of FOOD versus INCOME and add the fitted least squares line
plot(cex5_small$income, cex5_small$food,
     main = "Scatter Plot of FOOD vs. INCOME",
     xlab = "INCOME",
     ylab = "FOOD",
     pch = 19,         # Solid circle markers
     col = "blue")
abline(lm_food, col = "red", lwd = 2)   # Fitted regression line

# Construct a 95% confidence interval for β₂ (the coefficient for INCOME)
confint_beta <- confint(lm_food, "income", level = 0.95)
print(confint_beta)

#c.
residuals_lm <- residuals(lm_food)

skew_lm_resd <- skewness(residuals_lm, na.rm = TRUE)
skew_lm_resd
kurto_lm_resd <- kurtosis(residuals_lm, na.rm = TRUE)
kurto_lm_resd

# Plot residuals against INCOME to check for patterns
plot(cex5_small$income, residuals_lm,
     main = "Residuals vs. INCOME",
     xlab = "INCOME",
     ylab = "Residuals",
     pch = 19,       # solid circle markers
     col = "blue")
abline(h = 0, col = "red", lwd = 2, lty = 2)  # add a horizontal line at 0

# Create a histogram of the residuals
hist(residuals_lm,
     main = "Histogram of Residuals",
     xlab = "Residuals",
     col = "lightblue",
     border = "black")

# Conduct the Jarque-Bera test for normality on the residuals
jb_resid <- jarque.bera.test(residuals_lm)
print(jb_resid)

#d.
elasticity_ci <- function(x, model) {
  beta1 <- coef(model)[1]
  beta2 <- coef(model)[2]

  # Calculate the fitted value of FOOD at income x (treated as nonrandom)
  fitted_value <- beta1 + beta2 * x

  # Point elasticity estimate: elasticity = beta2 * (x / fitted_value)
  elasticity_hat <- beta2 * (x / fitted_value)

  # Standard error for beta2 from the model summary
  se_beta2 <- summary(model)$coefficients["income", "Std. Error"]

  # Using the delta method, the standard error of the elasticity is:
  se_elasticity <- (x / fitted_value) * se_beta2

  # Degrees of freedom and t-critical value for a 95% CI
  df <- model$df.residual
  t_crit <- qt(0.975, df)

  # Construct the confidence interval
  ci_lower <- elasticity_hat - t_crit * se_elasticity
  ci_upper <- elasticity_hat + t_crit * se_elasticity

  return(list(
    point_estimate = elasticity_hat,
    se = se_elasticity,
    ci_lower = ci_lower,
    ci_upper = ci_upper,
    fitted_value = fitted_value  # The corresponding predicted FOOD expenditure
  ))
}

# Calculate elasticity estimates for income levels 19, 65, and 160 using the model lm_food (from part (b))
elasticity_19 <- elasticity_ci(19, lm_food)
elasticity_65 <- elasticity_ci(65, lm_food)
elasticity_160 <- elasticity_ci(160, lm_food)

# Combine all results into one data frame
results_df <- data.frame(
  Income = c(19, 65, 160),
  Fitted_Food = c(elasticity_19$fitted_value, elasticity_65$fitted_value, elasticity_160$fitted_value),
  Elasticity = c(elasticity_19$point_estimate, elasticity_65$point_estimate, elasticity_160$point_estimate),
  SE = c(elasticity_19$se, elasticity_65$se, elasticity_160$se),
  CI_Lower = c(elasticity_19$ci_lower, elasticity_65$ci_lower, elasticity_160$ci_lower),
  CI_Upper = c(elasticity_19$ci_upper, elasticity_65$ci_upper, elasticity_160$ci_upper)
)

# Print the results table
print(results_df)

#e.
# Fit the log-log model: ln(FOOD) = γ1 + γ2 ln(INCOME) + e
lm_log_log <- lm(log(food) ~ log(income), data = cex5_small)
summary(lm_log_log)

# Create a scatter plot for ln(FOOD) versus ln(INCOME) and add the fitted regression line
plot(log(cex5_small$income), log(cex5_small$food),
     main = "Scatter Plot of ln(FOOD) vs ln(INCOME)",
     xlab = "ln(INCOME)",
     ylab = "ln(FOOD)",
     pch = 19,
     col = "blue")
abline(lm_log_log, col = "red", lwd = 2)


# Generate predictions on the log scale
log_predictions <- predict(lm_log_log)

# Apply Duan's smearing estimator to transform predictions back to the original scale
log_residuals <- residuals(lm_log_log)
smearing_factor <- mean(exp(log_residuals))
predictions_original_scale <- exp(log_predictions) * smearing_factor

# Actual food expenditures from the original data
actual_food <- cex5_small$food

# Calculate the generalized R² for the log-log model as the squared correlation between actual and predicted values
generalized_r2_log_log <- cor(actual_food, predictions_original_scale)^2

# Extract the R² from the linear model (assumed previously estimated as lm_food)
linear_model_R2 <- summary(lm_food)$r.squared

# Create a comparison table for both models
model_comparison <- data.frame(
  Model = c("Linear", "Log-Log (Generalized R²)"),
  R2 = c(linear_model_R2, generalized_r2_log_log)
)

print(model_comparison)

#f.

# Extract the elasticity (gamma2) and its standard error from the log-log model
gamma2 <- coef(lm_log_log)["log(income)"]
se_gamma2 <- summary(lm_log_log)$coefficients["log(income)", "Std. Error"]

# Degrees of freedom and t-critical value for a 95% CI
df_log_log <- lm_log_log$df.residual
t_val <- qt(0.975, df_log_log)

# Calculate the 95% confidence interval for gamma2
ci_lower_log_log <- gamma2 - t_val * se_gamma2
ci_upper_log_log <- gamma2 + t_val * se_gamma2

# Print the results for the log-log model elasticity
cat("Log-log model elasticity (point estimate):", gamma2, "\n")
cat("95% Confidence Interval for elasticity:", ci_lower_log_log, "to", ci_upper_log_log, "\n\n")

# For comparison, we assume that the linear model elasticities at various income levels
# were computed in part (d) and stored in elasticity_19, elasticity_65, and elasticity_160.
# For example, here's how to compare with the elasticity at INCOME = 65:

cat("Elasticity from the linear model at INCOME = 65:\n")
print(elasticity_65)

# Create a combined table for comparison
comparison_table <- data.frame(
  Model = c("Linear, INCOME=19", "Linear, INCOME=65", "Linear, INCOME=160", "Log-Log"),
  Elasticity = c(elasticity_19$point_estimate, elasticity_65$point_estimate, elasticity_160$point_estimate, gamma2),
  CI_Lower = c(elasticity_19$ci_lower, elasticity_65$ci_lower, elasticity_160$ci_lower, ci_lower_log_log),
  CI_Upper = c(elasticity_19$ci_upper, elasticity_65$ci_upper, elasticity_160$ci_upper, ci_upper_log_log)
)

print(comparison_table)

#g.
log_log_resid <- residuals(lm_log_log)

# Plot residuals against ln(INCOME)
plot(log(cex5_small$income), log_log_resid,
     main = "Residuals vs ln(INCOME) for Log-Log Model",
     xlab = "ln(INCOME)",
     ylab = "Residuals",
     pch = 19,
     col = "blue")
abline(h = 0, col = "red", lwd = 2, lty = 2)

# Construct a histogram of the residuals
hist(log_log_resid,
     main = "Histogram of Residuals for Log-Log Model",
     xlab = "Residuals",
     col = "lightblue",
     border = "black")

# Perform the Jarque-Bera test for normality on the residuals
jb_log_log <- jarque.bera.test(log_log_resid)
print(jb_log_log)
log_log_skew <- skewness(log_log_resid, na.rm = TRUE)
log_log_kur <- kurtosis(log_log_resid, na.rm = TRUE)
log_log_skew
log_log_kur


#h.
# Estimate the linear-log model: FOOD = α1 + α2 * ln(INCOME) + e
lm_lin_log <- lm(food ~ log(income), data = cex5_small)
summary(lm_lin_log)

# Create a scatter plot for FOOD versus ln(INCOME)
plot(log(cex5_small$income), cex5_small$food,
     main = "Scatter Plot of FOOD vs ln(INCOME)",
     xlab = "ln(INCOME)",
     ylab = "FOOD",
     pch = 19, col = "blue")

# Add the fitted least squares line
alpha1 <- coef(lm_lin_log)[1]
alpha2 <- coef(lm_lin_log)[2]
abline(a = alpha1, b = alpha2, col = "red", lwd = 2)

# Extract R² values for the linear and linear-log models
R2_linear <- summary(lm_food)$r.squared
R2_lin_log <- summary(lm_lin_log)$r.squared

# For the log-log model, we previously calculated a generalized R² (using Duan's smearing estimator)
# Here we assume that 'generalized_r2_log_log' has been computed earlier
# For completeness, if needed, here's how it might have been computed:
# log_predictions <- predict(lm_log_log)
# log_residuals <- residuals(lm_log_log)
# smearing_factor <- mean(exp(log_residuals))
# predictions_original_scale <- exp(log_predictions) * smearing_factor
# actual_food <- cex5_small$food
# generalized_r2_log_log <- cor(actual_food, predictions_original_scale)^2

# Combine the R² values into a comparison table
model_comparison <- data.frame(
  Model = c("Linear", "Linear-Log", "Log-Log (Generalized R²)"),
  R2 = c(R2_linear, R2_lin_log, generalized_r2_log_log)
)

print(model_comparison)

cat("\nThe scatter plot for the linear-log model (FOOD vs. ln(INCOME)) can be compared with the plots from parts (b) and (e).\n")
cat("Review the pattern in each plot and compare the R² values to decide which specification fits the data better.\n")

#i.
# Function to compute elasticity and its 95% CI for the linear-log model using the delta method
elasticity_lin_log <- function(x, model) {
  # Extract estimated coefficients
  alpha1 <- coef(model)[1]
  alpha2 <- coef(model)[2]

  # Fitted FOOD expenditure at income x (treated as fixed)
  fitted_value <- alpha1 + alpha2 * log(x)

  # Point elasticity for the linear-log model: elasticity = alpha2 / (alpha1 + alpha2 * ln(x))
  elasticity_point <- alpha2 / fitted_value

  # Compute the gradient of the elasticity function with respect to alpha1 and alpha2.
  # Let D = (alpha1 + alpha2 * ln(x))
  # Then:  d/d(alpha1) E(x) = -alpha2 / D^2,
  #        d/d(alpha2) E(x) =  alpha1 / D^2.
  D <- fitted_value
  grad_alpha1 <- -alpha2 / (D^2)
  grad_alpha2 <- alpha1 / (D^2)
  gradient <- c(grad_alpha1, grad_alpha2)

  # Get the variance-covariance matrix for the coefficients
  vcov_mat <- vcov(model)

  # Calculate variance using the delta method
  var_elasticity <- t(gradient) %*% vcov_mat %*% gradient
  se_elasticity <- sqrt(var_elasticity)

  # Degrees of freedom and critical t-value for a 95% CI
  df_model <- model$df.residual
  t_val <- qt(0.975, df_model)

  # Construct the confidence interval
  ci_lower <- elasticity_point - t_val * se_elasticity
  ci_upper <- elasticity_point + t_val * se_elasticity

  return(list(
    Income = x,
    Fitted_Food = fitted_value,
    Elasticity = elasticity_point,
    SE = se_elasticity,
    CI_Lower = ci_lower,
    CI_Upper = ci_upper
  ))
}

# Assuming the linear-log model is estimated as:
# lm_lin_log <- lm(food ~ log(income), data = cex5_small)
# Compute elasticity estimates at INCOME = 19, 65, and 160.
elasticity_lin_log_19 <- elasticity_lin_log(19, lm_lin_log)
elasticity_lin_log_65 <- elasticity_lin_log(65, lm_lin_log)
elasticity_lin_log_160 <- elasticity_lin_log(160, lm_lin_log)

# Combine the results into one table
results_lin_log <- data.frame(
  Income = c(elasticity_lin_log_19$Income, elasticity_lin_log_65$Income, elasticity_lin_log_160$Income),
  Fitted_Food = c(elasticity_lin_log_19$Fitted_Food, elasticity_lin_log_65$Fitted_Food, elasticity_lin_log_160$Fitted_Food),
  Elasticity = c(elasticity_lin_log_19$Elasticity, elasticity_lin_log_65$Elasticity, elasticity_lin_log_160$Elasticity),
  SE = c(elasticity_lin_log_19$SE, elasticity_lin_log_65$SE, elasticity_lin_log_160$SE),
  CI_Lower = c(elasticity_lin_log_19$CI_Lower, elasticity_lin_log_65$CI_Lower, elasticity_lin_log_160$CI_Lower),
  CI_Upper = c(elasticity_lin_log_19$CI_Upper, elasticity_lin_log_65$CI_Upper, elasticity_lin_log_160$CI_Upper)
)

print(results_lin_log)

#j.
lin_log_resid <- residuals(lm_lin_log)

lin_log_skew <- skewness(lin_log_resid, na.rm = TRUE)
lin_log_kur <- kurtosis(lin_log_resid, na.rm = TRUE)
lin_log_skew
lin_log_kur
# Plot residuals against ln(INCOME)
plot(log(cex5_small$income), lin_log_resid,
     main = "Residuals vs ln(INCOME) for Linear-Log Model",
     xlab = "ln(INCOME)",
     ylab = "Residuals",
     pch = 19, col = "blue")
abline(h = 0, col = "red", lwd = 2, lty = 2)

# Construct a histogram of the residuals
hist(lin_log_resid,
     main = "Histogram of Residuals for Linear-Log Model",
     xlab = "Residuals",
     col = "lightblue",
     border = "black")

# Perform the Jarque-Bera test for normality on the residuals
library(tseries)
jb_lin_log <- jarque.bera.test(lin_log_resid)
print(jb_lin_log)


#=================================

# Question 4: Model Comparisons and Marginal Effects
library(ggplot2)
library(dplyr)

# Define coefficients for the two models
# Model 1 (Linear): RATING = β₀ + β₁ * EXPER
beta0 <- 64.289
beta1 <- 0.990

# Model 2 (Logarithmic): RATING = β₀' + β₁' * ln(EXPER)
beta0_log <- 39.464
beta1_log <- 15.312

# --- PART A & B: Create data for both models ---

# Data for Model 1: EXPER ranges from 0 to 30 years
exp_lin <- 0:30
rating_lin <- beta0 + beta1 * exp_lin

# Data for Model 2: EXPER from 1 to 30 (excluding zero, because ln(0) is undefined)
exp_log <- 1:30
rating_log <- beta0_log + beta1_log * log(exp_log)

# --- PART A: Plot for Model 1 (Linear) ---
plot_lin <- ggplot(data.frame(Experience = exp_lin, Rating = rating_lin),
                   aes(x = Experience, y = Rating)) +
  geom_line(color = "darkolivegreen", size = 1.2) +
  labs(title = "Linear Model: Rating = 64.289 + 0.990 * Experience",
       x = "Years of Experience",
       y = "Performance Rating") +
  theme_bw() +
  scale_y_continuous(limits = c(60, 100))
print(plot_lin)

# --- PART B: Plot for Model 2 (Logarithmic) ---
plot_log <- ggplot(data.frame(Experience = exp_log, Rating = rating_log),
                   aes(x = Experience, y = Rating)) +
  geom_line(color = "darkmagenta", size = 1.2) +
  labs(title = "Log Model: Rating = 39.464 + 15.312 * ln(Experience)",
       x = "Years of Experience",
       y = "Performance Rating") +
  theme_bw() +
  scale_y_continuous(limits = c(60, 100))
print(plot_log)

# --- Comparison Plot for Both Models ---
comparison_data <- data.frame(
  Experience = rep(exp_log, 2),
  Rating = c(beta0 + beta1 * exp_log, rating_log),
  Model = rep(c("Linear Model", "Log Model"), each = length(exp_log))
)

comp_plot <- ggplot(comparison_data, aes(x = Experience, y = Rating, color = Model)) +
  geom_line(size = 1.1) +
  labs(title = "Model Comparison: Linear vs. Logarithmic",
       x = "Years of Experience",
       y = "Performance Rating") +
  theme_light() +
  scale_y_continuous(limits = c(60, 100)) +
  scale_color_manual(values = c("Linear Model" = "navy", "Log Model" = "firebrick"))
print(comp_plot)

# Explanation: EXPER = 0 is excluded from Model 2 since ln(0) is undefined.
cat("Note: Artists with 0 years of experience are excluded from Model 2 because ln(0) is undefined.\n\n")

# --- PART C: Marginal Effects for Model 1 ---
cat("PART C: Marginal Effects for the Linear Model\n")
cat("---------------------------------------------\n")
cat("For the linear model, the marginal effect remains constant at", beta1, "\n")
cat("For instance, at 10 and 20 years of experience, the effect is", beta1, "\n\n")

# --- PART D: Marginal Effects for Model 2 ---
cat("PART D: Marginal Effects for the Logarithmic Model\n")
cat("-----------------------------------------------\n")
me_10 <- beta1_log / 10
me_20 <- beta1_log / 20
cat("For the log model, the marginal effect is computed as", beta1_log, "/ EXPER.\n")
cat("At 10 years of experience, the marginal effect is", me_10, "\n")
cat("At 20 years of experience, the marginal effect is", me_20, "\n\n")

# Plot marginal effects for Model 2
marginal_df <- data.frame(Experience = exp_log,
                          MarginalEffect = beta1_log / exp_log)
marginal_plot <- ggplot(marginal_df, aes(x = Experience, y = MarginalEffect)) +
  geom_line(color = "orange", size = 1.2) +
  labs(title = "Marginal Effect of Experience (Log Model)",
       x = "Years of Experience",
       y = "Marginal Increase in Rating") +
  theme_classic()
print(marginal_plot)

# --- PART E: Model Fit Comparison ---
cat("PART E: Model Fit Comparison\n")
cat("-----------------------------\n")
r2_lin_all <- 0.3793
r2_lin_nonzero <- 0.4858
r2_log <- 0.6414

fit_table <- data.frame(
  Model = c("Linear Model (All Data)", "Linear Model (Exp > 0)", "Log Model"),
  R_Squared = c(r2_lin_all, r2_lin_nonzero, r2_log)
)
print(fit_table)

cat("\nModel 2 (Log Model) shows a higher R² of 0.6414, indicating a better fit compared to the linear model.\n")
cat("Even when excluding zero experience, the linear model's R² (0.4858) is lower.\n\n")

# --- PART F: Economic Plausibility Discussion ---
# Create a plot comparing marginal effects from both models
marginal_compare <- data.frame(
  Experience = 1:30,
  LinearEffect = rep(beta1, 30),
  LogEffect = beta1_log / (1:30)
)

econ_plot <- ggplot(marginal_compare, aes(x = Experience)) +
  geom_line(aes(y = LinearEffect, color = "Linear Model"), size = 1) +
  geom_line(aes(y = LogEffect, color = "Log Model"), size = 1) +
  scale_color_manual(values = c("Linear Model" = "darkolivegreen", "Log Model" = "darkmagenta")) +
  labs(title = "Marginal Effects: Linear vs. Log Models",
       x = "Years of Experience",
       y = "Increase in Performance Rating",
       color = "Model Type") +
  theme_bw()
print(econ_plot)

cat("PART F: Economic Plausibility Discussion\n")
cat("----------------------------------------\n")
cat("The Logarithmic Model (Model 2) is more plausible since it captures diminishing returns:\n")
cat("• Early in a career, additional experience yields substantial improvements,\n")
cat("• Later, the benefits taper off, aligning with typical learning curves.\n")
cat("Statistically, the higher R² for Model 2 further supports its superior fit.\n")



#++++++++++++++++++++++++++++++++

# Question 28: Wheat Yield Analysis Using WA_WHEAT Data

# Load the dataset directly from the web
url <- "http://www.principlesofeconometrics.com/poe5/data/rdata/wa_wheat.rdata"
connection <- url(url, "rb")  # Open a binary connection
load(connection)              # Load the RData file
close(connection)             # Close the connection

# ------------------------------
# Fit Various Regression Models
# ------------------------------

# Model 1: Linear relationship between YIELD and TIME
model1 <- lm(northampton ~ time, data = wa_wheat)

# Model 2: Linear relationship with log-transformed TIME
model2 <- lm(northampton ~ log(time), data = wa_wheat)

# Model 3: Quadratic relationship: YIELD versus TIME^2
model3 <- lm(northampton ~ I(time^2), data = wa_wheat)

# Model 4: Log-linear model: ln(YIELD) as a function of TIME
model4 <- lm(log(northampton) ~ time, data = wa_wheat)

# Display summaries for each model
summary(model1)
summary(model2)
summary(model3)
summary(model4)

# ----------------------------------
# Append Fitted Values to the Dataset
# ----------------------------------
wa_wheat$fit1 <- predict(model1)
wa_wheat$fit2 <- predict(model2)
wa_wheat$fit3 <- predict(model3)
wa_wheat$fit4 <- exp(predict(model4))  # Back-transform for Model 4

# -------------------------------------
# Create Plots Comparing Fitted Models
# -------------------------------------
library(ggplot2)
library(gridExtra)

# Plot for Model 1: Linear Fit
plot1 <- ggplot(wa_wheat, aes(x = time, y = northampton)) +
  geom_point(color = "darkgray", size = 3, shape = 16) +
  geom_line(aes(y = fit1), color = "steelblue", size = 1.3, linetype = "solid") +
  labs(title = "Model 1: Linear Fit",
       x = "Time", y = "Yield") +
  theme_classic() +
  scale_y_continuous(limits = c(60, 100))

# Plot for Model 2: Log(TIME) Model
plot2 <- ggplot(wa_wheat, aes(x = time, y = northampton)) +
  geom_point(color = "black", size = 3, shape = 17) +
  geom_line(aes(y = fit2), color = "firebrick", size = 1.3, linetype = "dotted") +
  labs(title = "Model 2: Log(TIME) Fit",
       x = "Time", y = "Yield") +
  theme_classic() +
  scale_y_continuous(limits = c(60, 100))

# Plot for Model 3: Quadratic Fit (TIME^2)
plot3 <- ggplot(wa_wheat, aes(x = time, y = northampton)) +
  geom_point(color = "forestgreen", size = 3, shape = 15) +
  geom_line(aes(y = fit3), color = "darkorange", size = 1.3, linetype = "longdash") +
  labs(title = "Model 3: Quadratic Fit (TIME^2)",
       x = "Time", y = "Yield") +
  theme_classic() +
  scale_y_continuous(limits = c(60, 100))

# Plot for Model 4: Log(YIELD) Model (Back-Transformed)
plot4 <- ggplot(wa_wheat, aes(x = time, y = northampton)) +
  geom_point(color = "purple", size = 3, shape = 18) +
  geom_line(aes(y = fit4), color = "mediumpurple4", size = 1.3, linetype = "dotdash") +
  labs(title = "Model 4: Log(YIELD) Fit",
       x = "Time", y = "Yield") +
  theme_classic() +
  scale_y_continuous(limits = c(60, 100))

# Arrange the four model plots in a 2x2 grid
grid.arrange(plot1, plot2, plot3, plot4, nrow = 2, ncol = 2)

cat("Note: Artists with 0 experience are excluded from Model 2 because ln(0) is undefined.\n\n")

# ---------------------------------
# Residual Analysis for Each Model
# ---------------------------------

# Calculate residuals and add them to the data frame
wa_wheat$resid1 <- resid(model1)
wa_wheat$resid2 <- resid(model2)
wa_wheat$resid3 <- resid(model3)
wa_wheat$resid4 <- resid(model4)

# Residual plot for Model 1
residPlot1 <- ggplot(wa_wheat, aes(x = time, y = resid1)) +
  geom_point(color = "black", size = 2) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "blue") +
  labs(title = "Residuals: Model 1", x = "Time", y = "Residuals") +
  theme_light()

# Residual plot for Model 2
residPlot2 <- ggplot(wa_wheat, aes(x = time, y = resid2)) +
  geom_point(color = "black", size = 2) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "blue") +
  labs(title = "Residuals: Model 2", x = "Time", y = "Residuals") +
  theme_light()

# Residual plot for Model 3
residPlot3 <- ggplot(wa_wheat, aes(x = time, y = resid3)) +
  geom_point(color = "black", size = 2) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "blue") +
  labs(title = "Residuals: Model 3", x = "Time", y = "Residuals") +
  theme_light()

# Residual plot for Model 4
residPlot4 <- ggplot(wa_wheat, aes(x = time, y = resid4)) +
  geom_point(color = "black", size = 2) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "blue") +
  labs(title = "Residuals: Model 4", x = "Time", y = "Residuals") +
  theme_light()

# Arrange the residual plots in a grid
grid.arrange(residPlot1, residPlot2, residPlot3, residPlot4, nrow = 2, ncol = 2)

# ------------------------------
# Normality Tests and R² Statistics
# ------------------------------

# Shapiro-Wilk tests for normality of residuals
shapiro1 <- shapiro.test(resid(model1))
shapiro2 <- shapiro.test(resid(model2))
shapiro3 <- shapiro.test(resid(model3))
shapiro4 <- shapiro.test(resid(model4))

# Collect adjusted R-squared values from all models
adj_R2 <- c(summary(model1)$adj.r.squared,
            summary(model2)$adj.r.squared,
            summary(model3)$adj.r.squared,
            summary(model4)$adj.r.squared)

cat("Shapiro-Wilk p-values for residuals:\n")
cat("Model 1:", shapiro1$p.value, "\n")
cat("Model 2:", shapiro2$p.value, "\n")
cat("Model 3:", shapiro3$p.value, "\n")
cat("Model 4:", shapiro4$p.value, "\n\n")

cat("Adjusted R-squared values:\n")
cat("Model 1:", adj_R2[1], "\n")
cat("Model 2:", adj_R2[2], "\n")
cat("Model 3:", adj_R2[3], "\n")
cat("Model 4:", adj_R2[4], "\n\n")

# -------------------------------------------
# Outlier Diagnostics for Model 3 (TIME^2 Model)
# -------------------------------------------

# Compute studentized residuals, leverage, DFBETAS, and DFFITS for Model 3
student_resid <- rstudent(model3)
leverage_vals <- hatvalues(model3)
num_params <- 2  # Intercept and TIME^2
lev_thresh <- 2 * (num_params / nrow(wa_wheat))

dfbetas_vals <- dfbetas(model3)
dffits_vals <- dffits(model3)

# Set thresholds for diagnostics
stud_thresh <- 2
dffits_thresh <- 2 / sqrt(nrow(wa_wheat))
dfbetas_thresh <- 2 / sqrt(nrow(wa_wheat))

# Flag any observation with any DFBETAS exceeding the threshold
flag_dfbetas <- apply(abs(dfbetas_vals), 1, function(row) any(row > dfbetas_thresh))

# Identify observations flagged by any criterion
outlier_idx <- which(
  abs(student_resid) > stud_thresh |
    (leverage_vals > lev_thresh) |
    (abs(dffits_vals) > dffits_thresh) |
    flag_dfbetas
)

cat("Indices of potential outliers:", outlier_idx, "\n")
cat("Corresponding years:", 1950 + outlier_idx - 1, "\n\n")

# --------------------------------------------------
# Prediction for 1997 Using Model 3 on Subset Data
# --------------------------------------------------

# Subset the data for years up to 1996 (TIME <= 47)
subset_1996 <- subset(wa_wheat, time <= 47)

# Refit Model 3 using the subset data
model3_1996 <- lm(northampton ~ I(time^2), data = subset_1996)

# Create a new observation for TIME = 48 (1997)
new_time <- data.frame(time = 48)

# Generate a 95% prediction interval for 1997 yield
pred_int <- predict(model3_1996, newdata = new_time, interval = "prediction", level = 0.95)

# Obtain the actual yield for TIME = 48
actual_yield <- wa_wheat$northampton[wa_wheat$time == 48]

cat("95% Prediction Interval for 1997 Yield:\n")
cat("Lower Bound:", pred_int[1, "lwr"], "\n")
cat("Upper Bound:", pred_int[1, "upr"], "\n")
cat("Predicted Yield:", pred_int[1, "fit"], "\n")
cat("Actual Yield (1997):", actual_yield, "\n")
cat("Does the interval cover the actual yield? ",
    (actual_yield >= pred_int[1, "lwr"] & actual_yield <= pred_int[1, "upr"]), "\n")
