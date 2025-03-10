library(POE5Rdata)
data("cps5_small")
?cps5_small

library(ggplot2)  # For prettier plots
#a.
# Summary statistics
summary_wage <- summary(cps5_small$wage)
summary_educ <- summary(cps5_small$educ)
cat("Summary Statistics for WAGE:\n", summary_wage, "\n")
cat("Summary Statistics for EDUC:\n", summary_educ, "\n")

# Histogram for WAGE
ggplot(cps5_small, aes(x = wage)) +
  geom_histogram(binwidth = 2, fill = "skyblue", color = "black", alpha = 0.7) +
  labs(title = "Histogram of Hourly Wage", x = "Wage ($/hour)", y = "Frequency") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, size = 14, face = "bold"))

# Histogram for EDUC
ggplot(cps5_small, aes(x = educ)) +
  geom_histogram(binwidth = 1, fill = "lightgreen", color = "black", alpha = 0.7) +
  labs(title = "Histogram of Education", x = "Years of Education", y = "Frequency") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, size = 14, face = "bold"))

#b.
# Linear regression
linear_model <- lm(wage ~ educ, data = cps5_small)
summary(linear_model)

# Extract coefficients
beta1 <- coef(linear_model)[1]  # Intercept
beta2 <- coef(linear_model)[2]  # Slope

cat("Intercept (β1):", beta1, "\n")
cat("Slope (β2):", beta2, "\n")


#c.
# Calculate residuals
cps5_small$residuals <- residuals(linear_model)

# Residual plot
ggplot(cps5_small, aes(x = educ, y = residuals)) +
  geom_point(color = "purple", alpha = 0.5, size = 2) +
  geom_hline(yintercept = 0, color = "black", linetype = "dashed", size = 1) +
  labs(title = "Residuals vs. EDUC", x = "Years of Education", y = "Residuals") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
        axis.title = element_text(size = 12))

#d.
# Assuming GENDER and RACE variables exist (modify based on your dataset)
# Subset data
males <- subset(cps5_small, female==0)
females <- subset(cps5_small, female==1)
blacks <- subset(cps5_small, black==1)
whites <- subset(cps5_small,  black==0)

# Regressions
model_males <- lm(wage ~ educ, data = males)
model_females <- lm(wage ~ educ, data = females)
model_blacks <- lm(wage ~ educ, data = blacks)
model_whites <- lm(wage ~ educ, data = whites)

# Extract coefficients
beta1_males <- coef(model_males)[1]
beta2_males <- coef(model_males)[2]
beta1_females <- coef(model_females)[1]
beta2_females <- coef(model_females)[2]
beta1_blacks <- coef(model_blacks)[1]
beta2_blacks <- coef(model_blacks)[2]
beta1_whites <- coef(model_whites)[1]
beta2_whites <- coef(model_whites)[2]

# Present as regression equations
cat("Regression Equations:\n",
"Males:   WAGE = ", round(coef(model_males)[1], 2), " + ", round(coef(model_males)[2], 2), "*EDUC\n",
"Females: WAGE = ", round(coef(model_females)[1], 2), " + ", round(coef(model_females)[2], 2), "*EDUC\n",
"Blacks:  WAGE = ", round(coef(model_blacks)[1], 2), " + ", round(coef(model_blacks)[2], 2), "*EDUC\n",
"Whites:  WAGE = ", round(coef(model_whites)[1], 2), " + ", round(coef(model_whites)[2], 2), "*EDUC\n",
    sep = "")

#e.
# Create EDUC squared
cps5_small$EDUC2 <- cps5_small$educ^2

# Quadratic regression
quad_model <- lm(wage ~ EDUC2, data = cps5_small)
summary(quad_model)

# Extract coefficients
alpha1 <- coef(quad_model)[1]  # Intercept
alpha2 <- coef(quad_model)[2]  # Coefficient of EDUC²

# Marginal effects
# Derivative: dWAGE/dEDUC = 2 * α2 * EDUC
marginal_12 <- 2 * alpha2 * 12  # At EDUC = 12
marginal_16 <- 2 * alpha2 * 16  # At EDUC = 16

cat("Marginal effect at EDUC = 12:", marginal_12, "\n")
cat("Marginal effect at EDUC = 16:", marginal_16, "\n")
cat("Marginal effect from linear model (β2):", beta2, "\n")

#f.
# Generate fitted values
cps5_small$linear_fit <- predict(linear_model)
cps5_small$quad_fit <- predict(quad_model)

# Plot with both fitted lines
# Ensure ggplot2 is loaded
library(ggplot2)

# Plot with both fitted lines
ggplot(data = cps5_small, aes(x = educ, y = wage)) +
  # Scatter plot of data points
  geom_point(color = "darkblue", alpha = 0.3, size = 2) +
  # Fitted line for linear model
  geom_line(aes(y = linear_fit), color = "red", size = 1.5, linetype = "solid") +
  # Fitted line for quadratic model
  geom_line(aes(y = quad_fit), color = "green", size = 1.5, linetype = "dashed") +
  # Add labels for the lines using annotate()
  
  # Alternative y positions (manually set)
  annotate("text", x = max(cps5_small$educ) - 2, y = 30,  # Adjust y as needed
           label = "Linear", color = "red", size = 5, hjust = 0) +
  annotate("text", x = max(cps5_small$educ) - 2, y = 50,  # Adjust y as needed
           label = "Quadratic", color = "green", size = 5, hjust = 0)

  # Customize plot labels and theme
  labs(title = "Linear vs. Quadratic Fit: WAGE vs. EDUC",
       x = "Years of Education", y = "Hourly Wage ($)") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
        axis.title = element_text(size = 12))

