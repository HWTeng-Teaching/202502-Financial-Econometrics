library(POE5Rdata)
data("cex5_small")
?cex5_small

#a.
# Assuming cex5_small is loaded
library(ggplot2)  # For prettier plots

# Histogram
ggplot(cex5_small, aes(x = foodaway)) +
  geom_histogram(binwidth = 10, fill = "skyblue", color = "black", alpha = 0.7) +
  labs(title = "Histogram of FOODAWAY", x = "Food Away Expenditure ($)", y = "Frequency") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, size = 14, face = "bold"))

# Summary statistics
summary_stats <- summary(cex5_small$foodaway)
mean_foodaway <- mean(cex5_small$foodaway, na.rm = TRUE)
median_foodaway <- median(cex5_small$foodaway, na.rm = TRUE)
quantiles <- quantile(cex5_small$foodaway, probs = c(0.25, 0.75), na.rm = TRUE)

cat("Mean FOODAWAY:", mean_foodaway, "\n")
cat("Median FOODAWAY:", median_foodaway, "\n")
cat("25th Percentile:", quantiles[1], "\n")
cat("75th Percentile:", quantiles[2], "\n")

#b.
# Assuming EDUCATION variable exists (modify based on your dataset)
# Subset data
adv_degree <- subset(cex5_small, advanced==1)
college_degree <- subset(cex5_small, college==1)
no_degree <- subset(cex5_small, advanced==0 & college==0)

# Calculate mean and median
mean_adv <- mean(adv_degree$foodaway, na.rm = TRUE)
median_adv <- median(adv_degree$foodaway, na.rm = TRUE)
mean_college <- mean(college_degree$foodaway, na.rm = TRUE)
median_college <- median(college_degree$foodaway, na.rm = TRUE)
mean_none <- mean(no_degree$foodaway, na.rm = TRUE)
median_none <- median(no_degree$foodaway, na.rm = TRUE)

# Output results
cat("Advanced Degree - Mean:", mean_adv, "Median:", median_adv, "\n")
cat("College Degree - Mean:", mean_college, "Median:", median_college, "\n")
cat("No Degree - Mean:", mean_none, "Median:", median_none, "\n")

#c.
# Create ln(FOODAWAY)
cex5_small$ln_FOODAWAY <- log(cex5_small$foodaway)

# Histogram
ggplot(cex5_small, aes(x = ln_FOODAWAY)) +
  geom_histogram(binwidth = 0.2, fill = "lightgreen", color = "black", alpha = 0.7) +
  labs(title = "Histogram of ln(FOODAWAY)", x = "ln(Food Away Expenditure)", y = "Frequency") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, size = 14, face = "bold"))

# Summary statistics
summary_ln <- summary(cex5_small$ln_FOODAWAY)
mean_ln <- mean(cex5_small$ln_FOODAWAY, na.rm = TRUE)
median_ln <- median(cex5_small$ln_FOODAWAY, na.rm = TRUE)

cat("Mean ln(FOODAWAY):", mean_ln, "\n")
cat("Median ln(FOODAWAY):", median_ln, "\n")
cat("Number of observations in ln(FOODAWAY):", length(na.omit(cex5_small$ln_FOODAWAY)), "\n")

#d.
# Linear regression
clean_data <- subset(cex5_small, foodaway > 0 & !is.na(foodaway) & !is.na(income))
clean_data$ln_FOODAWAY <- log(clean_data$foodaway)

reg_model <- lm(ln_FOODAWAY ~ income, data = clean_data)
summary(reg_model)

# Extract coefficients
beta1 <- coef(reg_model)[1]  # Intercept
beta2 <- coef(reg_model)[2]  # Slope

cat("Intercept (β1):", beta1, "\n")
cat("Slope (β2):", beta2, "\n")

#e.
ggplot(clean_data, aes(x = income, y = ln_FOODAWAY)) +
  geom_point(color = "darkblue", alpha = 0.5, size = 2) +
  geom_smooth(method = "lm", color = "red", size = 1.5, se = FALSE) +
  labs(title = "ln(FOODAWAY) vs. INCOME with Fitted Line",
       x = "Income ($100 units)", y = "ln(Food Away Expenditure)") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
        axis.title = element_text(size = 12))

#f.
clean_data$residuals <- residuals(reg_model)
ggplot(clean_data, aes(x = income, y = residuals)) +
  geom_point(color = "purple", alpha = 0.5, size = 2) +
  geom_hline(yintercept = 0, color = "black", linetype = "dashed", size = 1) +
  labs(title = "Residuals vs. INCOME",
       x = "Income ($100 units)", y = "Residuals") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
        axis.title = element_text(size = 12))

