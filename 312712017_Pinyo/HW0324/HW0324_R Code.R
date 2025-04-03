##################################### C05Q23_b################################################

# Load the dataset (assuming the file is named "cocaine.csv")
data <- read.csv("C:/Users/PINYKEWD/Documents/cocaine.csv")

# Estimate the regression model
model <- lm(price ~ quant + qual + trend, data = data)

# Output the summary of the regression results
summary(model)

##################################### C05Q23_c################################################

# Load necessary libraries
library(readr)

# Assuming the data is in a CSV file named 'cocaine.csv'
# Load the data
data <- read_csv("C:/Users/PINYKEWD/Documents/cocaine.csv")

# Fit the model with lowercase column names
model <- lm(price ~ quant + qual + trend, data = data)

# Get the summary of the model
summary(model)

# Extract R-squared value
r_squared <- summary(model)$r.squared

# Print R-squared value
print(paste("R-squared: ", round(r_squared, 4)))

# Calculate the proportion of variation explained
proportion_explained <- r_squared * 100

# Print the proportion of variation explained
print(paste("Proportion of variation explained: ", round(proportion_explained, 2), "%"))

##################################### C05Q23_e################################################

# Load necessary libraries
library(readr)

# Load the data
data <- read_csv("C:/Users/PINYKEWD/Documents/cocaine.csv")

# Fit the model
model <- lm(price ~ quant + qual + trend, data = data)

# Get the summary of the model
summary_model <- summary(model)

# Extract the coefficient and p-value for qual
qual_coefficient <- coef(summary_model)["qual", "Estimate"]
qual_p_value <- coef(summary_model)["qual", "Pr(>|t|)"]

# Since this is a one-sided test, we need to adjust the p-value
# For a one-sided test, we only consider the upper tail (since H1: β3 > 0)
qual_p_value_one_sided <- qual_p_value / 2

# Print results
print(paste("Coefficient for qual: ", round(qual_coefficient, 4)))
print(paste("One-sided p-value for qual: ", qual_p_value_one_sided))

# Interpret the results
if (qual_p_value_one_sided < 0.05 & qual_coefficient > 0) {
  print("Reject H₀: There is a significant positive relationship between quality and price.")
} else {
  print("Fail to reject H₀: No significant positive relationship between quality and price.")
}

##################################### C05Q23_f################################################

# Load necessary libraries
library(readr)
library(dplyr)
library(ggplot2)

# Load the data
data <- read_csv("C:/Users/PINYKEWD/Documents/cocaine.csv")

# Calculate average price for each year (assuming trend represents years)
data_yearly_avg <- data %>%
  group_by(trend) %>%
  summarise(avg_price = mean(price))

# Plot the average price over time
ggplot(data_yearly_avg, aes(x = trend, y = avg_price)) +
  geom_point(color = "blue") +  # Scatter plot of average prices
  geom_line(color = "red") +  # Line connecting average prices
  labs(
    title = "Average Cocaine Price Over Time",
    x = "Year (Trend)",
    y = "Average Price per Gram ($)"
  ) +
  theme_minimal()

# Calculate the average annual change in price
annual_change <- coef(lm(avg_price ~ trend, data = data_yearly_avg))[2]
print(paste("Average annual change in price: ", round(annual_change, 4)))

