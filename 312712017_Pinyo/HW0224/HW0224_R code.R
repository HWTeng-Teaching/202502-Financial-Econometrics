# Define the file path (Update this with your actual path)
file_path <- "D:/Master Degree at NYCU/1. Slides and Homeworks/Spring_2025/Financial Econometrics/capm5.csv"  # Modify this!

# Load the dataset safely
if (file.exists(file_path)) {
  capm_data <- read.csv(file_path)
} else {
  cat("File not found at the specified path. Please choose the file manually.\n")
  capm_data <- read.csv(file.choose())  # Opens a file selection dialog
}

# Display first few rows to confirm successful load
head(capm_data)

# Attach data for easy column reference
attach(capm_data)

# List of companies
companies <- c("ge", "ibm", "ford", "msft", "dis", "xom")

# Create a list to store models
models <- list()
betas <- c()

# Loop through each company to estimate CAPM model
for (company in companies) {
  formula <- as.formula(paste(company, "- riskfree ~ mkt - riskfree"))
  model <- lm(formula, data=capm_data)
  models[[company]] <- model  # Store model
  betas[company] <- coef(model)[2]  # Store beta coefficient
  
  # Print results
  cat("\nCAPM Regression for", company, ":\n")
  print(summary(model))
}

# Find the most aggressive and defensive firms
most_aggressive <- names(betas)[which.max(betas)]
most_defensive <- names(betas)[which.min(betas)]

cat("\nMost Aggressive Stock:", most_aggressive, "(Highest Beta =", max(betas), ")\n")
cat("Most Defensive Stock:", most_defensive, "(Lowest Beta =", min(betas), ")\n")

###################################### Q16_c ############################################

# Load necessary libraries
library(ggplot2)

# Load the data (Modify the path if needed)
file_path <- "D:/Master Degree at NYCU/1. Slides and Homeworks/Spring_2025/Financial Econometrics/capm5.csv"  # Change this to your correct path
capm_data <- read.csv(file_path)

# Extract Microsoft returns and calculate excess returns
capm_data$MSFT_Excess <- capm_data$msft - capm_data$riskfree
capm_data$MKT_Excess <- capm_data$mkt - capm_data$riskfree

# Run CAPM regression: Microsoft Excess Return ~ Market Excess Return
msft_model <- lm(MSFT_Excess ~ MKT_Excess, data = capm_data)

# Show summary results (Check αj and βj values)
summary(msft_model)

# Extract coefficients
alpha <- coef(msft_model)[1]  # Intercept (α)
beta <- coef(msft_model)[2]   # Beta (β)

# Check if α ≈ 0
cat("Alpha (Intercept) for Microsoft:", alpha, "\n")
if (abs(alpha) < 0.05) {
  cat("Alpha is very close to zero, supporting the CAPM assumption.\n")
} else {
  cat("Alpha is not zero, contradicting the CAPM assumption.\n")
}

# Plot scatter plot with regression line
ggplot(capm_data, aes(x = MKT_Excess, y = MSFT_Excess)) +
  geom_point(color = "blue") +  # Data points
  geom_abline(intercept = alpha, slope = beta, color = "red", size = 1.2) +  # Regression line
  labs(title = "Microsoft CAPM Regression",
       x = "Market Excess Return (MKT - RISKFREE)",
       y = "Microsoft Excess Return (MSFT - RISKFREE)") +
  theme_minimal()


