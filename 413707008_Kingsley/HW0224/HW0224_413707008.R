# Question 1
# Load required packages
library(ggplot2)
library(dplyr)

# Data
data <- data.frame(x = c(3, 2, 1, -1, 0), y = c(4, 2, 3, 1, 0))
N <- nrow(data)

# Initialize a list to store all answers
answers <- list()

# a. Calculate sample means and table entries
data <- data %>%
  mutate(
    x_bar = mean(x),
    y_bar = mean(y),
    x_minus_xbar = x - x_bar,
    x_minus_xbar_sq = (x - x_bar)^2,
    y_minus_ybar = y - y_bar,
    x_y_diff_prod = (x - x_bar) * (y - y_bar)
  )

# Sums for the table
sums_a <- data %>%
  summarise(
    sum_x = sum(x),
    sum_y = sum(y),
    sum_x_minus_xbar = sum(x_minus_xbar),
    sum_x_minus_xbar_sq = sum(x_minus_xbar_sq),
    sum_y_minus_ybar = sum(y_minus_ybar),
    sum_x_y_diff_prod = sum(x_y_diff_prod)
  )

# Store answers for part a
answers$a_table <- data[, c("x", "y", "x_minus_xbar", "x_minus_xbar_sq", "y_minus_ybar", "x_y_diff_prod")]
answers$a_sums <- sums_a
answers$a_means <- c(x_bar = data$x_bar[1], y_bar = data$y_bar[1])

# b. Calculate b1 and b2 using OLS formulas
b2 <- sums_a$sum_x_y_diff_prod / sums_a$sum_x_minus_xbar_sq  # Slope
b1 <- data$y_bar[1] - b2 * data$x_bar[1]  # Intercept

# Store answers for part b
answers$b_coeffs <- c(b1 = b1, b2 = b2)
answers$b_regression <- paste("y_hat =", b1, "+", b2, "* x")

# c. Compute sums of squares and cross-products
sum_x_sq <- sum(data$x^2)
sum_y_sq <- sum(data$y^2)
sum_x_y <- sum(data$x * data$y)

# Verify identities
sum_x_minus_xbar_sq_check <- sum_x_sq - N * data$x_bar[1]^2
sum_x_y_diff_prod_check <- sum_x_y - N * data$x_bar[1] * data$y_bar[1]

# Store answers for part c
answers$c_sums <- c(sum_x_sq = sum_x_sq, sum_y_sq = sum_y_sq, sum_x_y = sum_x_y)
answers$c_identities <- c(
  sum_x_minus_xbar_sq_check = sum_x_minus_xbar_sq_check,
  sum_x_y_diff_prod_check = sum_x_y_diff_prod_check,
  should_match_x = sums_a$sum_x_minus_xbar_sq,
  should_match_xy = sums_a$sum_x_y_diff_prod
)

# d. Fitted values, residuals, and statistics
data <- data %>%
  mutate(
    y_hat = b1 + b2 * x,
    e = y - y_hat,
    e_sq = e^2,
    x_e = x * e
  )

# Sums for the second table
sums_d <- data %>%
  summarise(
    sum_x = sum(x),
    sum_y = sum(y),
    sum_y_hat = sum(y_hat),
    sum_e = sum(e),
    sum_e_sq = sum(e_sq),
    sum_x_e = sum(x_e)
  )

# Sample variances and covariance (corrected s_y_sq)
s_y_sq <- sum((data$y - data$y_bar[1])^2) / (N - 1)  # Variance of y
s_x_sq <- sums_a$sum_x_minus_xbar_sq / (N - 1)  # Variance of x
s_xy <- sums_a$sum_x_y_diff_prod / (N - 1)  # Covariance of x and y

# Sample correlation
r_xy <- s_xy / (sqrt(s_x_sq) * sqrt(s_y_sq))

# Coefficient of variation of x
CV_x <- 100 * (sqrt(s_x_sq) / data$x_bar[1])

# Median and 50th percentile of x
median_x <- median(data$x)

# Store answers for part d
answers$d_table <- data[, c("x", "y", "y_hat", "e", "e_sq", "x_e")]
answers$d_sums <- sums_d
answers$d_stats <- c(
  s_y_sq = s_y_sq, s_x_sq = s_x_sq, s_xy = s_xy, r_xy = r_xy, CV_x = CV_x, median_x = median_x
)

# e. Plot data points and regression line using ggplot2
# Create a single-row data frame for the mean point
mean_point <- data.frame(x = data$x_bar[1], y = data$y_bar[1])

plot <- ggplot(data, aes(x = x, y = y)) +
  geom_point(size = 3, color = "blue", alpha = 0.7) +  # Data points
  geom_abline(intercept = b1, slope = b2, color = "red", size = 1.5) +  # Regression line
  geom_point(data = mean_point, aes(x = x, y = y), color = "green", size = 4, shape = 17) +  # Mean point using a separate data frame
  labs(title = "Data Points, Regression Line, and Mean Point",
       x = "x", y = "y") +
  theme_minimal() +
  annotate("text", x = data$x_bar[1], y = data$y_bar[1], label = "(x_bar, y_bar)", vjust = -1, color = "green")

# Store the plot for part e
answers$e_plot <- plot

# f. Check if line passes through means
mean_point_check <- b1 + b2 * data$x_bar[1] == data$y_bar[1]
answers$f_check <- paste("The regression line passes through the mean point (", data$x_bar[1], ",", data$y_bar[1], "):", mean_point_check)

# i. Compute sigma^2
sigma_sq_hat <- sums_d$sum_e_sq / (N - 2)
answers$i_sigma_sq <- sigma_sq_hat

# j. Compute var(b2 | x) and se(b2)
var_b2 <- sigma_sq_hat / sums_a$sum_x_minus_xbar_sq
se_b2 <- sqrt(var_b2)
answers$j_stats <- c(var_b2 = var_b2, se_b2 = se_b2)

# Print all answers at the end
cat("\n--- All Answers for the Exercise ---\n\n")

# Part a
cat("a. Table Results and Sample Means:\n")
print(answers$a_table)
cat("\nSums:\n")
print(answers$a_sums)
cat("Sample means: x_bar =", answers$a_means["x_bar"], ", y_bar =", answers$a_means["y_bar"], "\n\n")

# Part b
cat("b. OLS Estimates:\n")
cat("b2 (slope) =", answers$b_coeffs["b2"], "\n")
cat("b1 (intercept) =", answers$b_coeffs["b1"], "\n")
cat(answers$b_regression, "\n\n")

# Part c
cat("c. Sums and Identity Verifications:\n")
cat("Sum x_i^2:", answers$c_sums["sum_x_sq"], "\n")
cat("Sum y_i^2:", answers$c_sums["sum_y_sq"], "\n")
cat("Sum x_i * y_i:", answers$c_sums["sum_x_y"], "\n")
cat("Sum (x_i - x_bar)^2 check:", answers$c_identities["sum_x_minus_xbar_sq_check"], " (should match", answers$c_identities["should_match_x"], ")\n")
cat("Sum (x_i - x_bar)(y_i - y_bar) check:", answers$c_identities["sum_x_y_diff_prod_check"], " (should match", answers$c_identities["should_match_xy"], ")\n\n")

# Part d
cat("d. Fitted Values, Residuals, and Statistics:\n")
print(answers$d_table)
cat("\nSums:\n")
print(answers$d_sums)
cat("Statistics:\n")
cat("Sample variance of y (s_y^2):", answers$d_stats["s_y_sq"], "\n")
cat("Sample variance of x (s_x^2):", answers$d_stats["s_x_sq"], "\n")
cat("Sample covariance of x and y (s_xy):", answers$d_stats["s_xy"], "\n")
cat("Sample correlation (r_xy):", answers$d_stats["r_xy"], "\n")
cat("Coefficient of variation of x (CV_x, %):", answers$d_stats["CV_x"], "\n")
cat("Median (50th percentile) of x:", answers$d_stats["median_x"], "\n\n")

# Part e
cat("e. Plot of Data Points and Regression Line:\n")
print(answers$e_plot)

# Part f
cat("\nf. Regression Line and Mean Point:\n")
cat(answers$f_check, "\n\n")

# Part g
check_g <- b1 + b2 * x_bar
check_g == y_bar

# Part h
check_h <- sums_d$sum_y_hat/N
check_h == y_bar

# Part i
cat("i. Estimated Variance (sigma^2):\n")
cat("sigma^2 =", answers$i_sigma_sq, "\n\n")

# Part j
cat("j. Variance and Standard Error of b2:\n")
cat("Variance of b2 | x:", answers$j_stats["var_b2"], "\n")
cat("Standard error of b2:", answers$j_stats["se_b2"], "\n")

# Question 14
# Given values
b2_rural <- 1.80  # Coefficient for EDUC in rural area
b2_urban <- 2.46  # Coefficient for EDUC in urban area
se_b2_urban <- 0.16  # Standard error of b2 in urban area
mean_wage_rural <- 19.74  # Mean WAGE in rural area (given)
mean_educ_urban <- 13.68  # Mean EDUC in urban area
intercept_rural <- -4.88  # Intercept for rural regression
intercept_urban <- -10.76  # Intercept for urban regression

### (a) Compute Elasticity of Wages in Rural Area
# Compute mean EDUC in rural area using regression equation
mean_educ_rural <- (mean_wage_rural - intercept_rural) / b2_rural

# Compute elasticity
elasticity_rural <- (b2_rural * mean_educ_rural) / mean_wage_rural

# Print results for part (a)
print(paste("Mean years of education in the rural area:", round(mean_educ_rural, 3)))
print(paste("Elasticity of wages in the rural area:", round(elasticity_rural, 3)))

### (b) Compute Standard Error of Elasticity in Urban Area
# Compute mean wage in urban area using its regression equation
mean_wage_urban <- intercept_urban + (b2_urban * mean_educ_urban)

# Compute standard error of elasticity for urban area
se_elasticity_urban <- (se_b2_urban * mean_educ_urban) / mean_wage_urban

# Print result for part (b)
print(paste("Mean wage in urban area:", round(mean_wage_urban, 3)))
print(paste("Standard error of elasticity in urban area:", round(se_elasticity_urban, 3)))

### (c) Compute Predicted Wages for 12 and 16 Years of Education
# Function to compute predicted wage
predicted_wage <- function(educ, intercept, slope) {
  return(intercept + slope * educ)
}

# Compute predicted wages for given education levels
educ_years <- c(12, 16)
predicted_wages <- data.frame(
  Education_Years = educ_years,
  Urban_Wage = sapply(educ_years, function(educ) predicted_wage(educ, intercept_urban, b2_urban)),
  Rural_Wage = sapply(educ_years, function(educ) predicted_wage(educ, intercept_rural, b2_rural))
)

# Print predicted wages for part (c)
print("Predicted Wages for 12 and 16 Years of Education:")
print(predicted_wages)

# Question 16
# Define the URL
url <- "https://www.principlesofeconometrics.com/poe5/data/rdata/capm5.rdata"
# Open a connection to the URL
con <- url(url, "rb")  # "rb" = read binary mode
# Load the RData file directly from the web
load(con)
#load("C:/Users/user/AppData/Local/Temp/7zO4D193FC2/capm5.rdata")

# Load required libraries
library(tidyverse)
library(broom)

# a. Explain why the econometric model above is a simple regression model like those discussed in this chapter.

# b.
# Compute excess returns (subtract risk-free rate)
capm5 <- capm5 %>%
  mutate(
    ge_excess = ge - riskfree,
    ibm_excess = ibm - riskfree,
    ford_excess = ford - riskfree,
    msft_excess = msft - riskfree,
    dis_excess = dis - riskfree,
    xom_excess = xom - riskfree,
    mkt_excess = mkt - riskfree
  )

# Function to run CAPM regression and extract coefficients
model <- lm(ge_excess ~ mkt_excess, data = capm5)
summary(model)

run_capm <- function(stock_excess, market_excess) {
  model <- lm(stock_excess ~ market_excess, data = capm5)
  return(tidy(model))  # Returns coefficients and significance levels
}

# Run CAPM for each firm
ge_capm <- run_capm(capm5$ge_excess, capm5$mkt_excess)
ibm_capm <- run_capm(capm5$ibm_excess, capm5$mkt_excess)
ford_capm <- run_capm(capm5$ford_excess, capm5$mkt_excess)
msft_capm <- run_capm(capm5$msft_excess, capm5$mkt_excess)
dis_capm <- run_capm(capm5$dis_excess, capm5$mkt_excess)
xom_capm <- run_capm(capm5$xom_excess, capm5$mkt_excess)

# Combine results into a summary table
capm_results <- bind_rows(
  ge_capm %>% mutate(stock = "GE"),
  ibm_capm %>% mutate(stock = "IBM"),
  ford_capm %>% mutate(stock = "Ford"),
  msft_capm %>% mutate(stock = "Microsoft"),
  dis_capm %>% mutate(stock = "Disney"),
  xom_capm %>% mutate(stock = "ExxonMobil")
) %>%
  select(stock, term, estimate, std.error, statistic, p.value)

# Display the results
print(capm_results)

# Analyze the beta values
beta_values <- capm_results %>%
  filter(term == "market_excess") %>%
  arrange(desc(estimate)) %>% mutate(type = ifelse(estimate > 1, "aggressive", "defensive"))

print(beta_values)

# Identify the most aggressive and defensive stock
most_aggressive <- beta_values %>% filter(estimate == max(estimate))
most_defensive <- beta_values %>% filter(estimate == min(estimate))

print(most_aggressive)  # Highest beta
print(most_defensive)  # Lowest beta

# c.
# Filter results for the intercept term (alpha_j)
alpha_values <- capm_results %>%
  filter(term == "(Intercept)") %>%
  select(stock, estimate, std.error, statistic, p.value)

# Display alpha values with p-values
print(alpha_values)

# Check if alpha is significantly different from zero (other options: 0.1 and 0.01)
significant_alphas <- alpha_values %>%
  filter(p.value < 0.05)

# Display stocks with significant alphas
print(significant_alphas)

# Conclusion based on significance
if (nrow(significant_alphas) == 0) {
  print("All intercept estimates are not significantly different from zero, supporting CAPM theory.")
} else {
  print("Some intercepts are significantly different from zero, suggesting additional factors influence returns.")
}

# Scatter plot with regression line
ggplot(capm5, aes(x = mkt_excess, y = msft_excess)) +
  geom_point(alpha = 0.6) +  # Scatter points
  geom_smooth(method = "lm", color = "blue", se = FALSE) +  # Regression line
  labs(title = "CAPM Regression: Microsoft Stock",
       x = "Market Excess Return",
       y = "Microsoft Excess Return") +
  theme_minimal()

# d.
# Function to run CAPM regression without intercept
run_capm_no_intercept <- function(stock_excess, market_excess) {
  model <- lm(stock_excess ~ market_excess + 0, data = capm5)  # No intercept
  return(tidy(model))  # Extract coefficients
}

# Run regression without intercept for each firm
ge_capm_no_intercept <- run_capm_no_intercept(capm5$ge_excess, capm5$mkt_excess)
ibm_capm_no_intercept <- run_capm_no_intercept(capm5$ibm_excess, capm5$mkt_excess)
ford_capm_no_intercept <- run_capm_no_intercept(capm5$ford_excess, capm5$mkt_excess)
msft_capm_no_intercept <- run_capm_no_intercept(capm5$msft_excess, capm5$mkt_excess)
dis_capm_no_intercept <- run_capm_no_intercept(capm5$dis_excess, capm5$mkt_excess)
xom_capm_no_intercept <- run_capm_no_intercept(capm5$xom_excess, capm5$mkt_excess)

# Combine results
capm_results_no_intercept <- bind_rows(
  ge_capm_no_intercept %>% mutate(stock = "GE"),
  ibm_capm_no_intercept %>% mutate(stock = "IBM"),
  ford_capm_no_intercept %>% mutate(stock = "Ford"),
  msft_capm_no_intercept %>% mutate(stock = "Microsoft"),
  dis_capm_no_intercept %>% mutate(stock = "Disney"),
  xom_capm_no_intercept %>% mutate(stock = "ExxonMobil")
) %>%
  select(stock, term, estimate, std.error, statistic, p.value)

# Display the results
print(capm_results_no_intercept)

# Compare beta estimates with and without intercept
beta_comparison <- capm_results %>%
  filter(term == "market_excess") %>%
  select(stock, estimate) %>%
  rename(beta_with_intercept = estimate) %>%
  left_join(
    capm_results_no_intercept %>%
      filter(term == "market_excess") %>%
      select(stock, estimate) %>%
      rename(beta_without_intercept = estimate),
    by = "stock"
  )

# Display beta comparison
print(beta_comparison)
# Beta values remain similar, it indicates that the intercept does not strongly influence the estimates.
