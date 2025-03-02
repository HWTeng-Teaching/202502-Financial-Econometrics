## 2.1
# Download packages
install.packages("ggplot2")
install.packages("dplyr")
library(ggplot2)
library(dplyr)

# Creates a data frame with two columns (x and y) and five rows
data <- data.frame(x = c(3, 2, 1, -1, 0), y = c(4, 2, 3, 1, 0))
N <- nrow(data)
print(data)

# a. Calculate sample means and table entries
# Compute the sample means
data <- data %>%
  mutate(
    x_bar = mean(x),
    y_bar = mean(y),
    x_minus_xbar = x - x_bar,
    x_minus_xbar_sq = (x - x_bar)^2,
    y_minus_ybar = y - y_bar,
    x_y_diff_prod = (x - x_bar) * (y - y_bar)
  )


print(data)


# Sums for the table
sum_row <- data.frame(
  x = sum(data$x),
  y = sum(data$y),
  x_minus_xbar = sum(data$x_minus_xbar),
  x_minus_xbar_sq = sum(data$x_minus_xbar_sq),
  y_minus_ybar = sum(data$y_minus_ybar),
  x_y_diff_prod = sum(data$x_y_diff_prod)
)

print(data)
# Store answers for part a
answers$a_table <- data[, c("x", "y", "x_minus_xbar", "x_minus_xbar_sq", "y_minus_ybar", "x_y_diff_prod")]
answers$a_sums <- sums_a
answers$a_means <- c(x_bar = data$x_bar[1], y_bar = data$y_bar[1])
print(answers$a_sums)

# b. Calculate b1 and b2 using OLS formulas
b2 <- sums_a$sum_x_y_diff_prod / sums_a$sum_x_minus_xbar_sq  # Slope
b1 <- data$y_bar[1] - b2 * data$x_bar[1]  # Intercept

# Print results
cat("b2 (Slope):", b2, "\n")
cat("b1 (Intercept):", b1, "\n")
# Store answers for part b
answers$b_coeffs <- c(b1 = b1, b2 = b2)
answers$b_regression <- paste("y_hat =", b1, "+", b2, "* x")

# c. Compute sums of squares and cross-products
sum_x_sq <- sum(data$x^2) # Sum of x_i^2
sum_x_y <- sum(data$x * data$y) # Sum of x_i * y_i

# Verify equation
# equation 1: ∑(xi - x̄)^2 = ∑xi^2 - N*x̄^2
sum_x_minus_xbar_sq_check <- sum_x_sq - N * data$x_bar[1]^2
should_match_x = sums_a$sum_x_minus_xbar_sq


# Verify equation 2: ∑(xi - x̄)(yi - ȳ) = ∑xi*yi - N*x̄*ȳ
sum_x_y_diff_prod_check <- sum_x_y - N * data$x_bar[1] * data$y_bar[1]
should_match_xy = sums_a$sum_x_y_diff_prod

# Print results
cat("LHS of equation 1:", sum_x_minus_xbar_sq_check, " | RHS of equation 1:", should_match_x, "\n")
cat("LHS of equation 2:", sum_x_y_diff_prod_check, " | RHS of equation 2:", should_match_xy, "\n")


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

print(answers$d_table)
print(answers$d_sums)
print(answers$d_stats)
print(median_x)


# Store the plot for part e
answers$e_plot <- plot

# f. Check if line passes through means
mean_point_check <- b1 + b2 * data$x_bar[1] == data$y_bar[1]
answers$f_check <- paste("The regression line passes through the mean point (", data$x_bar[1], ",", data$y_bar[1], "):", mean_point_check)
print(answers$f_check)
#g, h
check_g <- b1 + b2 * x_bar
check_h <- sums_d$sum_y_hat/N
y_bar == check_g
y_bar == check_h
# i. Compute sigma^2
sigma_sq_hat <- sums_d$sum_e_sq / (N - 2)
answers$i_sigma_sq <- sigma_sq_hat
print(answers$i_sigma_sq)
# j. Compute var(b2 | x) and se(b2)
var_b2 <- sigma_sq_hat / sums_a$sum_x_minus_xbar_sq
se_b2 <- sqrt(var_b2)
answers$j_stats <- c(var_b2 = var_b2, se_b2 = se_b2)
print(answers$j_stats)



## 2.14
# Given data
b2_rural <- 1.80  # Coefficient for EDUC in rural area
b2_urban <- 2.46  # Coefficient for EDUC in urban area
se_b2_urban <- 0.16  # Standard error of b2 in urban area
mean_wage_rural <- 19.74  # Mean WAGE in rural area (given)
mean_educ_urban <- 13.68  # Mean EDUC in urban area
intercept_rural <- -4.88  # Intercept for rural regression
intercept_urban <- -10.76  # Intercept for urban regression

#a. Compute Elasticity of Wages in Rural Area
# Compute mean EDUC in rural area using regression equation
mean_educ_rural <- (mean_wage_rural - intercept_rural) / b2_rural
print(mean_educ_rural)

# Compute elasticity
elasticity_rural <- (b2_rural * mean_educ_rural) / mean_wage_rural
print(elasticity_rural)


#b. Compute Standard Error of Elasticity in Urban Area
# Compute mean wage in urban area using its regression equation
mean_wage_urban <- intercept_urban + (b2_urban * mean_educ_urban)
# Compute standard error of elasticity for urban area
se_elasticity_urban <- (se_b2_urban * mean_educ_urban) / mean_wage_urban
print(se_elasticity_urban)

#c. Compute Predicted Wages for 12 and 16 Years of Education
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

print("Predicted Wages for 12 and 16 Years of Education:")
print(predicted_wages)




## 2.16
install.packages("broom")
library(broom)
# Define the URL of the capm5.rdata file
url <- "https://www.principlesofeconometrics.com/poe5/data/rdata/capm5.rdata"
# Define the destination file path
destfile <- "capm5.rdata"
# Download the file
download.file(url, destfile, mode = "wb")
# Load the downloaded RData file
load(destfile)
# Verify the loaded data
ls()  # List objects in the environment
head(capm5)  # Display the first few rows of the dataset

# a. OK
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

# c.
# Filter results for the intercept term (alpha_j)
alpha_values <- capm_results %>%
  filter(term == "(Intercept)") %>%
  select(stock, estimate, std.error, statistic, p.value)

# Display alpha values with p-values
print(alpha_values)

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

# Print beta comparison
print(beta_comparison)
