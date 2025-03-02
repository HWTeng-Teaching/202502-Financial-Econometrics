![圖片](https://github.com/user-attachments/assets/1bfc746b-97d6-462d-be3c-16f9b5e92f57)

## a.
The capital asset pricing model (CAPM):\
$r_j - r_f = \alpha_j + \beta_j (r_m - r_f) + \epsilon_j$
\
The model is a simple regression model because it can be written as $y = \beta_1 + \beta_2 x +e$

$y = r_j - r_f , x = r_m - r_f , \beta_1 = \alpha ,\beta_2 = \beta_j$
\
The capital asset pricing model (CAPM) is an important model in the field of finance. It explains variations in the rate of return on a security as a function of the rate of return on a portfolio consisting of all publicly traded stocks, which is called the *market portfolio*. Generally, the rate of return on any investment is measured relative to its opportunity cost, which is the return on a risk-free asset. The resulting difference is called the *risk premium*, since it is the reward or punishment for making a risky investment. The CAPM says that the risk premium in security \( j \) is *proportional* to the risk premium on the market portfolio. That is,

$r_j - r_f = \beta_j (r_m - r_f)$

where \( r_j \) and \( r_f \) are the returns to security \( j \) and the risk-free rate, respectively, \( r_m \) is the return on the market portfolio, and \( \beta_j \) is the security’s *beta* value. A stock’s *beta* is important to investors since it reveals the stock’s volatility. It measures the sensitivity of security \( j \)’s return to variation in the whole stock market. As such, values of \( \beta \) less than one indicate that the stock is *defensive* since its variation is less than the market’s. A beta greater than one indicates an *aggressive stock*. Investors usually want an estimate of a stock’s *beta* before purchasing it. The CAPM model shown above is the *economic model* in this case. The *econometric model* is obtained by including an intercept in the model (even though theory says it should be zero) and an error term:

```
# Load necessary library
library(dplyr)

# Define firms
company <- c("ge", "ibm", "ford", "msft", "dis", "xom")

# Initialize storage for results
results <- data.frame(
  Firm = character(), 
  Alpha = numeric(), Alpha_SE = numeric(),
  Beta = numeric(), Beta_SE = numeric(), 
  N = integer(),
  stringsAsFactors = FALSE
)

# Run CAPM regression for each firm
for (i in company) {
  y <- capm5[[i]] - capm5$riskfree  # Firm's excess return
  x <- capm5$mkt - capm5$riskfree   # Market excess return
  
  # Perform linear regression
  model <- lm(y ~ x)
  
  # Extract coefficients and standard errors
  alpha <- coef(model)[1]
  beta <- coef(model)[2]
  alpha_se <- summary(model)$coefficients[1,2]
  beta_se <- summary(model)$coefficients[2,2]
  
  # Store results
  results <- rbind(results, data.frame(
    Firm = i, Alpha = alpha, Alpha_SE = alpha_se,
    Beta = beta, Beta_SE = beta_se, N = length(y)
  ))
}

# Format table for readability
results <- results %>%
  mutate(
    Alpha = sprintf("%.6f", Alpha), 
    Alpha_SE = sprintf("SE(%.5f)", Alpha_SE),  # Fix: Make row names unique
    Beta = sprintf("%.3f", Beta),
    Beta_SE = sprintf("SE(%.4f)", Beta_SE)     # Fix: Make row names unique
  ) %>%
  select(-Firm)  # Remove Firm column before transposing

# Transpose the table (convert columns to rows)
results_t <- as.data.frame(t(results))
colnames(results_t) <- company  # Set firm names as column headers

# Set unique row names
rownames(results_t) <- c("b1=\u03B1̂j", "\u03B1(SE)",
                         "b2=\u03B2̂j", "\u03B2(SE)", "N")

# Print the final transposed table
print(results_t)
```
| Metric      | ge        | ibm      | ford     | msft     | dis      | xom      |
|:-----------|:---------|:--------|:--------|:--------|:--------|:--------|
| **b₁ = αⱼ**  | -0.000959 | 0.006053 | 0.003779 | 0.003250 | 0.001047 | 0.005284 |
| **SE(αⱼ)**   | (0.00442) | (0.00483) | (0.01023) | (0.00604) | (0.00468) | (0.00354) |
| **b₂ = βⱼ**  | 1.148     | 0.977    | 1.662    | 1.202    | 1.012    | 0.457    |
| **SE(βⱼ)**   | (0.0895)  | (0.0978)  | (0.2069)  | (0.1222)  | (0.0946)  | (0.0716)  |
| **N**        | 180       | 180      | 180      | 180      | 180      | 180      |


