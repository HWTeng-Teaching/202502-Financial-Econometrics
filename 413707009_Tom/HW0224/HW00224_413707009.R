#C02Q01a

x <- c(3,2,1,-1,0)
y <- c(4,2,3,1,0)

df <- data.frame(x,y)
df

xbar <- mean(x)
ybar <- mean(y)

df$xm <- df$x - xbar
df$xm2 <- (df$xm)^2
df$ym <- df$y - ybar
df$ym2 <- NULL
df$xym <- df$xm * df$ym

df_sums <- colSums(df)
df <- rbind(df, df_sums)

#C02Q01b

b2 <- df[nrow(df), 6] /df[nrow(df), 4]
b1 <- ybar - b2*xbar

#C02Q01c

sum_x_sq <- sum(x^2)
sum_xm2 <- sum((x-xbar)^2)
sum_xy <- sum(x*y)
N <- length(x)
lhs <- sum_xm2
rhs <- sum_x_sq - N*xbar^2
cat("LHS =", lhs, " | RHS =", rhs, "\n")  # Compare
sum_xy <- sum(x*y)

sum_cp1 <- sum((x-xbar)*(y-ybar))
sum_cp2 <- sum(x*y) -N*xbar*ybar

cat("sum_cp1 =", sum_cp1, " | sum_cp2 =", sum_cp2, "\n")

#C02Q01d

yhat <- b1 + b2*x
e <- y - yhat

# 4. Build a data frame
df2 <- data.frame(
  x = x,
  y = y,
  yhat = yhat,
  e    = e,
  e2   = e^2,
  xe   = x*e
)

df2_sums <- colSums(df2)
df2 <- rbind(df2, df2_sums)


sx2 <- sum_xm2/(N-1)   # s_x^2
sy2 <- sum( (y - ybar)^2 )/(N-1)
sx  <- sqrt(sx2)
sy  <- sqrt(sy2)

sxy <- sum( (x - xbar)*(y - ybar) )/(N-1)  # covariance
rxy <- sxy / (sx*sy)                      # correlation

CVx <- 100*(sx/xbar)                      # coefficient of variation
med_x <- median(x)

#C02Q01e

plot(x, y,
     main="(e) Scatterplot with Fitted Regression Line",
     xlab="x", ylab="y",
     pch=19)             # pch=19 for solid circles
abline(b1, b2, col="red", lwd=2)

#C02Q01f

points(mean(x), mean(y),
       pch=8,   # star symbol
       col="blue",
       cex=1.5) # slightly bigger
text(mean(x), mean(y),
     labels="(x̄, ȳ)",
     pos=4, col="blue")  # pos=4 = label to the right

#C02Q01g

ybar1 <- b1 +b2*xbar
mean_y <- mean(y)




SSE  <- sum(e^2)         # sum of squared residuals
mean_yhat <- mean(yhat)
mean_y    <- mean(y)

sigma2 <- SSE / (N - 2)

Sxx  <- sum( (x - xbar)^2 )
Var_b2 <- sigma2 / Sxx
se_b2  <- sqrt(Var_b2)



#C02Q14a
# -- For Rural regression:
b0_rural  <- -4.88   # intercept
b1_rural  <-  1.80   # slope
se_b1_rur <-  0.24   # std. error of slope

# -- For Urban regression:
b0_urban  <- -10.76  # intercept
b1_urban  <-  2.46   # slope
se_b1_urb <-  0.16   # std. error of slope

mean_WAGE <- 19.74
mean_EDUC_rural <- (b0_rural-mean_WAGE)/(-b1_rural)
elasticity_rural <- b1_rural * (mean_EDUC_rural / mean_WAGE)
elasticity_rural

mean_EDUC_urban <- (b0_urban-mean_WAGE)/(-b1_urban)
elasticity_urban <- b1_urban * (mean_EDUC_urban / mean_WAGE)
elasticity_urban


#b
xbar_urban <- 13.68
se_elasticity_urban <- (xbar_urban / mean_WAGE) * se_b1_urb
se_elasticity_urban

#c

wage_urban_12 <- b0_urban + b1_urban*12
wage_urban_12
wage_urban_16 <- b0_urban + b1_urban*16
wage_urban_16

wage_rural_12 <- b0_rural + b1_rural*12
wage_rural_12
wage_rural_16 <- b0_rural + b1_rural*16
wage_rural_16

#16.a
library(tidyverse)
library(readxl)
capm5 <- read_excel("/Users/tom/Downloads/poe5xlsx/capm5.xlsx", sheet = 1)

capm5$ge_excess        <- capm5$ge        - capm5$riskfree
capm5$ibm_excess        <- capm5$ibm        - capm5$riskfree
capm5$ford_excess       <- capm5$ford       - capm5$riskfree
capm5$msft_excess  <- capm5$msft  - capm5$riskfree
capm5$dis_excess     <- capm5$dis     - capm5$riskfree
capm5$xom_excess      <- capm5$xom      - capm5$riskfree
capm5$mkt_excess        <- capm5$mkt        - capm5$riskfree

model <- lm(ge_excess ~ mkt_excess, data = capm5)
summary(model)

library(broom)
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

library(dplyr)                 # Nạp gói

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

beta_values <- capm_results %>%
  filter(term == "market_excess") %>%
  arrange(desc(estimate)) %>% mutate(type = ifelse(estimate > 1, "aggressive", "defensive"))

print(beta_values)

# Identify the most aggressive and defensive stock
most_aggressive <- beta_values %>% filter(estimate == max(estimate))
most_defensive <- beta_values %>% filter(estimate == min(estimate))

print(most_aggressive)  # Highest beta
print(most_defensive)  # Lowest beta


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
  geom_smooth(method = "lm", color = "red", se = FALSE) +  # Regression line
  labs(title = "CAPM Regression: Microsoft Stock",
       x = "Market Excess Return",
       y = "Microsoft Excess Return") +
  theme_minimal()

#d.
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