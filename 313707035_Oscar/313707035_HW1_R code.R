# 安裝並載入必要的套件 
if (!requireNamespace("remotes", quietly = TRUE)) install.packages("remotes")
remotes::install_github("ccolonescu/POE5Rdata", force = TRUE)

# 載入 CAPM 數據
library(POE5Rdata)
data("capm5")

# 定義要分析的公司
company <- c("ge", "ibm", "ford", "msft", "dis", "xom")

# Load necessary libraries
library(ggplot2)
library(dplyr)
library(broom)

# (a) Compute excess returns
capm5 <- capm5 %>%
  mutate(
    GE_excess = ge - riskfree,
    IBM_excess = ibm - riskfree,
    Ford_excess = ford - riskfree,
    Microsoft_excess = msft - riskfree,
    Disney_excess = dis - riskfree,
    ExxonMobil_excess = xom - riskfree,
    Market_excess = mkt - riskfree
  )

# (b) Function to run regression for each firm
run_regression <- function(stock_excess) {
  lm(stock_excess ~ Market_excess, data = capm5)
}

# Run regressions for each firm
ge_model <- run_regression(capm5$GE_excess)
ibm_model <- run_regression(capm5$IBM_excess)
ford_model <- run_regression(capm5$Ford_excess)
msft_model <- run_regression(capm5$Microsoft_excess)
disney_model <- run_regression(capm5$Disney_excess)
exxon_model <- run_regression(capm5$ExxonMobil_excess)

# Summarize results
ge_summary <- tidy(ge_model)
ibm_summary <- tidy(ibm_model)
ford_summary <- tidy(ford_model)
msft_summary <- tidy(msft_model)
disney_summary <- tidy(disney_model)
exxon_summary <- tidy(exxon_model)

# Print beta values for each firm
print("(b) Beta values:")
print(data.frame(
  Firm = c("GE", "IBM", "Ford", "Microsoft", "Disney", "ExxonMobil"),
  Beta = c(ge_summary$estimate[2], ibm_summary$estimate[2], ford_summary$estimate[2],
           msft_summary$estimate[2], disney_summary$estimate[2], exxon_summary$estimate[2])
))

# Identify most aggressive and defensive stock
betas <- data.frame(
  Firm = c("GE", "IBM", "Ford", "Microsoft", "Disney", "ExxonMobil"),
  Beta = c(ge_summary$estimate[2], ibm_summary$estimate[2], ford_summary$estimate[2],
           msft_summary$estimate[2], disney_summary$estimate[2], exxon_summary$estimate[2])
)

most_aggressive <- betas[which.max(betas$Beta), ]
most_defensive <- betas[which.min(betas$Beta), ]
print(paste("(b) Most aggressive stock:", most_aggressive$Firm))
print(paste("(b) Most defensive stock:", most_defensive$Firm))

# (c) Hypothesis test for alpha_j = 0 (checking intercept significance)
alpha_significance <- data.frame(
  Firm = c("GE", "IBM", "Ford", "Microsoft", "Disney", "ExxonMobil"),
  Alpha_pvalue = c(ge_summary$p.value[1], ibm_summary$p.value[1], ford_summary$p.value[1],
                    msft_summary$p.value[1], disney_summary$p.value[1], exxon_summary$p.value[1])
)
print("(c) P-values for intercepts (alpha):")
print(alpha_significance)

# Scatter plot with regression line for Microsoft
ggplot(capm5, aes(x = Market_excess, y = Microsoft_excess)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", col = "blue") +
  labs(title = "(c) CAPM Regression for Microsoft", x = "Market Excess Return", y = "Microsoft Excess Return")

# (d) Re-estimate model with alpha_j = 0 (forcing intercept to zero)
run_regression_no_intercept <- function(stock_excess) {
  lm(stock_excess ~ Market_excess - 1, data = capm5) # Remove intercept
}

ge_model_no_int <- run_regression_no_intercept(capm5$GE_excess)
ibm_model_no_int <- run_regression_no_intercept(capm5$IBM_excess)
ford_model_no_int <- run_regression_no_intercept(capm5$Ford_excess)
msft_model_no_int <- run_regression_no_intercept(capm5$Microsoft_excess)
disney_model_no_int <- run_regression_no_intercept(capm5$Disney_excess)
exxon_model_no_int <- run_regression_no_intercept(capm5$ExxonMobil_excess)

# Compare new beta estimates
print("(d) Beta values without intercept:")
print(data.frame(
  Firm = c("GE", "IBM", "Ford", "Microsoft", "Disney", "ExxonMobil"),
  Beta_No_Intercept = c(tidy(ge_model_no_int)$estimate[1], tidy(ibm_model_no_int)$estimate[1], 
                        tidy(ford_model_no_int)$estimate[1], tidy(msft_model_no_int)$estimate[1],
                        tidy(disney_model_no_int)$estimate[1], tidy(exxon_model_no_int)$estimate[1])
))
