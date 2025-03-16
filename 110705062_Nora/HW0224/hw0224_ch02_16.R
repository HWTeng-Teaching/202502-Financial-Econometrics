# Download and load the dataset
url <- "https://www.principlesofeconometrics.com/poe5/data/rdata/capm5.rdata"
download.file(url, destfile = "capm5.rdata", mode = "wb") 
load("capm5.rdata")
capm5$market_excess <- capm5$mkt - capm5$riskfree
firms <- c("ge", "ibm", "ford", "msft", "dis", "xom")

## 2.16.2 and 4
regression_results <- data.frame(Firm = firms, Alpha = NA, Beta = NA, 
                                 R_Squared = NA, P_Value_Alpha = NA, P_Value_Beta = NA)
regression_no_intercept <- data.frame(Firm = firms, Beta_No_Intercept = NA, 
                                      R_Squared = NA, P_Value_Beta = NA)
for (i in 1:length(firms)) {
  firm_excess <- capm5[[firms[i]]] - capm5$riskfree 
  
  model <- lm(firm_excess ~ capm5$market_excess)
  alpha <- coef(model)[1]  
  beta <- coef(model)[2]
  r_squared <- summary(model)$r.squared
  p_value_alpha <- summary(model)$coefficients[1,4] 
  p_value_beta <- summary(model)$coefficients[2,4]   
  regression_results[i, 2:6] <- c(alpha, beta, r_squared, p_value_alpha, p_value_beta)

  model_no_intercept <- lm(firm_excess ~ capm5$market_excess + 0)
  beta_no_intercept <- coef(model_no_intercept)[1]
  r_squared_no_intercept <- summary(model_no_intercept)$r.squared
  p_value_beta_no_intercept <- summary(model_no_intercept)$coefficients[1,4]
  regression_no_intercept[i, 2:4] <- c(beta_no_intercept, r_squared_no_intercept, p_value_beta_no_intercept)
}
print("Standard CAPM Regression Results (with Alpha)")
print(regression_results)

## 2.16.4
print("No-Intercept CAPM Regression Results (α = 0)")
print(regression_no_intercept)
beta_comparison <- data.frame(Firm = firms, Beta_With_Alpha = regression_results$Beta, 
                              Beta_No_Intercept = regression_no_intercept$Beta_No_Intercept,
                              Difference = regression_results$Beta - regression_no_intercept$Beta_No_Intercept)

print("Comparison of Beta Estimates (With vs. Without Alpha)")
print(beta_comparison)

## 2.16.3
msft_excess <- capm5$msft - capm5$riskfree
market_excess <- capm5$market_excess
lm_msft <- lm(msft_excess ~ market_excess)

plot(market_excess, msft_excess,
     main = "CAPM for Microsoft",
     xlab = "市場超額報酬",
     ylab = "MSFT超額報酬",
     pch = 19,      
     col = "green")  
abline(lm_msft, col = "red", lty = "dashed")