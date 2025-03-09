
#library(PoEdata)
#data("camp5")
url <- "https://www.principlesofeconometrics.com/poe5/data/rdata/capm5.rdata"
download.file(url, destfile = "capm5.rdata", mode = "wb")  # 下載檔案

# 1. Load data
load("capm5.rdata")

# 2. Compute excess market return
capm5$market_excess <- capm5$mkt - capm5$riskfree
firms <- c("ge", "ibm", "ford", "msft", "dis", "xom")

# 3. Create DataFrame to store regression results
regression_results <- data.frame(Firm = firms, Alpha = NA, Beta = NA, 
                                 R_Squared = NA, P_Value_Alpha = NA, P_Value_Beta = NA)

# 4. Create DataFrame for no-intercept model (α = 0)
regression_no_intercept <- data.frame(Firm = firms, Beta_No_Intercept = NA, 
                                      R_Squared = NA, P_Value_Beta = NA)

# 5. Run regressions for each firm
for (i in 1:length(firms)) {
  firm_excess <- capm5[[firms[i]]] - capm5$riskfree  # Compute excess return
  
  # (1) Standard CAPM regression (with alpha)
  model <- lm(firm_excess ~ capm5$market_excess)
  alpha <- coef(model)[1]  
  beta <- coef(model)[2]
  r_squared <- summary(model)$r.squared
  p_value_alpha <- summary(model)$coefficients[1,4]  # Alpha significance
  p_value_beta <- summary(model)$coefficients[2,4]   # Beta significance
  
  # Store in DataFrame
  regression_results[i, 2:6] <- c(alpha, beta, r_squared, p_value_alpha, p_value_beta)
  
  # (2) CAPM regression under the constraint α = 0
  model_no_intercept <- lm(firm_excess ~ capm5$market_excess + 0)
  beta_no_intercept <- coef(model_no_intercept)[1]
  r_squared_no_intercept <- summary(model_no_intercept)$r.squared
  p_value_beta_no_intercept <- summary(model_no_intercept)$coefficients[1,4]
  
  # Store in DataFrame
  regression_no_intercept[i, 2:4] <- c(beta_no_intercept, r_squared_no_intercept, p_value_beta_no_intercept)
}

# 6. Display standard CAPM results
print("📌 Standard CAPM Regression Results (with Alpha)")
print(regression_results)

# 7. Display no-intercept CAPM results
print("📌 No-Intercept CAPM Regression Results (α = 0)")
print(regression_no_intercept)

# 8. Compare beta estimates
beta_comparison <- data.frame(Firm = firms, Beta_With_Alpha = regression_results$Beta, 
                              Beta_No_Intercept = regression_no_intercept$Beta_No_Intercept,
                              Difference = regression_results$Beta - regression_no_intercept$Beta_No_Intercept)

print("📌 Comparison of Beta Estimates (With vs. Without Alpha)")
print(beta_comparison)

library(ggplot2)
msft_excess <- capm5$msft - capm5$riskfree

# 取出市場超額報酬
market_excess <- capm5$market_excess

# 建立線性回歸模型
lm_msft <- lm(msft_excess ~ market_excess)

# 繪製散點圖
plot(market_excess, msft_excess,
     main = "CAPM for Microsoft",
     xlab = "市場超額報酬",
     ylab = "MSFT超額報酬",
     pch = 19,       # 點的形狀
     col = "blue")   # 點的顏色
# 加入回歸直線
abline(lm_msft, col = "red", linetype = "dashed")
