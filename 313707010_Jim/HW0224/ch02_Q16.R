if (!requireNamespace("remotes", quietly = TRUE)) install.packages("remotes")
remotes::install_github("ccolonescu/POE5Rdata", force = TRUE)

library(POE5Rdata)
data("capm5")

data$MKT_rp <- data$mkt - data$riskfree
stocks <- c("ge", "ibm", "ford", "msft", "dis", "xom")
for (stock in stocks) {
  data[[stock]] <- data[[stock]] - data$riskfree
}

run_regression <- function(dep_var, data, intercept = TRUE) {
  formula <- as.formula(paste(dep_var, "~", if (intercept) "MKT_rp" else "0 + MKT_rp"))
  lm(formula, data = data)
}

# (b) cal beta
print("(b)")
models <- setNames(lapply(stocks, run_regression, data = data), stocks)

beta_values <- sapply(models, function(m) coef(m)[2])
print(beta_values)
cat("most aggressive:", names(which.max(beta_values)), "\n")
cat("most defensive", names(which.min(beta_values)), "\n")

# (c) 
print("(c)")
alpha_results <- data.frame(Stock = stocks, Alpha = sapply(models, function(m) coef(m)[1]),
                            P_Value = sapply(models, function(m) coef(summary(m))[1, 4]))
alpha_results$Significant <- ifelse(alpha_results$P_Value < 0.05, "Yes", "No")
print(alpha_results)

plot(data$MKT_rp, data$msft, main = "Microsoft's CAPM regression", 
     xlab = "market risk premium (mkt_rp)", ylab = "Microsoft risk premium", 
     col = "blue", pch = 19)

model_msft <- lm(msft ~ MKT_rp, data = data)
abline(model_msft, col = "red")

# (d)
print('(d)')
models_no_intercept <- setNames(lapply(stocks, run_regression, data = data, intercept = FALSE), stocks)
beta_no_intercept <- sapply(models_no_intercept, function(m) coef(m)[1])

comparison <- data.frame(Stock = stocks, Beta_with_intercept = beta_values, Beta_no_intercept = beta_no_intercept)
comparison$Difference <- comparison$Beta_with_intercept - comparison$Beta_no_intercept
print(comparison)
