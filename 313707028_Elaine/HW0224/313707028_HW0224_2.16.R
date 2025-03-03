#2.16
data("capm5")
company <- c("ge", "ibm", "ford", "msft", "dis", "xom")
for (i in company) {
  y <- capm5[[i]] - capm5$riskfree
  x <- capm5$mkt - capm5$riskfree
  tab <- lm(y ~ x)
  
  cat("Company:", i, "\n")
  cat("Intercept:", coef(tab)[1], "\n")
  cat("Slope:", coef(tab)[2], "\n")

  print(summary(tab))
  cat("-------------------------\n")
}
plot(capm5$mkt - capm5$riskfree,capm5$msft - capm5$riskfree, main = "MSFT Regression", xlab = "Market Excess Return", ylab = "MSFT Excess Return")
abline(tab, col = "red")
?lm
for (i in company) {
  y <- capm5[[i]] - capm5$riskfree
  x <- capm5$mkt - capm5$riskfree
  tab <- lm(y ~ x - 1)
  
  cat("Company:", i, "\n")
  cat("Slope:", coef(tab)[1], "\n")
  cat("-------------------------\n")
}
