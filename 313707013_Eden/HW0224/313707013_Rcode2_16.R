#2.16 b
#讀取資料集
data("capm5")

#指定
company <- c("ge", "ibm", "ford", "msft", "dis", "xom")

#計算每間公司之迴歸參數
for (i in company) {
  y <- capm5[[i]] - capm5$riskfree
  x <- capm5$mkt - capm5$riskfree
  tab <- lm(y ~ x)
  
  cat("Company:", i, "\n")
  cat("Regression model",':',"y","=", coef(tab)[1],"+",coef(tab)[2],"x", "\n")

  print(summary(tab))
  cat("-------------------------\n")
}

#2.16 c
plot(capm5$mkt - capm5$riskfree, capm5$msft - capm5$riskfree,
     main = "MSFT Regression", xlab = "Market Excess Return", 
     ylab = "MSFT Excess Return", pch = 16, col = "blue")
abline(tab, col = "red", lwd = 2)
legend("topleft", legend = "Regression Line", col = "red", lwd = 2)

#2.16 d
?lm
for (i in company) {
  y <- capm5[[i]] - capm5$riskfree
  x <- capm5$mkt - capm5$riskfree
  tab <- lm(y ~ x - 1)
  
  cat("Company:", i, "\n")
  cat("Slope:", coef(tab)[1], "\n")
  cat("-------------------------\n")
}

