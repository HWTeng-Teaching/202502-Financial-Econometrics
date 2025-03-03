# 先下載套件和 data
install.packages("remotes")
library(remotes)
remotes::install_github("ccolonescu/POE5Rdata", force = TRUE)
library(POE5Rdata)

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


x <- capm5$mkt - capm5$riskfree
y <- capm5$msft - capm5$riskfree

tab <- lm(y ~ x)

mean_x <- mean(x)
mean_y <- mean(y)


plot(x, y, 
     main = "MSFT Regression", 
     xlab = "Market Excess Return", 
     ylab = "MSFT Excess Return", 
     col = "blue", pch = 16, cex = 0.8)


abline(tab, col = "red", lwd = 1.6)

points(mean_x, mean_y, col = "green", pch = 17, cex = 1.5)
text(mean_x, mean_y, labels = "Mean", pos = 4, col = "green", cex = 1)

grid()
