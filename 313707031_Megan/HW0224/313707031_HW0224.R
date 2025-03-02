#2.16 (b)
if (!requireNamespace("remotes", quietly = TRUE)) install.packages("remotes")
remotes::install_github("ccolonescu/POE5Rdata", force = TRUE)

library(POE5Rdata)
data("capm5")

firm <- c("ge", "ibm", "ford", "msft", "dis", "xom")

for (i in firm) {
  y <- capm5[[i]] - capm5$riskfree
  x <- capm5$mkt - capm5$riskfree
  model <- lm(y~x)
  beta<-coef(model)[2]
  cat('firm:',i,"\n","beta=",sprintf("%.6f",beta),"\n")
}

#2.16 (c)
y_msft <- capm5$msft-capm5$riskfree
x_msft <- capm5$mkt-capm5$riskfree
modelmsft <- lm(y_msft~x_msft)
summary(modelmsft)

plot(y_msft, x_msft, main = "Microsoft stock", 
     xlab = "Market Excess Return", 
     ylab = "MSFT Excess Return",pch=16,col="blue")

abline(modelmsft,col='red',lwd=2)

#2.16 (d)
for (i in firm) {
  y <- capm5[[i]] - capm5$riskfree
  x <- capm5$mkt - capm5$riskfree
  model <- lm(y~x-1)
  beta<-coef(model)[1]
  cat('firm:',i,"\n",'beta=',sprintf("%.6f",beta),"\n")
}
