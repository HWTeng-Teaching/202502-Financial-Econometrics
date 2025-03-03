install.packages("POE5Rdata")
library(POE5Rdata)
data(capm5)

cam =  c("ge", "ibm", "ford", "msft", "dis", "xom")

#b小題
for (i in cam) {
  y = capm5[[i]] - capm5$riskfree
  x = capm5$mkt - capm5$riskfree
  l_model = lm(y~x)
  beta = coef(l_model)[2]
  cat('cam:',i,"\n","beta=",sprintf("%.6f",beta),"\n")
}


#c小題
rf = capm5$riskfree
rm = capm5$mkt
rj = capm5$msft

rj_c = rj - rf
rm_c = rm - rf

capm_model = lm(rj_c ~rm_c)
summary(capm_model)

plot(rm_c, rj_c, main = "CAPM Model for Microsoft", xlab = "Market Excess Return", ylab = "Microsoft Excess Return", col = "blue")
abline(capm_model, col = "red")

#d小題
for(i in cam){
  y = capm5[[i]] - capm5$riskfree
  x = capm5$mkt - capm5$riskfree
  model = lm(y~x - 1)
  beta2 = coef(model)[1]
  
  cat("com", i , "\n")
  cat("Slpoe", beta2, "\n")
  }
