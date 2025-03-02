#匯入資料
#devtools::install_github("ccolonescu/POE5Rdata")#下載資料
library(POE5Rdata)
data("capm5")

#查看資料及選取觀察公司備份
?capm5
stocks <- c("ge", "ibm", "ford", "msft", "dis", "xom")
capm_y = capm5

#修改x, y值
for(stock in stocks){
  capm_y[[stock]] = capm_y[[stock]] - capm_y$riskfree
}
capm_x = capm5$mkt - capm5$riskfree 
capm_y$capm_x = capm_x

#繪製散步圖觀察資料
#?plot
for(stock in stocks){
  plot(capm_y$capm_x, capm_y[[stock]], main = stock, xlab = "rm-rf", ylab = "rj-rf")
}

#建立表格儲存參數值
par = data.frame(matrix(0, 3, length(stocks)))
rownames(par) = c("beta", "alpha", "betaWithoutAlpha")
colnames(par) = stocks

#linear model
for(stock in stocks){
  mod = lm(capm_y[[stock]] ~ capm_x, data = capm_y)
  modd = lm(capm_y[[stock]] ~ 0 + capm_x, data = capm_y)#移除截距項
  par[1, stock] = coef(mod)[2] #slope(beta)
  par[2, stock] = coef(mod)[1] #intercept(alpha)
  par[3, stock] = coef(modd)[1] #betaWithoutAlpha
}

#畫圖
plot(capm_y$capm_x, capm_y$msft, main = "RL&scatter", xlab = "rm-rf(msft)", ylab = "rj-rf(msft)")
b1 = par[1, "msft"] #slope(beta)
b2 = par[2, "msft"] #intercept(alpha)
b3 = par[3, "msft"] #betaWithoutAlpha
#?abline
abline(b2, b1, col = "red")	
abline(0, b3, col = "green")

# 圖例
legend("topleft", 
       legend = c("With Intercept", "Without Intercept"), 
       col = c("red", "green"), 
       lwd = 2, lty = 1, 
       bty = "n")  # bty="n" 移除圖例邊框

#捨棄多於資料
capm_y = capm_y[ , c(stocks, "capm_x")]
