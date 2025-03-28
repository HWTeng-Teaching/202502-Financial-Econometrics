library(POE5Rdata)
POE5Rdata::cocaine
install.packages("xtable")
library(xtable)
install.packages("knitr")   
library(knitr)
#5.23(b)
mod1 <- lm(price~quant+qual+trend,data = cocaine)
s <- summary(mod1)
s
coefs <- round(s$coefficients[,1], 4)
ses   <- round(s$coefficients[,2], 4)
ts    <- round(s$coefficients[,3], 4)
r2    <- round(s$r.squared, 4)

cat(paste0( #paste0用來把多個文字合併成一串,只寫paste不會有空格
  "PRICE = ", coefs[1], 
  ifelse(coefs[2] >= 0, " + ", " - "), abs(coefs[2]), "*QUANT", #若係數是正的就印出正
  ifelse(coefs[3] >= 0, " + ", " - "), abs(coefs[3]), "*QUAL",
  ifelse(coefs[4] >= 0, " + ", " - "), abs(coefs[4]), "*TREND",
  "     R² = ", r2, "\n\n",  #這是你計算出來的 R-squared 值
  
  "(se)    ", paste("(", ses, ")", collapse = " "), "\n",  #"(" 一段, ses 一段, ")" 一段
  "(t)     ", paste("(", ts, ")", collapse = " "), "\n"  #collaspe 才能把全部併成一句
))

#5.23(d)(e)
smod1 <- data.frame(xtable(summary(mod1)))
kable(smod1,
     caption = "The multiple regression model of 5.23",
     col.names = c("coefficient","std.error","t-value","p-value"),
     align = "c",digits = 3)
