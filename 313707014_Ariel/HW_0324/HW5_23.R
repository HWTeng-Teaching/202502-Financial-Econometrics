#5.23
# 313707014 陳紀蓁

library(POE5Rdata)
library(xtable)
library(knitr)


data ("cocaine")
summary('cocaine')

#PRICE= price per gram in dollars for a cocaine sale 每公克售價
#QUANT= number of grams of cocaine in a given sale 幾公克
#QUAL= quality of the cocaine expressed as percentage purity 純度
#TREND= a time variable with 1984= 1 up to 1991= 8 


#b.
mod1 <- lm(price ~ quant+qual+trend, data=cocaine)
smod1 <- data.frame(xtable(summary(mod1)))
kable(smod1, 
      caption="The basic multiple regression model", 
      col.names=c("coefficient", "Std. Error", "t-value", "p-value"),
      align="c", digits=3)

s <- summary(mod1)
s

#c
r <- summary(mod1)$r.squared
r

#d 假設檢定 對於一次交易大量會不會比較便宜，結果是會
# H1: b2 < 0

alpha <- 0.05
df <- mod1$df.residual
tcr <- qt(alpha, df) #這邊在算95信心水準的t值，單尾
tcr
b2 <- coef(mod1)[["quant"]]

seb <- summary(mod1)$coefficients["quant","Std. Error"] 

#seb2 <- sqrt(vcov(mod1)[2,2]) 這樣寫也可以
t <- b2/seb2
t



#e. 純度是否會影響價格，結果是不拒絕虛無假設
# H0: b3 < 0; H1: b3 > 0
alpha <- 0.05
df <- mod1$df.residual
tcr <- qt(alpha, df) #信心水準雙尾
tcr

b3 <- coef(mod1)[["qual"]]
seb3 <- summary(mod1)$coefficients["qual","Std. Error"] 

t <- b3/seb3
t

#f.年價格的變化
year_price <- coef(mod1)['trend']
year_price








