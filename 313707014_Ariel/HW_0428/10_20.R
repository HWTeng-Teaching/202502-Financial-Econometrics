#10.20
# 313707014 陳紀蓁

library(POE5Rdata)
library(tidyverse)
library(sandwich)
library(lmtest)
library(sandwich)
library(car)
library(AER)
library(stargazer)

data ("capm5")
summary(capm5)
head(capm5)



#a. 先估計出回歸式
capm5$excess_msft <- capm5$msft - capm5$riskfree
capm5$excess_rm <- capm5$mkt - capm5$riskfree
capm5$excess_rm


capm_ols <- lm(excess_msft ~ excess_rm, data = capm5)
summary(capm_ols)

#b. 用rank作為工具變數
capm5$rank <- rank(capm5$excess_rm) #排序由小到大
capm5$rank

capm5.firstrank <- lm(excess_rm ~ rank , data= capm5)
summary(capm5.firstrank)

linearHypothesis(capm5.firstrank, c("rank = 0")) #會希望工具變數跟rm有關係，看F檢定，F>10代表是強工具變數

#c.把剛剛第一階段的殘差加進去跑回歸，如果顯著，那excess_rm就是內生的

capm5$rankhat <- residuals(capm5.firstrank)
capm5.second <- lm(excess_msft ~ excess_rm+rankhat, data = capm5)
summary(capm5.second)

linearHypothesis(capm5.second, c("rankhat = 0"))


#d. rank作為 IV 去跑 2SLS
capm5.rank <- ivreg(excess_msft ~ excess_rm |
                      rank , data=capm5)
summary(capm5.rank)

#e.建立POS變數, first stage regression 有遇到問題？？

capm5$POS <- ifelse(capm5$excess_rm > 0, 1, 0)


capm5.firstrp <- lm(excess_rm ~ rank+ POS , data= capm5)
summary(capm5.firstrp)

linearHypothesis(capm5.firstrp, c(" rank = 0", "POS = 0" ))




#f.

 取出殘差
capm5$vhat2 <- residuals(capm5.firstrp)

#增強的 OLS 回歸：加入 vhat2 檢定內生性
hausman_model <- lm(excess_msft ~ excess_rm + vhat2, data = capm5)
summary(hausman_model)


#g. 2SLS POS rank
capm5.rp <- ivreg(excess_msft ~ excess_rm |
                      rank + POS , data=capm5)
summary(capm5.rp)

confint(capm5.rp )


#h.
capm5$vhat3 <- residuals(capm5.rp)


sargan_aux <- lm(vhat3 ~ rank + POS, data = capm5)
summary(sargan_aux)













