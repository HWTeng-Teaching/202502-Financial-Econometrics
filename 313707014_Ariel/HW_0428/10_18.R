#10.18
# 313707014 陳紀蓁

library(POE5Rdata)
library(tidyverse)
library(sandwich)
library(lmtest)
library(sandwich)
library(car)
library(AER)
library(stargazer)

data ("mroz")
summary(mroz)
head(mroz)

#a. 找出父母學歷

mroz$mothercoll <- ifelse(mroz$mothereduc > 12, 1, 0)

mroz$fathercoll <- ifelse(mroz$fathereduc > 12, 1, 0)


mean(mroz$mothercoll)  
mean(mroz$fathercoll) 



#b. 找到mothercoll 、fathercoll 與EDUC的相關性
cor(mroz[, c("educ", "mothercoll", "fathercoll")], use = "complete.obs")

#c 工具變數 mothercoll/直接去算2SLS的標準差會是錯的，但是直接套function的會是對的
mroz$wage
mroz$educ



# 篩選：這些欄位不能是 NA，也不能是 0
mroz_clean <- subset(mroz, wage > 0)

mroz_clean$wage
summary(mroz_clean)



mroz.ols <- lm(log(wage) ~ educ+exper+I(exper^2), data=mroz_clean)
summary(mroz.ols)

mroz.ivmocoll <- ivreg(log(wage)~ educ+exper+I(exper^2) |
                    exper+I(exper^2)+mothercoll, data=mroz_clean)
summary(mroz.ivmocoll)


confint(mroz.ivmocoll)


#d.mothercoll第一階段回歸

mroz.firstm <- lm(educ~exper+I(exper^2)+mothercoll, data=mroz_clean)
summary(mroz.firstm)

linearHypothesis(mroz.firstm, c("mothercoll = 0")) #會希望工具變數跟WAGE有關係，看F檢定，F>10代表是強工具變數


#e mothercoll fathercoll 作為工具變數
mroz.ivmofacoll <- ivreg(log(wage)~ educ+exper+I(exper^2) |
                         exper+I(exper^2)+mothercoll+fathercoll, data=mroz_clean)
summary(mroz.ivmofacoll)

confint(mroz.ivmocoll)


#f.
mroz.firstmf <- lm(educ~exper+I(exper^2)+mothercoll+fathercoll, data=mroz_clean)
summary(mroz.firstmf)

linearHypothesis(mroz.firstmf, c("mothercoll = 0","fathercoll = 0"))



#g.判斷是否過度識別

# Hansen's J test

hansen_j <- summary(mroz.ivmofacoll, diagnostics = TRUE)
hansen_j


stargazer(mroz.ols, mroz.ivmocoll, mroz.firstm,mroz.ivmofacoll,mroz.firstmf,
          title = "Wage equation: OLS, IV mothercoll, mroz.firstm ,IV mothercoll and fathercoll,mroz.firstmf,",
          header = FALSE,
          type = "text",   # <<--- 這裡是關鍵
          keep.stat = "n",
          omit.table.layout = "n",
          star.cutoffs = NA,
          digits = 5,
          intercept.bottom = FALSE,
          column.labels = c("OLS", "IV mothercoll","fist mothercoll", 
                            "IV mothercoll fathercoll","first mothercoll fathercoll"),
          dep.var.labels.include = FALSE,
          model.numbers = FALSE,
          dep.var.caption = "Dependent variable: wage",
          model.names = FALSE,
          star.char = NULL)





