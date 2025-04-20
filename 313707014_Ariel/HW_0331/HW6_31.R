#5.31
# 313707014 陳紀蓁

library(POE5Rdata)
library(xtable)
library(knitr)

data ("commute5")
summary(commute5)
head(commute5)

#a

mod1 <- lm(time ~ depart+reds+trains, data=commute5)
smod1 <- data.frame(xtable(summary(mod1)))
kable(smod1, 
      caption="The basic multiple regression model", 
      col.names=c("coefficient", "Std. Error", "t-value", "p-value"),
      align="c", digits=3)

s <- summary(mod1)
s

#b
alpha <- 0.05
confints <- confint(mod1,  level=0.95)
confints

#c
b3 <- coef(mod1)["reds"]
se_b3 <- summary(mod1)$coefficients["reds", "Std. Error"]
t <- (b3 - 2) / se_b3
t

alpha <- 0.05
df <- mod1$df.residual
tcr <- qt(alpha, df) 
tcr


#d
b4 <- coef(mod1)["trains"]
se_b4 <- summary(mod1)$coefficients["trains", "Std. Error"]
t <- (b4 - 3) / se_b4
t

alpha <- 0.1
df <- mod1$df.residual
tcr_10 <- qt(alpha/2, df) 
tcr_10


#e
b2 <- coef(mod1)["depart"]
se_b2 <- summary(mod1)$coefficients["depart", "Std. Error"]
beta_diff <- 30 * b2
se_diff <- 30 * se_b2

t <- (beta_diff - 10) / se_diff
t

alpha <- 0.05
df <- mod1$df.residual
tcr <- qt(alpha, df) 
tcr


#f

beta_diff_e <- b4 - 3*b3

var_b3 <- se_b3^2
var_b4 <- se_b4^2
cov_b3b4 <- vcov(mod1)["reds", "trains"]

sef <- (9*var_b3 + var_b4 -2*3*cov_b3b4)^0.5

t <- (beta_diff_e) / sef
t

alpha <- 0.05
df <- mod1$df.residual
tcr <- qt(alpha, df) 
tcr



#g

xg <- data.frame(depart = 30, reds = 6, trains = 1)
pred_g <- predict(mod1, newdata = xg, interval = "confidence", level = 0.95)
print(pred_g)  # 得出b1 + 30*b2 + 6*b3 + b4


vcov_matrix <- vcov(mod1)
c_vec <- c(1, 30, 6, 1)  
(sef_g <- sqrt(t(c_vec) %*% vcov_matrix %*% c_vec))
sef_g

t <- (pred_g[1] - 45) /sef_g
t

alpha <- 0.05
df <- mod1$df.residual
tcr <- qt(1-alpha, df) 
tcr





