#5.33
# 313707014 陳紀蓁

library(POE5Rdata)
library(xtable)
library(knitr)

data ("cps5_small")
summary(cps5_small)
head(cps5_small)

#a

mod1 <- lm(log(wage) ~ educ+I(educ^2)+exper+I(exper^2)+I(educ*exper), data=cps5_small)
smod1 <- data.frame(xtable(summary(mod1)))
kable(smod1, 
      caption="The basic multiple regression model", 
      col.names=c("coefficient", "Std. Error", "t-value", "p-value"),
      align="c", digits=3)

s <- summary(mod1)
s

#b
b <- coef(mod1)
marginal_educ <- with(cps5_small, b["educ"] + 2 * b["I(educ^2)"] * educ + b["I(educ * exper)"] * exper)
marginal_educ

#c.
summary(marginal_educ)
quantile(marginal_educ, probs = c(0.05, 0.5, 0.95))
hist(marginal_educ, main = "Marginal Effect of EDUC", xlab = "Effect")

#d.
marginal_exper <- with(cps5_small, b["exper"] + 2 * b["I(exper^2)"] * exper + b["I(educ * exper)"] * educ)
marginal_exper

#e.
summary(marginal_exper)
quantile(marginal_exper, probs = c(0.05, 0.5, 0.95))
hist(marginal_exper, main = "Marginal Effect of EXPER", xlab = "Effect")


#f
S <- data.frame(educ = 16, exper = 18)
pred_S <- predict(mod1, newdata = S, interval = "confidence", level = 0.95)
print(pred_S) 

D <- data.frame(educ = 17, exper = 8)
pred_D <- predict(mod1, newdata = D, interval = "confidence", level = 0.95)
print(pred_D) 


vcov_matrix <- vcov(mod1)
c_vec <- c(0, -1,16^2-17^2,10, 18^2-8^2, 16*18-17*8)  
(sef_g <- sqrt(t(c_vec) %*% vcov_matrix %*% c_vec))
sef_g



t <- (pred_S[1]-pred_D[1])/ sef_g
t



alpha <- 0.05
df <- mod1$df.residual
tcr <- qt(alpha, df) 
tcr


#g.
#f
S_8 <- data.frame(educ = 16, exper = 26)
pred_S_8 <- predict(mod1, newdata = S_8, interval = "confidence", level = 0.95)
print(pred_S_8) 

D_8 <- data.frame(educ = 17, exper = 16)
pred_D_8 <- predict(mod1, newdata = D_8, interval = "confidence", level = 0.95)
print(pred_D_8) 


vcov_matrix <- vcov(mod1)
c_vec <- c(0, -1,16^2-17^2, 10, 26^2-16^2, 16*26-17*16)  
(sef_g <- sqrt(t(c_vec) %*% vcov_matrix %*% c_vec))
sef_g


t <- (pred_S_8[1]-pred_D_8[1])/ sef_g
t




#h
b <- coef(mod1)
b

#ME <-  b4 + 2*b5*exper + b6*edu

W <- b['exper'] + 2*b['I(exper^2)'] *17 + b['I(educ * exper)']*12
J <- b['exper'] + 2*b['I(exper^2)'] *11+ b['I(educ * exper)']*16

vcov_matrix <- vcov(mod1)
c_vec <- c(0, 0,0,0,  12, -4)  
(sef_g <- sqrt(t(c_vec) %*% vcov_matrix %*% c_vec))
sef_g


t <- (W-J)/ sef_g
t


alpha <- 0.05
df <- mod1$df.residual
tcr <- qt(alpha/2, df) 
tcr

#i
J <- b['exper'] + 2*b['I(exper^2)'] *11+ b['I(educ * exper)']*16


exper <- -(b['I(educ * exper)']*16 + b['exper']) / (2*b['I(exper^2)'])
exper
exper_zero <- exper -11
exper_zero

b <- coef(mod1)


b4 <- b["exper"]
b5 <- b['I(exper^2)']
b6 <- b["I(educ * exper)"]


g_b4 <- -1 / (2 * b5)
g_b5 <- (b4 + 16 * b6) / (2 * b5^2)
g_b6 <- -16 / (2 * b5)

g_b4
cov_matrix <- vcov(mod1)



se_root <- sqrt(g_b4^2*cov_matrix[4,4] +
                g_b5^2*cov_matrix[5,5]+
                g_b6^2*cov_matrix[6,6]+
                2*g_b4*g_b5*cov_matrix[4,5]+
                2*g_b4*g_b6*cov_matrix[4,6]+
                2*g_b6*g_b5*cov_matrix[6,5])
se_root


alpha <- 0.05
df <- mod1$df.residual
tcr <- qt(1- (alpha/2), df) 
tcr

# 建構信賴區間
lower <- exper_zero - tcr * se_root
upper <- exper_zero + tcr* se_root
lower
upper
