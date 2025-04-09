library(POE5Rdata)
data <- POE5Rdata::commute5

#Q5.31.a
mod1 <- lm(time ~ depart+reds+trains, data = data)
summary(mod1)
#Q5.31.b
ci <- confint(mod1 , level = 0.95)

b <- summary(mod1)$coefficients[,'Estimate']
se <- summary(mod1)$coefficients[,'Std. Error']

#Q5.31.c H0: B3 = 2, H1: B3 < 2
t_c <- (b[3]-2)/se[3]
p_c <- pt(t_c , df = df.residual(mod1)) 
t_c
p_c

#Q5.31.d H0: B4 = 3, H1: B4 =/ 3
t_d <- (b[4]-3)/se[4]
p_d <- 2* pt(t_d , df = df.residual(mod1)) 
t_d
p_d

#Q5.31.e H0: B2 > 1/3, H1: B4 < 1/3
t_e <- (b[2]-(1/3))/se[2]
p_e <- pt(t_e , df = df.residual(mod1)) 
t_e
p_e

#Q5.31.f H0: B4 - 3B3 > 0, H1: B4 - 3B3 < 0
vcov <- vcov(mod1)
se_f <- (vcov[4,4] + 9*vcov[3,3] + 2*-3*vcov[3,4])^0.5
t_f <- (b[4]-3*b[3])/se_f
p_f <- pt(t_f , df = df.residual(mod1))
t_f
p_f

#Q5.31.g H0: B1 + 30B2 + 6B3 + B4 < 45
w_g <- c(1,30,6,1)
se_g <- sqrt(t(w_g) %*% vcov %*% w_g)
t_g <- as.numeric((b[1]+ 30*b[2] + 6*b[3] + b[4] - 45)/se_g)
p_g <- as.numeric(1-pt(t_g , df = df.residual(mod1)))
t_g
p_g

#Q5.31.e H0: B1 + 30B2 + 6B3 + B4 > 45
p_h <- as.numeric(pt(t_g , df = df.residual(mod1)))
p_h

#Q5.33.a
data <- POE5Rdata::cps5_small
mod1 <- lm(log(wage) ~ educ + I(educ^2) + exper + I(exper^2) + I(educ*exper), data = data)
summary(mod1)

#Q5.33.c
b = summary(mod1)$coefficients[,'Estimate']
me <- b[2] + 2*b[3]*data$educ + b[6]*data$exper
hist(me, main = 'Histogram of Marginal effects', xlab = 'Marginal effects')
cat(quantile(me,0.05),'\n')
cat(quantile(me,0.5),'\n')
cat(quantile(me,0.95),'\n')

#Q5.33.e
me2 <- b[4] + 2*b[5]*data$exper + b[6]*data$educ
hist(me2, main = 'Histogram of Marginal effects', xlab = 'Marginal effects')
cat(quantile(me2,0.05),'\n')
cat(quantile(me2,0.5),'\n')
cat(quantile(me2,0.95),'\n')

#Q5.33.f
vcov <- vcov(mod1)
w <- c(0,1,33,-10,-260,-152)
se <- sqrt(t(w) %*% vcov %*% w)
t <- (b[2] + 33*b[3] - 10*b[4] - 260*b[5] -152*b[6])/se
p <- 1-pt(t , df = df.residual(mod1))
cat(t,'\n')
cat(p,'\n')

#Q5.33.g
w2 <- c(0,1,33,-10,-420,-144)
se2 <- sqrt(t(w2) %*% vcov %*% w2)
t2 <- (b[2] + 33*b[3] - 10*b[4] - 420*b[5] -144*b[6])/se2
p2 <- 1-pt(t2 , df = df.residual(mod1))
cat(t2,'\n')
cat(p2,'\n')

#Q5.33.h
w3 <- c(0,0,0,0,12,-4)
se3 <- sqrt(t(w3) %*% vcov %*% w3)
t3 <- (12*b[5] -4*b[6])/se3
p3 <- 2*pt(t3 , df = df.residual(mod1))
cat(t3,'\n')
cat(p3,'\n')

#Q5.33.i
delta <- -(b[4] + 16 * b[6]) / (2 * b[5])-11
w4 <- c(0, 0, 0, -1 / (2 * b[5]), (b[4] + 16 * b[6]) / (2 * b[5]^2), -8 / b[5])
se4 <- sqrt(t(w4) %*% vcov %*% w4)
ci_lower <- delta - 1.96 * se4
ci_upper <- delta + 1.96 * se4

# 輸出結果
cat('Still ',delta, 'years so that me become negative', "\n")
cat("95% CI: [", ci_lower, ", ", ci_upper, "]\n")









