rm(list=ls()) # Caution: this clears the Environment

library(POE5Rdata)
data(capm5)

x = capm5$mkt-capm5$riskfree
y_ge   = capm5$ge  -capm5$riskfree
y_ibm  = capm5$ibm -capm5$riskfree
y_ford = capm5$ford-capm5$riskfree
y_msft = capm5$msft-capm5$riskfree
y_dis  = capm5$dis -capm5$riskfree
y_xom  = capm5$xom -capm5$riskfree


mod_ge   <- lm(y_ge   ~ x)
mod_ibm  <- lm(y_ibm  ~ x)
mod_ford <- lm(y_ford ~ x)
mod_msft <- lm(y_msft ~ x)
mod_dis  <- lm(y_dis  ~ x)
mod_xom  <- lm(y_xom  ~ x)

firm  <- c("GE","IBM","Ford","Microsoft","Disney","Exxon-Mobil")
alpha <- c(coef(mod_ge  )[[1]],coef(mod_ibm)[[1]],coef(mod_ford)[[1]],
           coef(mod_msft)[[1]],coef(mod_dis)[[1]],coef(mod_xom )[[1]])
beta  <- c(coef(mod_ge  )[[2]],coef(mod_ibm)[[2]],coef(mod_ford)[[2]],
           coef(mod_msft)[[2]],coef(mod_dis)[[2]],coef(mod_xom )[[2]])
pvalue <- c(summary(mod_ge  )$coefficients[1,4],
            summary(mod_ibm )$coefficients[1,4],
            summary(mod_ford)$coefficients[1,4],
            summary(mod_msft)$coefficients[1,4],
            summary(mod_dis )$coefficients[1,4],
            summary(mod_xom )$coefficients[1,4])
print(data.frame(firm,alpha,pvalue,beta))

plot(x,y_msft,
     ylim=c(min(x,y_msft), max(x,y_msft)),
     xlim=c(min(x,y_msft), max(x,y_msft)),
     xlab="market excessive return", 
     ylab="microsoft excessive return", 
     type = "p")
abline(coef(mod_msft)[[1]],coef(mod_msft)[[2]])

mod_na_ge   <- lm(y_ge   ~ x-1)
mod_na_ibm  <- lm(y_ibm  ~ x-1)
mod_na_ford <- lm(y_ford ~ x-1)
mod_na_msft <- lm(y_msft ~ x-1)
mod_na_dis  <- lm(y_dis  ~ x-1)
mod_na_xom  <- lm(y_xom  ~ x-1)

beta_noalpha  <- c(coef(mod_na_ge  )[[1]],coef(mod_na_ibm)[[1]],coef(mod_na_ford)[[1]],
                   coef(mod_na_msft)[[1]],coef(mod_na_dis)[[1]],coef(mod_na_xom )[[1]])
change <- beta-beta_noalpha
print(data.frame(firm,beta,beta_noalpha,change))
