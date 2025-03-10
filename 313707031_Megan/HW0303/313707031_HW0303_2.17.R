#2.17 (a)
if (!requireNamespace("remotes", quietly = TRUE)) install.packages("remotes")
remotes::install_github("ccolonescu/POE5Rdata", force = TRUE)

library(POE5Rdata)
data(collegetown)

plot(collegetown$sqft, collegetown$price,
     main = 'Scatter Diagram of House Price against House Size',
     xlab = "House Size(in hundreds of square feet)", 
     ylab = "House Price(in thousands of dollars)",
     pch=18,col="purple")


#2.17 (b)
y = collegetown$price
x = collegetown$sqft
lr_model <- lm(y~x)
summary(lr_model)

plot(collegetown$sqft, collegetown$price,
     main = 'Scatter Diagram of House Price against House Size',
     xlab = "House Size(in hundreds of square feet)", 
     ylab = "House Price(in thousands of dollars)",
     pch=18,col="purple")
abline(lm_model,col='pink',lwd=2.5)

#2.17(c)
y = collegetown$price
x2 = collegetown$sqft^2
qr_model <- lm(y~x2)
summary(qr_model)

sqft_2000 <- 20
marginal_effect <-2*coef(qr_model)[2]*sqft_2000
marginal_effect

#2.17(d)
plot(collegetown$sqft, collegetown$price,
     main = 'Scatter Diagram of House Price against House Size',
     xlab = "House Size(in hundreds of square feet)", 
     ylab = "House Price(in thousands of dollars)",
     pch=18,col="purple")
curve(coef(qr_model)[1] + coef(qr_model)[2]*x^2,
      add = TRUE, col = "orange", lwd = 2)
abline(a=0,b=marginal_effect,col='pink',lwd=2.5,lty=2)

#2.17(f)
lrm_resid <- resid(lr_model)
plot(collegetown$sqft, lrm_resid,
     main = 'lrm_resid against House Size',
     xlab = 'House Size(in hundreds of square feet)',
     ylab = 'Linear Regression Model Residuals',
     pch=18,col="purple")
abline(a=0,b=0,col='pink',lwd=2.5,lty=2)

qrm_resid <- resid(qr_model)
plot(collegetown$sqft, qrm_resid,
     main = 'qrm_resid against House Size',
     xlab = 'House Size(in hundreds of square feet)',
     ylab = 'Quadratic Regression Model Residuals',
     pch=18,col="purple")
abline(a=0,b=0,col='pink',lwd=2.5,lty=2)

#2.17(g)
lrm_sse = sum(lrm_resid^2)
lrm_sse

qrm_sse = sum(qrm_resid^2)
qrm_sse
