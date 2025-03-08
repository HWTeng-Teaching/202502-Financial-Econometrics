library(POE5Rdata)
data("collegetown")
x = collegetown$sqft
y = collegetown$price

#繪製散佈圖觀察
plot(x, y, xlab = "sqft", ylab = "price")

#linear (PRICE = β1 + β2SQFT + e)
ln_mod = lm(y ~ x, data = collegetown)
coef(ln_mod) #intercept:-115.42 x:13.40
#?abline
β1 = coef(ln_mod)[1]
β2 = coef(ln_mod)[2]
abline(β1, β2, col="red")

#quadratic (PRICE = α1 + α2 SQFT^2 + e)
qd_mod = lm(y ~ I(x^2), data = collegetown)
coef(qd_mod) #intercept:93.57 I(x^2):0.18
α1 = coef(qd_mod)[1]
α2 = coef(qd_mod)[2]
curve(α1+α2*(x^2), from = min(x), to = max(x), col="green", add = TRUE)

#marginal effect of sqrt (2*α2*SQFT)
sqft = 20
mg_ef_20sqft = 2*α2*sqft
mg_intercept = α1 + α2*(sqft^2) - mg_ef_20sqft*sqft
abline(mg_intercept, mg_ef_20sqft, col="blue")
legend("topleft", c("linear","quadratic","tangent"), col = c("red","green","blue"),
       lwd = 2, lty = 1)
#elasticity of PRICE with respect to SQFT (2*α2*SQFT*SQFT/price)
price = α1 + α2*(sqft^2)
et = mg_ef_20sqft*sqft/price

#residual compare
ln_r = resid(ln_mod)
plot(x, ln_r, main="ln_mod_r",
     xlab="SQFT", ylab="Residuals", col="red", pch=20)

qd_r = resid(qd_mod)
plot(x, qd_r, main="qd_mod_r",
     xlab="SQFT", ylab="Residuals", col="green",pch=20)
#test = c(1, 2, 3)
#sum(test^2) 
ln_r_sse = sum(resid(ln_mod)^2)
qd_r_sse = sum(resid(qd_mod)^2)
