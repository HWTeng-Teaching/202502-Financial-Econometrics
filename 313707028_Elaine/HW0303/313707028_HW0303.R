#2.17
#a
data('collegetown')
plot(collegetown$sqft,collegetown$price,pch=16,cex=0.7,xlab = 'house size',ylab = 'house price')
#b
tab <- lm(price ~ sqft, data = collegetown)
summary(tab)
abline(tab, col = "red")
#c
collegetown$sqft_square <- collegetown$sqft^2
tab2 <- lm(price ~ sqft_square, data = collegetown)
summary(tab2)
quad_fun(21)
quad_fun(20)
#d
quad_fun <- function(x){
    y=coef(tab2)[1]+coef(tab2)[2]*x^2
    return(y)
}
plot(collegetown$sqft,collegetown$price,pch=16,cex=0.7,xlab = 'house size',ylab = 'house price')
curve(quad_fun, col = "blue",add = TRUE,lwd=2)
m <- 40*coef(tab2)[2]
tangline<- function(x){
  y=m*x+19.7583
}
curve(tangline, col = "orange",add = TRUE,lwd=1.5)


#f
plot(collegetown$sqft,summary(tab)$resid,pch=16,cex=0.5,xlab = 'house size', ylab = 'residual ',main='linear model')
plot(collegetown$sqft,summary(tab2)$resid,pch=16,cex=0.5,xlab = 'house size', ylab = 'residual',main='quadratic model')

#g
(resid1_square <- deviance(tab))
(resid2_square <- deviance(tab2))

#2.25
#a
data('cex5_small')
str(cex5_small)
length(cex5_small$foodaway)
summary(cex5_small$foodaway)
hist(cex5_small$foodaway,breaks=30,main = 'Histogram of FOODAWAY',xlab = 'FOODAWAY')

#b
cex5_small$advanced <- as.logical(cex5_small$advanced)
advanced_foodaway <- cex5_small$foodaway[cex5_small$advanced]
summary(advanced_foodaway)
cex5_small$college <- as.logical(cex5_small$college)
college_foodaway <- cex5_small$foodaway[cex5_small$college]
summary(college_foodaway)
ncorna_foodaway <- cex5_small$foodaway[cex5_small$college==F&cex5_small$advanced==F]
summary(ncorna_foodaway)

#c
cex5_small <- cex5_small[cex5_small$foodaway>0,]
cex5_small$lnfoodaway <- log(cex5_small$foodaway)
length(cex5_small$lnfoodaway)
summary(cex5_small$lnfoodaway)
hist(cex5_small$lnfoodaway,main = 'Histogram of LNFOODAWAY',xlab = 'LNFOODAWAY',breaks=30)

#d
tab3 <- lm(lnfoodaway~income, data = cex5_small)
summary(tab3)

#e
plot(cex5_small$income,cex5_small$lnfoodaway,pch=16,cex=0.7,xlab = 'INCOME',ylab = 'LNFOODAWAY')
abline(tab3, col = "red",lwd=2)

#f
(resid_square <- sum(summary(tab3)$resid))
plot(cex5_small$income,summary(tab3)$resid,pch=16,cex=0.5)

#2.28
#a
data("cps5_small")
summary(cps5_small$educ)
summary(cps5_small$wage)
hist(cps5_small$educ,breaks = 30,xlab = "EDUC",main = 'Histogram of EDUC')
hist(cps5_small$wage,breaks = 30,xlab = "WAGE",main = 'Histogram of WAGE')

#b
tab4 <- lm(wage~educ, data = cps5_small)
summary(tab4)

#c
(resid4_square <- sum(summary(tab4)$resid))
plot(cps5_small$educ,(summary(tab4)$resid),pch=16,cex=0.5)

#d
str(cps5_small)
tab4a <- lm(wage~educ, data = cps5_small,subset=(female==0))
summary(tab4a)
tab4b <- lm(wage~educ, data = cps5_small,subset=(female==1))
summary(tab4b)
tab4c <- lm(wage~educ, data = cps5_small,subset=(black==1))
summary(tab4c)
tab4d <- lm(wage~educ, data = cps5_small,subset=(black==0))
summary(tab4d)
install.packages('ggplot2')
library(ggplot2)
ggplot(cps5_small, aes(x = educ, y = wage, colour = factor(female))) +
  geom_point(size = 1.5, alpha=0.5) +
  geom_smooth(method = "lm", se = FALSE) +
  labs(title = "Scatter Plot with Regression Line")
ggplot(cps5_small, aes(x = educ, y = wage, colour = factor(black))) +
  geom_point(size = 1.5, alpha=0.5) +
  geom_smooth(method = "lm", se = FALSE) +
  labs(title = "Scatter Plot with Regression Line")

#e
cps5_small$educ_square <- (cps5_small$educ)^2
tab5 <- lm(wage~educ_square, data = cps5_small)
summary(tab5)

#f
plot(cps5_small$educ,cps5_small$wage,pch=16,cex=0.7,xlab = 'EDUC',ylab = 'WAGE',ylim = c(-50,250))
abline(tab4, col = "red",lwd=2)
quad_fun2 <- function(x){
  y=coef(tab5)[1]+coef(tab5)[2]*x^2
  return(y)
}
curve(quad_fun2, col = "blue",add = TRUE,lwd=2)
legend("topleft", legend = c("Linear Fit", "Quadratic Fit"), 
       col = c("red", "blue"), lwd = 2, bty = "n")
