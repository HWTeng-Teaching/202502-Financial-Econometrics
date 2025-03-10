rm(list=ls())
library(POE5Rdata)

#C02Q17
#(a)
data(collegetown)
c <- collegetown
plot(c$sqft,c$price,
     xlab="house size", 
     ylab="house price", 
     type = "p")

#(b)
mod1 <- lm(price ~ sqft, data = c)
abline(coef(mod1)[[1]],coef(mod1)[[2]])

#(c)
mod2 <- lm(price ~ I(sqft^2), data = c)
b = coef(mod2)
print(b)
x = 20
y = b[[1]]+b[[2]]*x^2
dydx =  2*b[[2]]*x
"marginal effect: 7.38 thousands of dollar"

#(d)
plot(c$sqft,c$price,
     xlab="house size", 
     ylab="house price", 
     type = "p")
curve(b[[1]]+b[[2]]*x^2, col="red", add=TRUE)
abline(y-dydx*x,dydx,col="blue")

#(e)
ela = (2*b[[2]]*x^2)/y
print(ela)
#(f)
plot(c$sqft, c$price-predict(mod1), 
     xlab="X", 
     ylab="Residuals"
    )

plot(c$sqft, c$price-predict(mod2), 
     xlab="X", 
     ylab="Residuals"
    )
"SR3 appears violated. Var(e|x) seems not a constant,
it increases with x increasing."

#(g)
print(sum((c$price-predict(mod1))^2))
print(sum((c$price-predict(mod2))^2))
"Model (c) has a lower SSE, it should be a better model"


#C02Q25
#(a)
data(cex5_small)
cex <- cex5_small
hist(cex$foodaway,
     xlab="Foodway" ,breaks = 50
     )
sum <- summary(cex$foodaway)
print(sum)

#(b)
fa_adv <- numeric()
fa_col <- numeric()
fa_nac <- numeric()

for (i in 1:nrow(cex)){
  if (cex$advanced[i] == 1){
    fa_adv <- c(fa_adv,cex$foodaway[i])
  }
  else if (cex$college[i] == 1){
    fa_col <- c(fa_col,cex$foodaway[i])
  }
  else{
    fa_nac <- c(fa_nac,cex$foodaway[i])
  }
}
print(summary(fa_adv))
print(summary(fa_col))
print(summary(fa_nac))

#(c)
fa <- numeric()
ic <- numeric()
for (i in 1:nrow(cex)){
  if (cex$foodaway[i] > 0){
    fa <- c(fa,cex$foodaway[i])
    ic <- c(ic,cex$income[i])
  }
}
ln_fa = log(fa)
hist(ln_fa,
     xlab="ln(Foodaway)" 
)
print(summary(ln_fa))
"Some of FOODAWAY are 0 in observation. They can't be put into ln() function"

#(d)
mod3 <- lm(ln_fa ~ ic)
b3 = coef(mod3)
print(b3)
"When the income is x, the expected FOODAWAY increases about
0.0069*exp(3.1293+0.0069*x) unit with an additional x."

#(e)
plot(ic,ln_fa,
     xlab="income", 
     ylab="ln(foodaway)", 
     type = "p")
abline(b3[[1]],b3[[2]],col="red")


#(f)
plot(ic, ln_fa-predict(mod3), 
     xlab="X", 
     ylab="Residuals"
)
"They seem completely random."

#C02Q28
#(a)
data(cps5_small)
cps <- cps5_small
hist(cps$wage,
     xlab="WAGE",breaks = 50
)
hist(cps$educ,
     xlab="EDUC" 
)
"WAGE is a right-skewed data, while EDUC is a left-skewed data"

#(b)
mod4 <- lm(cps$wage ~ cps$educ)
print(summary(mod4))
b4 = coef(mod4)

#(c)
plot(cps$educ, cps$wage-predict(mod4), 
     xlab="X", 
     ylab="Residuals"
)
"These patterns should be hold in the residuals:
random scatter around zero, constant variance, no autocorrelation, and normality.
In this plot, the residuals seems not random scatter around zero"

#(d)
males_wage <- numeric()
females_wage <- numeric()
blacks_wage <- numeric()
whites_wage <- numeric()
males_educ <- numeric()
females_educ <- numeric()
blacks_educ <- numeric()
whites_educ <- numeric()

for (i in 1:nrow(cps)){
  if (cps$female[i] == 0){
    males_wage <- c(males_wage,cps$wage[i])
    males_educ <- c(males_educ,cps$educ[i])
  }
  else{
    females_wage <- c(females_wage,cps$wage[i])
    females_educ <- c(females_educ,cps$educ[i])
  }
  if (cps$black[i] == 1){
    blacks_wage <- c(blacks_wage,cps$wage[i])
    blacks_educ <- c(blacks_educ,cps$educ[i])
  }
  else{
    whites_wage <- c(whites_wage,cps$wage[i])
    whites_educ <- c(whites_educ,cps$educ[i])
  }
}
mod_males <- lm(males_wage ~ males_educ)
print(summary(mod_males))
mod_females <- lm(females_wage ~ females_educ)
print(summary(mod_females))
mod_blacks <- lm(blacks_wage ~ blacks_educ)
print(summary(mod_blacks))
mod_whites <- lm(whites_wage ~ whites_educ)
print(summary(mod_whites))

#(e)
mod5 <- lm(wage ~ I(educ^2), data = cps)
print(summary(mod5))
b5 = coef(mod5)
cat(2*b5[[2]]*12,2*b5[[2]]*16)
"The marginal effect in a linear model is the same 
for both 12y EDUC and 16Y EDUC, but in a quartic model, 
the effect on 12Y EDUC is smaller than in the linear model, 
while the effect on 16Y EDUC is larger than in the linear model."

#(f)
plot(cps$educ,cps$wage,
     xlab="EDUC", 
     ylab="WAGE", 
     type = "p")
curve(b5[[1]]+b5[[2]]*x^2, col="red", add=TRUE)
abline(b4[[1]],b4[[2]],col="blue")
"Quadratic model appears to fit the data better."