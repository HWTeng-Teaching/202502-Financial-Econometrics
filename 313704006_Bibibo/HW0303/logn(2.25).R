library(POE5Rdata)
data("cex5_small")
x = cex5_small$income
y = cex5_small$foodaway
advance = cex5_small$advanced
college = cex5_small$college
#觀察foodaway
#?hist
hist(y, main = paste("Histogram of foodaway"), xlab = "foodaway")
summary(y)
#Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#0.00   12.04   32.55   49.27   67.50 1179.00
length(y)

#mean v.s. median (advanced, college)
ad = cex5_small$foodaway[advance==1]
mean(ad) #73.15494
median(ad) #48.15
clg = cex5_small$foodaway[college==1]
mean(clg) #48.59718
median(clg) #36.11
both_no = cex5_small$foodaway[(college==0 & advance==0)]
mean(both_no) #39.01017
median(both_no) #26.02

#ln_y
#ln_y = log(y)
#ln_y
ln_y = y[y>0]
ln_y = log(ln_y)
hist(ln_y, main = "Histogram of ln(foodaway)", xlab = "ln(foodaway)")
summary(ln_y)
length(ln_y)
#修正ln_y與x大小不同
x = x[y>0]

#lgn model (ln(FOODAWAY) = β1 + β2INCOME + e)
lgn_model = lm(ln_y~x, data = cex5_small)
lgn_model
#(Intercept)            x  
#3.129300     0.006902 

β1 = coef(lgn_model)[1]
β2 = coef(lgn_model)[2]
plot(x, ln_y, pch=20, xlab = "income", ylab = "ln(foodaway)")
abline(lgn_model, col="red")

#residual
residual = resid(lgn_model)
plot(x, residual, main = "rs vs. INCOME", xlab = "income", pch=20)
