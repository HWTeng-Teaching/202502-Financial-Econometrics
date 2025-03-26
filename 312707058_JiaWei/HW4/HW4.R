#-----------------C04Q04-----------------
rm(list=ls())
reg <- data.frame(
  b1 = c(64.289,39.464),
  b2 = c(0.990,15.312),
  se_b1 = c(2.422,4.198),
  se_b2 = c(0.183,1.727),
  N = c(50, 46),
  Rsquare = c(0.3793,0.6414)
)

#(a)
x1 <- (0:30)
y1 <- reg[1,"b1"]+reg[1,"b2"]*x1

plot(x1,y1,
     xlab="EXPER", 
     ylab="RATING", 
     type = "p")
#(b)
x2 <- (1:30)
y2 <- reg[2,"b1"]+reg[2,"b2"]*log(x2)

plot(x2,y2,
     xlab="EXPER", 
     ylab="RATING", 
     type = "p")
"Since the EXPER for the artists with no experience should be 0, 
 and log(0) -> -infinity, they can't be used in the model"

#(c)(d)
x1 = 10
x2 = 20
cat("Model1 10 years: ", reg[1,"b2"]," 20 years:  ", reg[1,"b2"])
cat("Model2 10 years: ", reg[1,"b2"]/x1," 20 years: ", reg[2,"b2"]/x2)
#(e)
"Model2 is better, because the R-squared of Model2 is larger."
#(f)
"Model2 is more reasonable, because the effect of experience on performance rating is positive, 
 but it should decrease as experience increases"

#-----------------C04Q28-----------------
rm(list=ls())
library(POE5Rdata)
data(wa_wheat)


li2li = lm(northampton~time,data = wa_wheat )
li2ln = lm(northampton~I(log(time)),data = wa_wheat )
quadr = lm(northampton~I(time^2),data = wa_wheat )
ln2li = lm(I(log(northampton))~time,data = wa_wheat )

#(a)
plot(wa_wheat$time,wa_wheat$northampton,
     xlab="TIME", 
     ylab="YIELD", 
     type = "p")
curve(li2li$coefficients[1]+li2li$coefficients[2]*x, col="red", add=TRUE)
curve(li2ln$coefficients[1]+li2ln$coefficients[2]*log(x), col="blue", add=TRUE)
curve(quadr$coefficients[1]+quadr$coefficients[2]*x^2, col="green", add=TRUE)
curve(exp(ln2li$coefficients[1]+ln2li$coefficients[2]*x), col="purple", add=TRUE)


plot(wa_wheat$time, wa_wheat$northampton-predict(li2li), 
     xlab="TIME", 
     ylab="Residuals")
plot(wa_wheat$time, wa_wheat$northampton-predict(li2ln), 
     xlab="TIME", 
     ylab="Residuals")
plot(wa_wheat$time, wa_wheat$northampton-predict(quadr), 
     xlab="TIME", 
     ylab="Residuals")
plot(wa_wheat$time, wa_wheat$northampton-predict(ln2li), 
     xlab="TIME", 
     ylab="Residuals")

qqnorm(li2li$residuals)
qqline(li2li$residuals, col = "red")
qqnorm(li2ln$residuals)
qqline(li2ln$residuals, col = "red")
qqnorm(quadr$residuals)
qqline(quadr$residuals, col = "red")
qqnorm(ln2li$residuals)
qqline(ln2li$residuals, col = "red")
summary(li2li)
summary(li2ln)
summary(quadr)
summary(ln2li)

"The quadratic model is preferable because it has the largest R-squared."

#(b)
"For every 1-unit increase in TIME^2, YIELD increase by γ1 units"

#(c)
studentized_residuals <- rstudent(quadr)
leverage <- hatvalues(quadr)
dfbetas_values <- dfbetas(quadr)
dffits_values <- dffits(quadr)

n <- nrow(wa_wheat)
k <- length(coef(quadr)) - 1
leverage_threshold <- 2 * (k + 1) / n
dfbeta_threshold <- 2 / sqrt(n)
dffits_threshold <- 2 * sqrt(k / n)

outliers <- list(
  studentized_residuals = which(abs(studentized_residuals) > 2),
  leverage = which(leverage > leverage_threshold),
  dfbetas = which(apply(dfbetas_values, 1, function(x) any(abs(x) > dfbeta_threshold))),
  dffits = which(abs(dffits_values) > dffits_threshold)
)
print(outliers)

#(d)
x = wa_wheat$time[1:47]
y = wa_wheat$northampton[1:47]
x
mod = lm(y~x)
x48 <- list(x = wa_wheat$time[48])
y48_interval <- predict(mod, x48, interval = "prediction", level = 0.95)
cat(wa_wheat$northampton[48],"[",y48_interval[2],", ",y48_interval[3],"]")
"Yes, it does"

#-----------------C04Q29-----------------
rm(list=ls())
library(POE5Rdata)
data(cex5_small)
summary(cex5_small$food)
summary(cex5_small$income)
n = nrow(cex5_small)
#(a)
#install.packages("tseries")
library(tseries)
summary(cex5_small$food)
summary(cex5_small$income)

hist(cex5_small$food, main = "Histogram of Food", xlab = "FOOD", breaks = 20)
abline(v = mean(cex5_small$food), col = "red", lwd = 2)  
abline(v = median(cex5_small$food), col = "blue", lwd = 2)

hist(cex5_small$income, main = "Histogram of Income", xlab = "INCOME", breaks = 20)
abline(v = mean(cex5_small$income), col = "red", lwd = 2)  
abline(v = median(cex5_small$income), col = "blue", lwd = 2)
print(jarque.bera.test(cex5_small$food))
print(jarque.bera.test(cex5_small$income))
"The sample mean is larger than the median in both datasets, 
 and the histograms are neither symmetrical nor “bell-shaped” curves"

#(b)
mod <- lm(food~income,data = cex5_small)
summary(mod)
plot(cex5_small$income,cex5_small$food,
     xlab="INCOME", 
     ylab="FOOD", 
     type = "p")
abline(coef(mod)[[1]],coef(mod)[[2]],col = "red")	

b2 <- coef(mod)[2]
se_b2 <- summary(mod)$coefficients[2, 2]
df <- n - 2
alpha = 0.05
t <- qt(1-alpha/2, df)
CI_b2 = c(b2-(t*se_b2),b2+(t*se_b2))
cat("95% CI: [", CI_b2[1], ", ", CI_b2[2], "]")
"No, we havn't, because the R-square of the model is too small"

#(c)
plot(cex5_small$income, cex5_small$food-predict(mod), 
     xlab="INCOME", 
     ylab="Residuals")
hist(mod$residuals, main = "Histogram of Residuals", xlab = "redisuals", breaks = 20)
print(jarque.bera.test(mod$residuals))

"The distribution of residuals is right-skewed"
"It is more important that the random error e follows a normal distribution
 because this assumption is crucial for making inferences such as confidence intervals and hypothesis testing"

#(d)

ela_li2li <- function(x,b1,b2) {
  y = b1+b2*x
  return(b2*(x/y))
}

ela_19 <- ela_li2li(19,coef(mod)[[1]],coef(mod)[[2]])
ela_65 <- ela_li2li(65,coef(mod)[[1]],coef(mod)[[2]])
ela_160 <- ela_li2li(160,coef(mod)[[1]],coef(mod)[[2]])

CI_19 <- c(ela_li2li(19,coef(mod)[[1]],CI_b2[1]),ela_li2li(19,coef(mod)[[1]],CI_b2[2]))
CI_65 <- c(ela_li2li(65,coef(mod)[[1]],CI_b2[1]),ela_li2li(65,coef(mod)[[1]],CI_b2[2]))
CI_160 <- c(ela_li2li(160,coef(mod)[[1]],CI_b2[1]),ela_li2li(160,coef(mod)[[1]],CI_b2[2]))

cat("Elasticity at INCOME = 19: ", ela_19,", 95% CI: [", 
    CI_19[1], ",", CI_19[2], "]\n")
cat("Elasticity at INCOME = 65: ", ela_65,", 95% CI: [", 
    CI_65[1], ",", CI_65[2], "]\n")
cat("Elasticity at INCOME = 160:", ela_160,", 95% CI: [", 
    CI_160[1], ",", CI_160[2], "]\n")
"The estimated elasticities are dissimilar, and the interval estimates are not overlap."
"Based on Economics principles, the income elasticity for food
 should decrease as the INCOME increases."

#(e)
ln2ln <- lm(I(log(food))~I(log(income)),data = cex5_small)

plot(log(cex5_small$income),log(cex5_small$food),
     xlab="ln(INCOME)", 
     ylab="ln(FOOD)", 
     type = "p")
abline(coef(ln2ln)[[1]],coef(ln2ln)[[2]],col = "red")	
summary(ln2ln)
"The linear model  seems to fit the data better, since it has a larger R-squared"

#(f)
ela  <- coef(ln2ln)[[2]]

b2 <- coef(ln2ln)[2]
se_b2 <- summary(ln2ln)$coefficients[2, 2]
df <- n - 2
alpha = 0.05
t <- qt(1-alpha/2, df)
CI_b2 = c(b2-(t*se_b2),b2+(t*se_b2))

CI_ela  <- c(CI_b2[1],CI_b2[2])

cat("Elasticity (log-log model): ", ela,", 95% CI: [", 
    CI_ela[1], ",", CI_ela[2], "]\n")
"The estimated elasticities is dissimilar from linear model"

#(g)
plot(log(cex5_small$income), log(cex5_small$food)-predict(ln2ln), 
     xlab="ln(INCOME)", 
     ylab="Residuals")
hist(mod$residuals, main = "Histogram of Residuals", xlab = "redisuals", breaks = 20)
print(jarque.bera.test(ln2ln$residuals))
"The p-value < 0.05, we conclude that the regression errors is not normal distribution"

#(h)
li2ln <- lm(food~I(log(income)),data = cex5_small)

plot(log(cex5_small$income),cex5_small$food,
     xlab="ln(INCOME)", 
     ylab="FOOD", 
     type = "p")
abline(coef(li2ln)[[1]],coef(li2ln)[[2]],col = "red")	
summary(li2ln)
"No, it is not"
"Linear model is still better than others"

#(i)
ela_li2ln <- function(x,b1,b2) {
  y = b1+b2*x
  return(b2/y)
}

ela_19 <- ela_li2li(19,coef(li2ln)[[1]],coef(li2ln)[[2]])
ela_65 <- ela_li2li(65,coef(li2ln)[[1]],coef(li2ln)[[2]])
ela_160 <- ela_li2li(160,coef(li2ln)[[1]],coef(li2ln)[[2]])

b2 <- coef(li2ln)[2]
se_b2 <- summary(li2ln)$coefficients[2, 2]
df <- n - 2
alpha = 0.05
t <- qt(1-alpha/2, df)
CI_b2 = c(b2-(t*se_b2),b2+(t*se_b2))

CI_19 <- c(ela_li2li(19,coef(li2ln)[[1]],CI_b2[1]),ela_li2li(19,coef(li2ln)[[1]],CI_b2[2]))
CI_65 <- c(ela_li2li(65,coef(li2ln)[[1]],CI_b2[1]),ela_li2li(65,coef(li2ln)[[1]],CI_b2[2]))
CI_160 <- c(ela_li2li(160,coef(li2ln)[[1]],CI_b2[1]),ela_li2li(160,coef(li2ln)[[1]],CI_b2[2]))

cat("Elasticity at INCOME = 19: ", ela_19,", 95% CI: [", 
    CI_19[1], ",", CI_19[2], "]\n")
cat("Elasticity at INCOME = 65: ", ela_65,", 95% CI: [", 
    CI_65[1], ",", CI_65[2], "]\n")
cat("Elasticity at INCOME = 160:", ela_160,", 95% CI: [", 
    CI_160[1], ",", CI_160[2], "]\n")

"The estimated elasticities are dissimilar"

#(j)
plot(log(cex5_small$income), cex5_small$food-predict(ln2ln), 
     xlab="ln(INCOME)", 
     ylab="Residuals")
hist(mod$residuals, main = "Histogram of Residuals", xlab = "redisuals", breaks = 20)
print(jarque.bera.test(li2ln$residuals))
"The p-value < 0.05, we conclude that the regression errors is not normal distribution"

#(k)
"Though the linear relationship model is not fitted well, it is best among these three models since it has largest R-squared"