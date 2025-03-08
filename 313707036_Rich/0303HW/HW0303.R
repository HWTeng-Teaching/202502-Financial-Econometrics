library(POE5Rdata)

data <- POE5Rdata::collegetown

#Q2.17.a
plot(data$sqft,data$price,
     xlab = 'Size(sqft)',
     ylab = 'Price(USD)')

#Q2.17.b
plot(data$sqft,data$price,
     xlab = 'Size(sqft)',
     ylab = 'Price(USD)')
mod1 <- lm(data$price ~ data$sqft)
abline(mod1, col = 'red', lwd = 2)

beta1 <- coef(mod1)[1]
beta2 <- coef(mod1)[2]


#Q2.17.c
mod2 <- lm(data$price ~ I(data$sqft^2))

alpha1 <- coef(mod2)[1]
alpha2 <- coef(mod2)[2]

#Q2.17.d
plot(data$sqft,data$price,
     xlab = 'Size(sqft)',
     ylab = 'Price(USD)')
curve(coef(mod2)[1] + coef(mod2)[2]*x^2,
      add = TRUE, col = "blue", lwd = 2)
abline(a = 0, 
       b = 2*coef(mod2)[2]*20, col = "green", lwd = 2, lty = 2)

#Q2.17.f
resid1 = resid(mod1)
plot(data$sqft, resid1)

resid2 = resid(mod2)
plot(data$sqft, resid2)

#Q2.17.g
sse1 = sum(resid1^2)
sse1

sse2 = sum(resid2^2)
sse2

#Q2.25.a
data2 <- POE5Rdata::cex5_small

hist(data2$foodaway,
     xlab = 'Foodway',
     main = paste("Histogram of Foodway"))
summary(data2$foodaway)

#Q2.25.b grouping(以二元變數作為分組依準)
library(dplyr)
data2_adv <- data2 %>% filter(advanced == 1)
data2_col <- data2 %>% filter(college == 1)
data2_non <- data2 %>% filter(advanced == 0 , college==0)

summary(data2_adv$foodaway)
summary(data2_col$foodaway)
summary(data2_non$foodaway)

#Q2.25.c
data2$lnfoodway <- log(data2$foodaway,exp(1))
data2 <- data2[!is.infinite(data2$lnfoodway), ]
hist(data2$lnfoodway,
     xlab = 'ln(FOODWAY)',
     main = paste("Histogram of ln(Foodway)"))

#Q2.25.d
mod3 <- lm(data2$lnfoodway ~ data2$income)
summary(mod3)

#Q2.25.e
plot(data2$income , data2$lnfoodway,
     xlab = 'Income',
     ylab = 'ln(Foodway)')
abline(coef(mod3)[1],coef(mod3)[2],col = 'red',lwd = 2)

resid3 <- resid(mod3)
plot(data2$income, resid3,
     xlab = 'Income',
     ylab = 'Residuals')

#Q2.28.a
data3 <- POE5Rdata::cps5_small
summary(data3$wage)
summary(data3$educ)
hist(data3$wage,main = 'Histogram of wage',xlab = 'Wage')
hist(data3$educ,main = 'Histogram of education', xlab = 'Education')

#Q2.28.b

mod4 <- lm(data3$wage ~ data3$educ)
summary(mod4)

plot(data3$educ , data3$wage,
     xlab = 'educ',
     ylab = 'wage')
abline(coef(mod4)[1],coef(mod4)[2],col = 'red',lwd = 2)

resid4 <- resid(mod4)
plot(data3$educ , resid4,
     xlab = 'Education',
     ylab = 'Residual')

#Q2.28.d
data3_male <- data3 %>% filter(female == 0)
data3_female <- data3 %>% filter(female == 1)
data3_white <- data3 %>% filter(black == 0)
data3_black <- data3 %>% filter(black == 1)

# 將數據集放入 list
data_list <- list(
  male = data3_male,
  female = data3_female,
  white = data3_white,
  black = data3_black
)

# 用 lapply() 進行回歸
models <- lapply(data_list, function(df) lm(wage ~ educ, data = df))

# 查看回歸結果
lapply(models, summary)

par(mfrow=c(2,2))
plot(data3_male$educ,data3_male$wage, xlab = 'male educ',ylab = 'male wage',ylim = c(0,max(data3_male$wage)))
plot(data3_female$educ,data3_female$wage, xlab = 'female educ',ylab = 'female wage',ylim = c(0,max(data3_male$wage)))
plot(data3_white$educ,data3_white$wage, xlab = 'white educ',ylab = 'white wage',ylim = c(0,max(data3_male$wage)))
plot(data3_black$educ,data3_black$wage, xlab = 'black educ',ylab = 'black wage',ylim = c(0,max(data3_male$wage)))
par(mfrow=c(1,1))

#2.28.e
mod5 <- lm(wage ~ I(educ^2),data = data3)
summary(mod5)

#2.28.f
plot(data3$educ , data3$wage,
     xlab = 'educ',
     ylab = 'wage')
abline(coef(mod4)[1],coef(mod4)[2],col = 'red',lwd = 2)
curve(coef(mod5)[1] + coef(mod5)[2]*x^2,
      add = TRUE, col = "blue", lwd = 2)

summary(mod4)$r.squared
summary(mod5)$r.squared

