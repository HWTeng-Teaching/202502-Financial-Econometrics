if (!requireNamespace("remotes", quietly = TRUE)) install.packages("remotes")
remotes::install_github("ccolonescu/POE5Rdata", force = TRUE)
library(POE5Rdata)
data <- POE5Rdata::collegetown #POE5Rdata::collegetown


#2.17(a)
plot(data$sqft,data$price,  #先放x再放y
     xlim = c(0,max(data$sqft)),
     ylim = c(0,max(data$price)),
     xlab = "square feet of the house in hundreds",
     ylab = "sale price in thousands of dollars",type="p")
    
#2.17(b)

mod1 <- lm(price ~ sqft,data = data)
b1 <- coef(mod1)[[1]]
b2 <- coef(mod1)[[2]]
se_b1 <- summary(mod1)$coefficients[1,2] #先列後行 擷取SUMMARY的矩陣 
se_b2 <- summary(mod1)$coefficients[2,2]
sprintf("PRICE = %.5f + %.5f SQFT^2", b1, b2)
sprintf("(se)    (%.4f)    (%.5f)", se_b1, se_b2)
summary(mod1)
plot(data$sqft,data$price,pch = 16,
     xlim = c(0,max(0,max(data$sqft))),
     ylim = c(0,max(0,max(data$price))),
     xlab = "Sqft,100s",
     ylab = "Price,$1000",
     col = "darkblue")
xbar <- mean(data$sqft)
ybar <- mean(data$price)
abline(h = ybar, col = "darkgreen", lty = 2, lwd = 2)
abline(v = xbar, col = "darkgreen", lty = 2, lwd = 2)
abline(b1,b2,col = "red")

#2.17(c)
data <- POE5Rdata::collegetown #POE5Rdata::collegetown
mod2 <- lm(price ~ I(sqft^2),data = data) #I()在 R 線性回歸 (lm()) 裡的作用是 防止 R 把 ^ 認為是公式中的特殊運算。
mod2
se_b1 <- summary(mod2)$coefficients[1,2]
se_b2 <- summary(mod2)$coefficients[2,2]
sprintf("PRICE = %.5f + %.5f SQFT^2", b1, b2)
sprintf("(se)    (%.4f)    (%.5f)", se_b1, se_b2)
summary(mod2)
b1 <- coef(mod2)[[1]]
b2<- coef(mod2)[[2]]
sqftx <- 20
DpriceDsqft <- 2*b2*sqftx
sqft_change <- 1
price_change = DpriceDsqft*sqft_change
format_price <- sprintf("%.4f",price_change)
cat("Marginal effect:$",format_price,"in thousands","\n")

#2.17(d)
plot(data$sqft,data$price,
     xlim = c(0,max(data$sqft)),
     ylim = c(0,2000),
     xlab = "Sqft,100s",
     ylab = "Price,$1000")
curve(b1+b2*x^2,add = TRUE,col = "red")

sqft2<- data$sqft^2
mod2 <- lm(price ~ sqft2,data = data)
x0 <- 20
y0 <- predict(mod2, newdata = data.frame(x0))


coefs <- coef(mod2) # 計算該點的切線斜率（微分）,獲取回歸係數
slope <-  2 * coefs[[2]]*x0    

abline(a = y0 - slope*x0, b = slope, col = "blue", lwd = 2, lty = 2) # 繪製切線

#2.17(e)
#elasticity
mod2 <- lm(price ~ I(sqft^2),data = data)
b1 <- coef(mod2)[[1]]
b2 <- coef(mod2)[[2]]
sqftx <- 20
DpriceDsqft <- 2*b2*sqftx
elasticity = DpriceDsqft*sqftx/(b1 + b2 * sqftx^2)
round(elasticity, digits = 3)

#2.17(f)

r = resid(mod1)#找殘差
plot(data$sqft,r,col = "red",pch = 16,#實心圓點（常用）
     xlab = "Sqft,100s",
     ylab = "Residuals,linear fit",
     main = "Residuals from linear relation",
     type = "p") #在plot選擇繪圖方式

r1 = resid(mod2)
plot(data$sqft,r1,col = "darkblue",pch = 16,#實心圓點（常用）
     xlab = "Sqft,100s",
     ylab = "Residuals,linear fit",
     main = "Residuals from quadratic relation",
     type = "p") #在plot選擇繪圖方式

#Ans : In both models, the residual patterns do not appear random. 
#The variation in the residuals increases as SQFT increases, suggesting that the homoscedasticity assumption may be violated. 

#2.17(g)

SSE <- sum(resid(mod1)^2) # resid(mod1) 取得殘差，平方後加總
SSE2 <- sum(resid(mod2)^2)
SSE_round <- format(SSE, nsmall = 2)  #這樣會確保小數點後有兩位 format nsamll
SSE2_round <- format(SSE2,nsmall = 2)
SSE_round
SSE2_round
#Ans : The sum of square residuals linear relationship is 5,262,846.95.
#The sum of square residuals for the quadratic relationship is 4,222,356.35. 
#In this case the quadratic model has the lower SSE.
#The lower SSE means that the data values are closer to the fitted line for the quadratic model than for the linear model. 

#----------------------------------------------------------------------------------------
  
#2.25(a)
food <- POE5Rdata::cex5_small
head(food)

hist(food$foodaway,col="orange")
summary(food$foodaway)
mean(food$foodaway)
median(food$foodaway)
quantile(food$foodaway)

#2.25(b)

food <- POE5Rdata::cex5_small
mean1 <- mean(subset(food$foodaway,food$advanced == 1,na.rm = True))#若有na忽略
med1 <- median(subset(food$foodaway,food$advanced == 1,na.rm = True))
mean2 <- mean(subset(food$foodaway,food$college == 1,na.rm = True))#若有na忽略
med2 <- median(subset(food$foodaway,food$college == 1,na.rm = True))
mean3 <- mean(subset(food$foodaway,food$college == 0&food$advanced == 0,na.rm = TRUE))#若有na忽略
med3<- median(subset(food$foodaway,food$college == 0&food$advanced == 0,na.rm = TRUE))

summary_table <- data.frame(
  Group = c("Advanced","college","None"),
  N= c(sum(food$advanced == 1, na.rm = TRUE),
      sum(food$college == 1, na.rm = TRUE),
      sum(food$college == 0 & food$advanced == 0, na.rm = TRUE)),
  Mean = c(mean1, mean2, mean3),
  Median = c(med1, med2, med3))
summary_table


#2.25(c)
hist_data <- hist(log(food$foodaway), breaks=20, plot=FALSE)
hist_data$counts <- hist_data$counts / sum(hist_data$counts) * 100  # 轉換為百分比
plot(hist_data, col="tan",
     ylab="Percent", main = paste("Histogram of ln(Foodway)"))

summary(log(food$foodaway))
#There are 178 fewer values of ln(FOODAWAY) because 178 households reported spending
#$0 on food away from home per person, and ln(0) is undefined. It creates a “missing value”
#which software cannot use in the regression. 


#2.25(d)
food <- na.omit(food)
food <- food[food$foodaway > 0, ] #foodaway有0的話log會報錯
mod3 <- lm(log(foodaway) ~ income,data = food) #不要加1，不然會有差異
b1 <- coef(mod3)[[1]]
b2 <- coef(mod3)[[2]]
se_b1 <- summary(mod3)$coefficients[1,2] #先列後行 擷取SUMMARY的矩陣 
se_b2 <- summary(mod3)$coefficients[2,2]
sprintf("ln(Foodaway) = %.5f + %.5f Income", b1, b2)
sprintf("(se)           (%.4f)    (%.5f)", se_b1, se_b2)
summary(mod3)

#Ans : We estimate that each additional $100 household income increases food away expenditures
#per person of about 0.69%, other factors held constant. 


#2.25(e)
plot(food$income,log(food$foodaway),col = "darkblue",pch = 16)
abline(b1,b2,col = "darkred",lwd = 5 )
#Ans: The plot shows a positive association between ln(FOODAWAY) and INCOMEs. 

#2.25(f)

SSE3 = resid(mod3) #要畫的是殘插圖 並不是要算SSE，若sum 會只有一個數值
plot(food$income,SSE3,
     xlab = " Income",
     ylab = "OLS residuals",
     col = "darkgreen",
     pch = 16,
     main = "Residuals v.s Income")
#ANS : The OLS residuals do appear randomly distributed with no obvious patterns. There are fewer
#observations at higher incomes, so there is more “white space.”

#---------------------------------------------------------------------------------------------
  
#2.28(a)
edu <- POE5Rdata::cps5_small
head(edu)
hist(edu$wage,xlab = "wages",main = "Histogram of wage",col = "brown")
hist(edu$educ,xlab = "years of edu",main = "Histogram of edu",col = "red")
summary(edu$wage)
summary(edu$educ)
#Ans:The distribution of wage is long-tail(right-skewed) and there some outliers in  wage data, and the distribution of education is close to normal distribution

#2.28(b)

mod4 = lm(wage ~ educ,data = edu)
b1 <- coef(mod4)[[1]]
b2 <- coef(mod4)[[2]]
se_b1 <- summary(mod4)$coefficients[1,2] #先列後行 擷取SUMMARY的矩陣 
se_b2 <- summary(mod4)$coefficients[2,2]
sprintf("Wage = %.5f + %.5f EDUC", b1, b2)
sprintf("(se)   (%.4f)  (%.5f)", se_b1, se_b2)
summary(mod4)

#ANS:  We estimate that each additional 1 year of education increases hourly wage rate for 2.39676 units,other factors held constant. 


#2.28(c)

r3 = resid(mod4)
plot(edu$educ,r3,xlab = "EDUC",
     ylab = "Residuals",main = "Residuals vs. EDUC",
     col = "orange",pch = 16,)

#ANS: No clear systematic trend: The residuals appear randomly distributed along the EDUC axis. 

#If the assumptions SR1–SR5 (Standard Regression Assumptions) hold, we should not see any clear pattern in the residual plot. The residuals should be:
#Randomly scattered around zero.   No systematic trend (e.g., upward or downward movement). #No heteroscedasticity (variance of residuals should be roughly constant).



#2.28(d)
mod5 = lm(wage ~ female == 0,data = edu)
b1 <- coef(mod5)[[1]]
b2 <- coef(mod5)[[2]]
a <- sprintf("Wage = %.5f + %.5f Males", b1, b2)

mod6 = lm(wage ~ female == 1,data = edu)
b1 <- coef(mod6)[[1]]
b2 <- coef(mod6)[[2]]
b <- sprintf("Wage = %.5f + %.5f Females", b1, b2)

mod7 = lm(wage ~ black == 1,data = edu)
b1 <- coef(mod7)[[1]]
b2 <- coef(mod7)[[2]]
c <- sprintf("Wage = %.5f + %.5f Blacks", b1, b2)

mod8 = lm(wage ~ black == 0,data = edu)
b1 <- coef(mod8)[[1]]
b2 <- coef(mod8)[[2]]
d <- sprintf("Wage = %.5f + %.5f Whites", b1, b2)

summary_table <- data.frame(
  Group = c("Males","Females","Blacks","Whites"),
  Regressions = c(a,b,c,d))
summary_table

#The phenomenon where b1 exhibits opposite effects for males and females, as well as for Black and White individuals, occurs because these groups exhibit inverse relationships in the model. 

#2.28(e)

mod9 <- lm(wage ~ I(educ^2),data = edu)
mod9
b1 <- coef(mod9)[[1]]
b2 <- coef(mod9)[[2]]
se_b1 <- summary(mod9)$coefficients[1,2]
se_b2 <- summary(mod9)$coefficients[2,2]
sprintf("Wage = %.5f + %.5f EDUC^2", b1, b2)
sprintf("(se)    (%.4f)    (%.5f)", se_b1, se_b2)
summary(mod9)

b2 <- coef(mod9)[[2]]
dwagededuc <- 2*b2*educ
educ <-c(12,16)
a <- data.frame(EDUC = educ, Marginal_Effect = dwagededuc)
a

b2 <- coef(mod4)[[2]]
dwagededuc <- b2*educ
educ <-c(12,16)
a <- data.frame(EDUC = educ, Marginal_Effect = dwagededuc)
a
#Ans:The effect of education on wages is increasing (because EDUC² has a positive coefficient).
#This differs from a linear regression model.it would mean that for each additional year of education, wages would increase by a fixed amount of 3.87. However, in this quadratic regression model, since EDUC² is included, it indicates that as education increases, the rate of wage growth accelerates.
#compare: In a linear relationship, the wage rate grows faster in the early stages. In a quadratic relationship, the growth starts off slower but increases over time.

#2.28(f)

edu <- POE5Rdata::cps5_small
mod4 = lm(wage ~ educ,data = edu)
mod9 <- lm(wage ~ I(educ^2),data = edu)

b1 <- coef(mod4)[[1]]
b2 <- coef(mod4)[[2]]
b3 <- coef(mod9)[[1]]
b4 <- coef(mod9)[[2]]
plot(edu$educ,edu$wage,col = "darkblue",pch = 16,
     xlim = c(0,30),
     ylim = c(0,100),,
     main = "Comparison: Linear vs Quadratic",
     xlab = "Education Years", ylab = "Wage")
abline(b1,b2,col = "darkred",lwd = 5)
curve(b3+b4*x^2,col = "darkgreen" ,lwd = 6,add = TRUE)

summary(mod4)$r.squared
summary(mod9)$r.squared

#ANS:If EDUC only includes only 12 and 16, quadratic regression (the green line) would be a more stable choice because it's R square is bigger than Linear model's R square. 


#ANS:If EDUC only includes only 12 and 16, linear regression (the red line) would be a more stable choice because it can reasonably explain the trend between these two points without overfitting.
