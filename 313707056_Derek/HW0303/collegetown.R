library(tidyverse)
library(POE5Rdata)
library(lmtest)
library(nortest)
library(randtests)
library(car)
data("collegetown")
?collegetown
#plot(collegetown$sqft,collegetown$price)
model<-lm(collegetown$price~collegetown$sqft,data=collegetown)
#abline(model$coefficients[1],model$coefficients[2])
summary(model)
ggplot(data=collegetown,aes(x=sqft, y=price)) +
  geom_point(color="black")+
  geom_abline(slope = model$coefficients[2],intercept = model$coefficients[1],color="blue")


m2<-lm(price~I(sqft^2),data=collegetown)
summary(m2)
a1<-m2$coefficients[1]
a2<-m2$coefficients[2]
sqft_2000<-data.frame(sqft=c(20))
price_2000<-predict(m2,sqft_2000)
margin<-2*a2*20

ggplot(data=collegetown,aes(x=sqft, y=price)) +
  geom_point(color="black")+
  geom_smooth(method="lm", formula = y ~ I(x^2), color="blue",linewidth=1.5)+
  stat_function(fun = function(x) margin * (x - 20) + price_2000[1],
                color="red", linetype="dashed",linewidth=1)+
  geom_point(aes(x = 20, y = price_2000[1]), color = "green", size = 3) # 切線接觸點
 
elasticity<-(2*a2*20^2)/(a1+a2)



# 計算殘差
collegetown$residuals_linear <- residuals(model)
collegetown$residuals_quadratic <- residuals(m2)

# 繪製殘差圖
ggplot(collegetown, aes(x = sqft, y = residuals_linear)) +
  geom_point(color = "blue") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  labs(title = "線性回歸殘差圖", x = "房屋面積（百平方英尺）", y = "殘差") +
  theme_minimal()

ggplot(collegetown, aes(x = sqft, y = residuals_quadratic)) +
  geom_point(color = "purple") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  labs(title = "二次回歸殘差圖", x = "房屋面積（百平方英尺）", y = "殘差") +
  theme_minimal()

#假設檢定

#常態
shapiro.test(collegetown$residuals_linear)
shapiro.test(collegetown$residuals_quadratic)

#Homoscedasticity
ncvTest(model)
bptest(model)
ncvTest(m2)
bptest(m2)
#獨立性檢定
runs.test(collegetown$residuals_linear)
runs.test(collegetown$residuals_quadratic)
#p-value<0.05 違反獨立性假設 非隨機
SSE_linear<-sum(collegetown$residuals_linear^2)
SSE_quadratic<-sum(collegetown$residuals_quadratic^2)













