library(POE5Rdata)
library(ggplot2)
library(tseries)
library(t)
#a
data("cex5_small")
summary(cex5_small)
sd(cex5_small$income)
sd(cex5_small$food)
ggplot(data = cex5_small,aes(x=income))+
  geom_histogram(binwidth =5,fill="blue",color="black")+
  geom_vline(aes(xintercept = mean(income)), 
             color = "red", linetype = "dashed", size = 1) +
  geom_vline(aes(xintercept = median(income)), 
             color = "green", linetype = "dashed", size = 1)+
  annotate("text",x=150,y=60,label="Mean",color="red",size=5)+
  annotate("text",x=150,y=50,label="Median",color="green",size=5)
    
    
ggplot(data = cex5_small,aes(x=food))+
  geom_histogram(binwidth =10,fill="blue",color="black")+
  geom_vline(aes(xintercept = mean(income)), 
             color = "red", linetype = "dashed", size = 1) +  
  geom_vline(aes(xintercept = median(income)), 
             color = "green", linetype = "dashed", size = 1)+
  annotate("text",x=120,y=100,label="Mean",color="red",size=5)+
  annotate("text",x=120,y=90,label="Median",color="green",size=5)

jarque.bera.test(cex5_small$food)
jarque.bera.test(cex5_small$income)

#b
linear<-lm(food~income,data=cex5_small)
summary(linear)


coefficients_interval<-confint(linear,level = 0.95)

ggplot(cex5_small, aes(x = income, y = food)) + 
  geom_point() + 
  geom_smooth(formula = y~x,method = "lm", col = "blue",se=FALSE) +
  ggtitle("Linear Relationship between FOOD and INCOME")

ggplot(cex5_small, aes(x = income, y = linear$residuals)) + 
  geom_point() + 
  geom_smooth(formula = y~x,method = "lm", col = "blue",se=FALSE) +
  ggtitle("Residuals of Linear Model")
#c
cex5_small["residual"]<-linear$residuals

ggplot(data = cex5_small,aes(x=residual))+
  geom_histogram(binwidth =5,fill="blue",color="black")+
  ggtitle("Residual Histgram")

jarque.bera.test(cex5_small$residual)
#d
#elasity interval
elasticity_at_income <- function(income_value) {
  elasticity <- beta2 * (income_value/linear$coefficients[1]+linear$coefficients[2]*income_value )
  return(elasticity)
}
newx<-data.frame(income=c(19,65,160))
newx['yhat']<-predict(linear,newdata = newx)
elasticity_interval<-data.frame(lower=c(0,0,0),upper=c(0,0,0))

beta2_upper<-coefficients_interval[2,2]
beta2_lower<-coefficients_interval[2,1]

for(i in 1:3){
  elasticity_interval$lower[i]<-(newx$income[i]/newx$yhat[i])*beta2_lower
  elasticity_interval$upper[i]<-(newx$income[i]/newx$yhat[i])*beta2_upper
}
  


#e
log_log<-lm(log(food)~log(income),data=cex5_small)
summary(log_log)
ggplot(cex5_small, aes(x = log(income), y = log(food))) + 
  geom_point() + 
  geom_smooth(formula = y~x,method = "lm", col = "blue",se=FALSE) 
#g
ggplot(data=cex5_small,aes(x=log(income)))+
  geom_point(aes(y=log_log$residuals))

cex5_small["log_log_residual"]<-log_log$residuals

ggplot(data = cex5_small,aes(x=log_log_residual))+
  geom_histogram(binwidth =0.1,fill="blue",color="black")+
  ggtitle("Log-Log Residual Histgram")

jarque.bera.test(cex5_small$log_log_residual)
#h
ggplot(data=cex5_small,aes(x=log(income),y=food))+
  geom_point()+
  geom_smooth(method="lm",formula = y~x,se=FALSE)

linear_log<-lm(food~log(income),data = cex5_small)
summary(linear_log)
#i
linear_log_elasticity_interval<-data.frame(lower=c(0,0,0),upper=c(0,0,0))
newx2<-data.frame(income=c(19,65,160))
newx2['linear_log_yhat']<-predict(linear_log,newdata = newx2)
linear_log_coefficients_interval<-confint(linear_log,level = 0.95)
linear_log_beta2_upper<-linear_log_coefficients_interval[2,2]
linear_log_beta2_lower<-linear_log_coefficients_interval[2,1]
for(i in 1:3){
  linear_log_elasticity_interval$lower[i]<-linear_log_beta2_lower/newx2$linear_log_yhat[i]
  linear_log_elasticity_interval$upper[i]<-linear_log_beta2_upper/newx2$linear_log_yhat[i]
}
#h
ggplot(data=cex5_small,aes(x=log(income)))+
  geom_point(aes(y=linear_log$residuals))

cex5_small["linear_log_residual"]<-linear_log$residuals

ggplot(data = cex5_small,aes(x=linear_log_residual))+
  geom_histogram(binwidth =5,fill="blue",color="black")+
  ggtitle("Linear-Log Residual Histgram")

jarque.bera.test(cex5_small$linear_log_residual)



